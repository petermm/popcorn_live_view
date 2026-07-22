defmodule WasmLiveView.MixProject do
  use Mix.Project

  def project do
    [
      app: :wasm_live_view,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps(),
      compilers: [:phoenix_live_view] ++ Mix.compilers()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :iex],
      mod: {WasmLiveView.Application, []}
    ]
  end

  defp deps do
    [
      {:easel, github: "petermm/easel", runtime: false},
      {:popcorn, "~> 0.3.2"},
      # {:popcorn, path: "../popcorn"},
      {:atomvm_packbeam,
       github: "petermm/atomvm_packbeam", branch: "atomvm-compat", runtime: false},
      # {:atomvm_packbeam, path: "../atomvm_packbeam", runtime: false},
      {:req, "~> 0.5.17", runtime: false},
      {:phoenix, "~> 1.8", runtime: false},
      {:phoenix_live_view,
       github: "SteffenDE/phoenix_live_view", branch: "sd-popcorn", override: true, runtime: false},
      {:phoenix_html, "~> 4.1", runtime: false},
      {:phoenix_ecto, "~> 4.6", runtime: false},
      {:ecto, "~> 3.12", runtime: false},
      {:plug, "~> 1.14", runtime: false},
      {:esbuild, "~> 0.10", runtime: false},
      {:tailwind, "~> 0.3", runtime: false},
      {:extty, "~> 0.2"},
      {:jason, "~> 1.0"},
      {:heroicons,
       github: "tailwindlabs/heroicons",
       tag: "v2.2.0",
       sparse: "optimized",
       app: false,
       compile: false,
       depth: 1}
    ]
  end

  defp aliases do
    [
      cook: &cook/1,
      "assets.setup": [
        "tailwind.install --if-missing",
        "esbuild.install --if-missing",
        "cmd npm install --prefix assets"
      ],
      "assets.build": ["compile", "tailwind wasm_live_view", "esbuild wasm_live_view"],
      "assets.deploy": [
        "tailwind wasm_live_view --minify",
        "esbuild wasm_live_view --minify",
        "phx.digest"
      ],
      setup: ["deps.get", "assets.setup", "assets.build", "cook"]
    ]
  end

  @stubs_dir "stubs"
  @stubs_out "_build/stubs"
  # Popcorn 0.3.x no longer ships AtomVM via include_vm.
  # JS glue comes from the npm package; AtomVM itself should be a *Release* build.
  # The npm @swmansion/popcorn package currently ships a Debug WASM (SAFE_HEAP +
  # asserts → abort("native code called abort()")), which breaks :re / run_js.
  @popcorn_npm_dist "assets/node_modules/@swmansion/popcorn/dist"
  @atomvm_release_dir "vendor/atomvm"
  @popcorn_js_files ~w(iframe.mjs popcorn.mjs bridge.mjs types.mjs errors.mjs index.mjs)
  @atomvm_files ~w(AtomVM.mjs AtomVM.wasm)
  @popcorn_runtime_files @atomvm_files ++ @popcorn_js_files

  defp cook(_) do
    Mix.Task.run("compile")
    Mix.Task.run("app.config")
    ensure_popcorn_npm!()
    Mix.Task.run("tailwind", ["wasm_live_view"])
    Mix.Task.run("esbuild", ["wasm_live_view"])

    # Popcorn already bundles Mix deps from _build/lib/*/ebin.
    # Add AtomVM-specific stubs plus OTP-only beams needed in the browser runtime.
    compile_stubs()
    stub_beams = Path.wildcard(Path.join([@stubs_out, "*.beam"]))
    syntax_tools_beams = otp_app_beams(:syntax_tools)

    Popcorn.cook(extra_beams: syntax_tools_beams ++ stub_beams)

    # Cook only writes bundle.avm; copy AtomVM + JS glue from @swmansion/popcorn.
    copy_popcorn_runtime!()
  end

  defp ensure_popcorn_npm! do
    unless File.dir?(@popcorn_npm_dist) do
      Mix.shell().info("Installing @swmansion/popcorn npm package...")
      0 = Mix.shell().cmd("npm install --prefix assets")
    end
  end

  defp copy_popcorn_runtime! do
    out_dir = Application.get_env(:popcorn, :out_dir) || "static/wasm"
    File.mkdir_p!(out_dir)

    for name <- @popcorn_js_files do
      src = Path.join(@popcorn_npm_dist, name)
      dest = Path.join(out_dir, name)

      unless File.exists?(src) do
        Mix.raise("""
        Missing Popcorn JS runtime file #{src}.
        Run `npm install --prefix assets` (or `mix assets.setup`) first.
        """)
      end

      File.cp!(src, dest)
    end

    for name <- @atomvm_files do
      dest = Path.join(out_dir, name)
      src = atomvm_source_path!(name)
      File.cp!(src, dest)
    end

    # AtomVM is built without wasmMemory/HEAP* exported. Emscripten then installs
    # aborting getters for those names. Export the real locals so run_js can
    # inspect linear memory (e.g. runtime-stats) without killing the VM.
    patch_atomvm_exports!(Path.join(out_dir, "AtomVM.mjs"))

    for name <- @popcorn_runtime_files do
      gzip_asset!(Path.join(out_dir, name))
    end

    Mix.shell().info("Copied AtomVM runtime to #{out_dir}/")
  end

  # Prefer a vendored Release build; fall back to npm only if present and not Debug.
  defp atomvm_source_path!(name) do
    vendor = Path.join(@atomvm_release_dir, name)
    npm = Path.join(@popcorn_npm_dist, name)

    cond do
      File.exists?(vendor) ->
        vendor

      File.exists?(npm) ->
        if name == "AtomVM.mjs" and atomvm_debug_build?(npm) do
          Mix.raise("""
          npm AtomVM.mjs looks like a Debug build (SAFE_HEAP/assertions).
          That aborts on FissionVM asserts during Popcorn.Wasm.run_js/tracked eval
          (e.g. the :re stub used by /regex-tester).

          Place a Release build at:
            #{@atomvm_release_dir}/AtomVM.mjs
            #{@atomvm_release_dir}/AtomVM.wasm

          Build with:
            mix popcorn.build_runtime --target wasm
          or popcorn's scripts/build-atomvm.sh release-wasm
          """)
        end

        npm

      true ->
        Mix.raise("""
        Missing AtomVM runtime file #{name}.
        Expected #{vendor} (preferred) or #{npm}.
        """)
    end
  end

  defp atomvm_debug_build?(mjs_path) do
    case File.read(mjs_path) do
      {:ok, src} -> String.contains?(src, "SAFE_HEAP") or String.contains?(src, "SAFE_HEAP_STORE")
      _ -> false
    end
  end

  defp patch_atomvm_exports!(path) do
    source = File.read!(path)

    # Idempotent: already patched.
    if String.contains?(source, "/* popcorn_live_view:export_wasm_memory */") do
      :ok
    else
      # Emscripten installs configurable aborting getters for unexported names.
      # Replace them with real data properties after HEAP* views are created.
      # Release builds minify updateMemoryViews to a single line; Debug builds keep newlines.
      export_tail =
        "/* popcorn_live_view:export_wasm_memory */" <>
          "var __exportMem=(k,v)=>Object.defineProperty(Module,k,{value:v,writable:true,configurable:true});" <>
          "__exportMem(\"wasmMemory\",wasmMemory);" <>
          "__exportMem(\"HEAP8\",HEAP8);" <>
          "__exportMem(\"HEAPU8\",HEAPU8);" <>
          "__exportMem(\"HEAP16\",HEAP16);" <>
          "__exportMem(\"HEAP32\",HEAP32);" <>
          "__exportMem(\"HEAPU32\",HEAPU32);" <>
          "__exportMem(\"HEAPF32\",HEAPF32);" <>
          "__exportMem(\"HEAPF64\",HEAPF64);"

      markers = [
        # Minified release (FissionVM / vendor)
        {"function updateMemoryViews(){var b=wasmMemory.buffer;HEAP8=new Int8Array(b);HEAP16=new Int16Array(b);HEAPU8=new Uint8Array(b);HEAPU16=new Uint16Array(b);HEAP32=new Int32Array(b);HEAPU32=new Uint32Array(b);HEAPF32=new Float32Array(b);HEAPF64=new Float64Array(b);HEAP64=new BigInt64Array(b);HEAPU64=new BigUint64Array(b)}",
         "function updateMemoryViews(){var b=wasmMemory.buffer;HEAP8=new Int8Array(b);HEAP16=new Int16Array(b);HEAPU8=new Uint8Array(b);HEAPU16=new Uint16Array(b);HEAP32=new Int32Array(b);HEAPU32=new Uint32Array(b);HEAPF32=new Float32Array(b);HEAPF64=new Float64Array(b);HEAP64=new BigInt64Array(b);HEAPU64=new BigUint64Array(b);" <>
           export_tail <> "}"},
        # Pretty debug (npm Debug build)
        {"function updateMemoryViews() {\n  var b = wasmMemory.buffer;\n  HEAP8 = new Int8Array(b);\n  HEAP16 = new Int16Array(b);\n  HEAPU8 = new Uint8Array(b);\n  HEAP32 = new Int32Array(b);\n  HEAPU32 = new Uint32Array(b);\n  HEAPF32 = new Float32Array(b);\n  HEAPF64 = new Float64Array(b);\n  HEAP64 = new BigInt64Array(b);\n  new BigUint64Array(b);\n}",
         "function updateMemoryViews() {\n  var b = wasmMemory.buffer;\n  HEAP8 = new Int8Array(b);\n  HEAP16 = new Int16Array(b);\n  HEAPU8 = new Uint8Array(b);\n  HEAP32 = new Int32Array(b);\n  HEAPU32 = new Uint32Array(b);\n  HEAPF32 = new Float32Array(b);\n  HEAPF64 = new Float64Array(b);\n  HEAP64 = new BigInt64Array(b);\n  new BigUint64Array(b);\n  " <>
           export_tail <> "\n}"}
      ]

      case Enum.find(markers, fn {marker, _} -> String.contains?(source, marker) end) do
        {marker, replacement} ->
          File.write!(path, String.replace(source, marker, replacement, global: false))

        nil ->
          Mix.shell().info(
            "warning: could not patch AtomVM.mjs memory exports (marker not found); " <>
              "runtime-stats will skip wasm memory probes"
          )
      end
    end
  end

  defp compile_stubs do
    File.mkdir_p!(@stubs_out)

    ex_files = Path.wildcard(Path.join([@stubs_dir, "*.ex"]))
    erl_files = Path.wildcard(Path.join([@stubs_dir, "*.erl"]))

    # Compile Elixir stubs
    ex_modules =
      for file <- ex_files, reduce: [] do
        acc ->
          modules = Code.compile_file(file)

          for {mod, binary} <- modules do
            beam_path = Path.join(@stubs_out, "#{mod}.beam")
            File.write!(beam_path, binary)
          end

          acc ++ modules
      end

    # Compile Erlang stubs (.erl → .beam via :compile)
    erl_modules =
      for file <- erl_files, reduce: [] do
        acc ->
          case :compile.file(to_charlist(file), [:binary, :return_errors]) do
            {:ok, mod, binary} ->
              beam_path = Path.join(@stubs_out, "#{mod}.beam")
              File.write!(beam_path, binary)
              acc ++ [{mod, binary}]

            {:error, errors, _warnings} ->
              Mix.raise("Failed to compile #{file}: #{inspect(errors)}")
          end
      end

    ex_modules ++ erl_modules
  end

  defp otp_app_beams(app) do
    app_pattern =
      Path.join([
        :code.root_dir() |> to_string(),
        "lib",
        "#{app}-*",
        "ebin",
        "*.beam"
      ])

    case Path.wildcard(app_pattern) do
      [] -> Mix.raise("Could not find OTP beams for #{app}")
      beams -> beams
    end
  end

  defp gzip_asset!(path) do
    path
    |> File.read!()
    |> :zlib.gzip()
    |> then(&File.write!("#{path}.gz", &1))
  end
end
