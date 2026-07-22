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
  # Popcorn 0.3.x no longer ships AtomVM via include_vm; runtime comes from the npm package.
  @popcorn_npm_dist "assets/node_modules/@swmansion/popcorn/dist"
  @popcorn_runtime_files ~w(AtomVM.mjs AtomVM.wasm iframe.mjs popcorn.mjs bridge.mjs types.mjs errors.mjs index.mjs)

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

    for name <- @popcorn_runtime_files do
      src = Path.join(@popcorn_npm_dist, name)
      dest = Path.join(out_dir, name)

      unless File.exists?(src) do
        Mix.raise("""
        Missing Popcorn runtime file #{src}.
        Run `npm install --prefix assets` (or `mix assets.setup`) first.
        """)
      end

      File.cp!(src, dest)
      gzip_asset!(dest)
    end

    Mix.shell().info("Copied AtomVM runtime to #{out_dir}/")
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
