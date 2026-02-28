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
      extra_applications: [:logger],
      mod: {WasmLiveView.Application, []}
    ]
  end

  defp deps do
    [
      {:popcorn, github: "SteffenDE/popcorn", branch: "sd-lv"},
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
      "assets.setup": ["tailwind.install --if-missing", "esbuild.install --if-missing"],
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

  defp cook(_) do
    Mix.Task.run("compile")
    Mix.Task.run("app.config")
    Mix.Task.run("tailwind", ["wasm_live_view"])
    Mix.Task.run("esbuild", ["wasm_live_view"])

    # 1. Compile stub modules and write their .beam files
    stub_modules = compile_stubs()
    stub_module_set = MapSet.new(stub_modules, fn {mod, _} -> "#{mod}" end)

    # 2. Collect beams from runtime: false deps, excluding those overridden by stubs
    dep_beams =
      ~w[phoenix phoenix_live_view phoenix_html phoenix_template phoenix_ecto ecto plug req mime atomvm_packbeam]
      |> Enum.flat_map(fn dep ->
        Path.wildcard(Path.join([Mix.Project.build_path(), "lib", dep, "ebin", "*.beam"]))
      end)
      |> Enum.reject(fn beam_path ->
        module_name = Path.basename(beam_path, ".beam")
        MapSet.member?(stub_module_set, module_name)
      end)

    # 3. Add stub beams
    stub_beams = Path.wildcard(Path.join([@stubs_out, "*.beam"]))

    Popcorn.cook(extra_beams: dep_beams ++ stub_beams)
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
end
