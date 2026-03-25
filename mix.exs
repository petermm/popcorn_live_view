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
      {:popcorn, "~> 0.2.2"},
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

    # Popcorn 0.2.x already bundles Mix deps from _build/lib/*/ebin.
    # Add AtomVM-specific stubs plus OTP-only beams needed in the browser runtime.
    compile_stubs()
    stub_beams = Path.wildcard(Path.join([@stubs_out, "*.beam"]))
    syntax_tools_beams = otp_app_beams(:syntax_tools)

    Popcorn.cook(
      extra_beams: syntax_tools_beams ++ stub_beams,
      include_vm: true
    )

    patch_popcorn_iframe_casts!()
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

  defp patch_popcorn_iframe_casts! do
    path = Path.join(["static", "wasm", "popcorn_iframe.js"])
    source = File.read!(path)

    source =
      source
      |> patch_iframe_cast_listener()
      |> patch_iframe_cast_handler()

    File.write!(path, source)
    gzip_asset!(path)
  end

  defp patch_iframe_cast_listener(source) do
    old = """
        if (type === MESSAGES.CALL) {
          await handleCall(data);
        } else if (type.startsWith("popcorn")) {
    """

    new = """
        if (type === MESSAGES.CALL) {
          await handleCall(data);
        } else if (type === MESSAGES.CAST) {
          handleCast(data.value);
        } else if (type.startsWith("popcorn")) {
    """

    cond do
      String.contains?(source, "else if (type === MESSAGES.CAST)") -> source
      String.contains?(source, old) -> String.replace(source, old, new, global: false)
      true -> raise "Expected Popcorn iframe listener pattern not found in #{path_for_error()}"
    end
  end

  defp patch_iframe_cast_handler(source) do
    old = """
    function ensureFunctionEval(maybeFunction) {
    """

    new = """
    function handleCast(request) {
      const { process, args } = request;
      Module.cast(process, args);
    }

    function ensureFunctionEval(maybeFunction) {
    """

    cond do
      String.contains?(source, "function handleCast(request)") -> source
      String.contains?(source, old) -> String.replace(source, old, new, global: false)
      true -> raise "Expected Popcorn handleCast insertion point not found in #{path_for_error()}"
    end
  end

  defp path_for_error, do: Path.join(["static", "wasm", "popcorn_iframe.js"])

  defp gzip_asset!(path) do
    path
    |> File.read!()
    |> :zlib.gzip()
    |> then(&File.write!("#{path}.gz", &1))
  end
end
