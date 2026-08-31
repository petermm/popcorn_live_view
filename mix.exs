defmodule WasmLiveView.MixProject do
  use Mix.Project

  def project do
    [
      app: :wasm_live_view,
      version: "0.1.0",
      elixir: "~> 1.19",
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
      {:easel, github: "petermm/easel", runtime: false},
      {:popcorn, path: "vendor/popcorn/elixir"},
      {:atomvm_packbeam,
       github: "petermm/atomvm_packbeam", branch: "atomvm-compat", runtime: false},
      {:req, "~> 0.5.17", runtime: false},
      {:phoenix, "~> 1.8", runtime: false},
      {:phoenix_live_view,
       github: "petermm/phoenix_live_view", branch: "popcorn", override: true, runtime: false},
      {:phoenix_html, "~> 4.1", runtime: false},
      {:phoenix_ecto, "~> 4.6", runtime: false},
      {:ecto, "~> 3.12", runtime: false},
      {:plug, "~> 1.14", runtime: false},
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
        "cmd npm install --prefix assets"
      ],
      "assets.build": ["cook"],
      "assets.deploy": ["cook"],
      setup: ["deps.get", "assets.setup", "cook"]
    ]
  end

  defp cook(_) do
    Mix.Task.run("compile")
    Mix.Task.run("app.config")
    _ = Mix.Task.run("tailwind", ["wasm_live_view"])
    ensure_popcorn_npm!()

    0 =
      Mix.shell().cmd("node build.mjs",
        cd: Path.expand("assets", __DIR__),
        env: [
          {"NODE_PATH", Path.expand("deps", __DIR__) <> ":" <> Mix.Project.build_path()}
        ]
      )
  end

  defp ensure_popcorn_npm! do
    popcorn_js = Path.expand("vendor/popcorn/js", __DIR__)
    pkg = Path.join(["assets", "node_modules", "@swmansion", "popcorn"])

    unless File.dir?(Path.join(pkg, "dist")) do
      Mix.shell().info("Installing @swmansion/popcorn from #{popcorn_js}...")
      0 = Mix.shell().cmd("npm install --prefix assets")
    end
  end
end
