import Config

config :popcorn,
  out_dir: "static/wasm",
  add_tracing: false

config :phoenix, :json_library, Jason

config :esbuild,
  version: "0.25.0",
  wasm_live_view: [
    args: ~w(
      js/app.js
      --bundle
      --format=esm
      --outdir=../static/assets
      --external:/wasm/*
    ),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

# Configure tailwind (the version is required)
config :tailwind,
  version: "4.1.12",
  wasm_live_view: [
    args: ~w(
      --input=assets/css/app.css
      --output=static/assets/css/app.css
    ),
    cd: Path.expand("..", __DIR__)
  ]
