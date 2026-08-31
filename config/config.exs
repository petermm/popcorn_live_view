import Config

config :phoenix, :json_library, Jason

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
