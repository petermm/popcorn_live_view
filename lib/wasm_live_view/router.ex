defmodule WasmLiveView.Router do
  use Phoenix.Router
  import Phoenix.LiveView.Router

  scope "/" do
    live "/", WasmLiveView.NotesLive, :index
    live "/counter", WasmLiveView.CounterLive, :index
    live "/streams", WasmLiveView.StreamsLive, :index

  end
end
