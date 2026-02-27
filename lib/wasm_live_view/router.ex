defmodule WasmLiveView.Router do
  use Phoenix.Router
  import Phoenix.LiveView.Router

  scope "/" do
    live "/", WasmLiveView.NotesLive, :index
    live "/counter", WasmLiveView.CounterLive, :index
    live "/streams", WasmLiveView.StreamsLive, :index
    live "/eval", WasmLiveView.EvalLive, :index
    live "/interop", WasmLiveView.InteropDemoLive, :index
    live "/notes-persisted", WasmLiveView.NotesPersistedLive, :index
    live "/notes-sqlite", WasmLiveView.NotesSqliteLive, :index
    live "/req-demo", WasmLiveView.ReqDemoLive, :index
    live "/packbeam", WasmLiveView.PackbeamLive, :index
    live "/debug-pack", WasmLiveView.DebugPackLive, :index
    live "/weather", WasmLiveView.WeatherLive, :index
  end
end
