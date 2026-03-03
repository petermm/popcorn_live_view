defmodule WasmLiveView.Router do
  use Phoenix.Router
  import Phoenix.LiveView.Router

  scope "/" do
    live("/", WasmLiveView.WokwiLive, :index)
    live("/notes", WasmLiveView.NotesLive, :index)
    live("/counter", WasmLiveView.CounterLive, :index)
    live("/streams", WasmLiveView.StreamsLive, :index)
    live("/eval", WasmLiveView.EvalLive, :index)
    live("/interop", WasmLiveView.InteropDemoLive, :index)
    live("/notes-persisted", WasmLiveView.NotesPersistedLive, :index)
    live("/notes-sqlite", WasmLiveView.NotesSqliteLive, :index)
    live("/req-demo", WasmLiveView.ReqDemoLive, :index)
    live("/packbeam", WasmLiveView.PackbeamLive, :index)
    live("/debug-pack", WasmLiveView.DebugPackLive, :index)
    live("/weather", WasmLiveView.WeatherLive, :index)
    live("/solar-forecast", WasmLiveView.SolarForecastLive, :index)
    live("/regex-tester", WasmLiveView.RegexTesterLive, :index)
    live("/runtime-stats", WasmLiveView.RuntimeStatsLive, :index)
    live("/iex", WasmLiveView.IexLive, :index)
    live("/wokwi", WasmLiveView.WokwiLive, :index)
    live("/wokwi-vertical", WasmLiveView.WokwiVerticalLive, :index)
  end
end
