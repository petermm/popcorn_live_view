defmodule WasmLiveView.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      WasmLiveView.Notes,
      {DynamicSupervisor, name: WasmLiveView.ChannelSupervisor, strategy: :one_for_one},
      WasmLiveView.EvalInWasm,
      WasmLiveView.TransportProcess
    ]

    opts = [strategy: :one_for_one, name: WasmLiveView.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
