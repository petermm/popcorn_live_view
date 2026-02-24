defmodule WasmLiveView.UserSocket do
  use Phoenix.Socket

  channel "lv:*", WasmLiveView.Channel

  @impl true
  def connect(_params, socket, _connect_info), do: {:ok, socket}

  @impl true
  def id(_socket), do: nil
end

defmodule WasmLiveView.ChannelStarter do
  @moduledoc false

  def start_child(_socket, _from, child_spec) do
    DynamicSupervisor.start_child(WasmLiveView.ChannelSupervisor, child_spec)
  end
end
