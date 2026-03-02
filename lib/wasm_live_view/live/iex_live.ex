defmodule WasmLiveView.IexLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :terminal_app}

  import WasmLiveViewWeb.CoreComponents

  @impl true
  def mount(_params, _session, socket) do
    socket = assign(socket, current_route: :iex, shell_pid: nil)

    socket =
      if connected?(socket) do
        {:ok, shell_pid} = WasmLiveView.IexShell.start_link(lv_pid: self())
        assign(socket, shell_pid: shell_pid)
      else
        socket
      end

    {:ok, socket}
  end

  @impl true
  def handle_event("send-input", %{"data" => data}, socket) do
    if pid = socket.assigns.shell_pid do
      WasmLiveView.IexShell.send_input(pid, data)
    end

    {:noreply, socket}
  end

  @impl true
  def handle_info({:tty_data, data}, socket) do
    {:noreply, push_event(socket, "tty-data", %{data: Base.encode64(data)})}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <.header>
      IEX
      <:subtitle>
        Interactive Elixir shell running in WASM. Adapted from
        <a href="https://github.com/software-mansion/popcorn/tree/main/examples/iex_wasm" target="_blank" rel="noopener noreferrer" class="link link-primary">software-mansion/popcorn iex_wasm</a>.
      </:subtitle>
    </.header>

    <div
      id="iex-terminal"
      phx-hook="IexTerminal"
      class="w-full rounded-lg overflow-hidden border border-base-300"
      style="height: 500px; background: #1e1e1e;"
    >
    </div>
    """
  end
end
