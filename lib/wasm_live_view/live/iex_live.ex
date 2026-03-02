defmodule WasmLiveView.IexLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :terminal_app}

  import WasmLiveViewWeb.CoreComponents

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, current_route: :iex, shell_pid: nil)}
  end

  # JS hook fires this once ghostty-web is loaded and the terminal is open,
  # so the initial IEx prompt arrives after the terminal is ready to display it.
  def handle_event("terminal-ready", _params, socket) do
    socket =
      if socket.assigns.shell_pid == nil do
        {:ok, shell_pid} = WasmLiveView.IexShell.start_link(lv_pid: self())
        assign(socket, shell_pid: shell_pid)
      else
        socket
      end

    {:noreply, socket}
  end

  @impl true
  def handle_event("clear-history", _params, socket) do
    {:noreply, push_event(socket, "clear-history", %{})}
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
      phx-update="ignore"
      class="w-full rounded-lg overflow-hidden border border-base-300"
      style="height: 500px; background: #1e1e1e;"
    >
    </div>

    <div class="mt-2 flex justify-end">
      <button phx-click="clear-history" class="btn btn-xs btn-ghost text-base-content/50">
        Clear history
      </button>
    </div>

    <div class="mt-1 text-sm text-base-content/60 space-y-1">
      <p>
        Runs on <strong>AtomVM</strong> — a lightweight BEAM implementation.
        Most basic Elixir expressions work. HTTP via <code>Req</code> is
        available — <code>WasmFetchAdapter</code> is set as the default adapter
        at startup, so <code>Req.get!("https://…")</code> works directly.
      </p>
      <p>
        Arrow-up/down history is implemented in the JS hook (AtomVM's
        <code>edlin</code> doesn't handle escape sequences). Up to 200 entries,
        persisted in <code>localStorage</code>.
      </p>
    </div>
    """
  end
end
