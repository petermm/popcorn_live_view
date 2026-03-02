defmodule WasmLiveView.IexShell do
  @moduledoc """
  GenServer that manages an ExTTY IEx shell session for a LiveView process.

  Started per-session from IexLive.mount/3. Forwards terminal output
  to the LiveView process as {:tty_data, data} messages, which the
  LiveView then push_event's to the JS IexTerminal hook.
  """
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  def send_input(pid, text) do
    GenServer.cast(pid, {:send_input, text})
  end

  @impl true
  def init(opts) do
    lv_pid = Keyword.fetch!(opts, :lv_pid)
    type = Keyword.get(opts, :type, :elixir)
    tty_name = :"iex_tty_#{System.unique_integer([:positive])}"

    {:ok, _tty_pid} =
      ExTTY.start_link(
        handler: self(),
        shell_opts: [dot_iex_path: ""],
        name: tty_name,
        type: type
      )

    {:ok, %{lv_pid: lv_pid, tty_name: tty_name, type: type}}
  end

  @impl true
  def handle_cast({:send_input, text}, state) do
    ExTTY.send_text(state.tty_name, text)
    {:noreply, state}
  end

  @impl true
  def handle_info({:tty_data, data}, state) do
    send(state.lv_pid, {:tty_data, data})
    {:noreply, state}
  end
end
