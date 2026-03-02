defmodule WasmLiveView.IexShell do
  @moduledoc """
  GenServer that manages an ExTTY IEx shell session for a LiveView process.

  Registered under the fixed name :iex_shell so it survives LiveView
  navigation. When the LiveView remounts, it calls reconnect/1 to attach
  the new LiveView pid so tty output is forwarded correctly.
  """
  use GenServer

  def start(opts) do
    GenServer.start(__MODULE__, opts, name: :iex_shell)
  end

  def send_input(pid, text) do
    GenServer.cast(pid, {:send_input, text})
  end

  def reconnect(lv_pid) do
    GenServer.call(:iex_shell, {:set_lv_pid, lv_pid})
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
  def handle_call({:set_lv_pid, lv_pid}, _from, state) do
    {:reply, :ok, %{state | lv_pid: lv_pid}}
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
