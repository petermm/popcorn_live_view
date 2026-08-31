defmodule WasmLiveView.TransportProcess do
  @moduledoc """
  Bridge between Popcorn's OTP/BEAM wasm genserver proxy and Phoenix.LiveView.Channel.

  Acts as the transport_pid directly — bypasses Phoenix.Socket entirely.
  Builds a %Phoenix.Socket{} struct manually, calls Channel.Server.join,
  and sends %Phoenix.Socket.Message{} structs to the channel pid.
  """
  use GenServer

  @process_name :main
  @serializer Phoenix.Socket.V2.JSONSerializer

  def start_link(_args) do
    GenServer.start_link(__MODULE__, [], name: @process_name)
  end

  @impl true
  def init([]) do
    {:ok, %{channel_pid: nil, join_ref: nil, topic: nil}}
  end

  @impl true
  def handle_call(%{"type" => "transport_connect"}, _from, state) do
    {:reply, "ok", state}
  end

  def handle_call(request, _from, state) do
    IO.puts("[TransportProcess] Unhandled call: #{inspect(request)}")
    {:reply, {:error, :unhandled}, state}
  end

  @impl true
  def handle_cast(%{"type" => "channel_msg", "payload" => json}, state) do
    {:noreply, handle_channel_msg(json, state)}
  end

  def handle_cast(%{"type" => "transport_close"}, state) do
    if state.channel_pid do
      send(state.channel_pid, %Phoenix.Socket.Message{
        topic: state.topic,
        event: "phx_leave",
        ref: nil,
        join_ref: state.join_ref
      })
    end

    {:noreply, state}
  end

  def handle_cast(msg, state) do
    IO.puts("[TransportProcess] Unhandled cast: #{inspect(msg)}")
    {:noreply, state}
  end

  @impl true
  def handle_info({:socket_push, opcode, payload}, state) do
    push_to_js(opcode, payload)
    {:noreply, state}
  end

  def handle_info({:socket_close, pid, _reason}, %{channel_pid: pid} = state) do
    {:noreply, %{state | channel_pid: nil}}
  end

  def handle_info({:socket_close, _pid, _reason}, state) do
    {:noreply, state}
  end

  def handle_info({:DOWN, _ref, _, pid, _reason}, %{channel_pid: pid} = state) do
    {:noreply, %{state | channel_pid: nil}}
  end

  def handle_info(msg, state) do
    IO.puts("[TransportProcess] Unhandled message: #{inspect(msg)}")
    {:noreply, state}
  end

  defp handle_channel_msg(json, state) do
    message = @serializer.decode!(json, opcode: :text)

    case {message.event, state.channel_pid} do
      {"phx_join", _} ->
        handle_join(message, state)

      {_, nil} ->
        handle_join(message, state)

      {_, pid} ->
        send(pid, message)
        state
    end
  end

  defp handle_join(message, state) do
    socket = %Phoenix.Socket{
      transport_pid: self(),
      serializer: @serializer,
      endpoint: WasmLiveView.Endpoint,
      pubsub_server: WasmLiveView.PubSub,
      handler: WasmLiveView.UserSocket,
      private: %{connect_info: %{session: %{}}}
    }

    opts = [starter: &WasmLiveView.ChannelStarter.start_child/3]

    case Phoenix.Channel.Server.join(socket, WasmLiveView.Channel, message, opts) do
      {:ok, reply, pid} ->
        encoded =
          @serializer.encode!(%Phoenix.Socket.Reply{
            join_ref: message.join_ref,
            ref: message.ref,
            topic: message.topic,
            status: :ok,
            payload: reply
          })

        push_to_js_encoded(encoded)
        %{state | channel_pid: pid, join_ref: message.join_ref, topic: message.topic}

      {:error, reply} ->
        encoded =
          @serializer.encode!(%Phoenix.Socket.Reply{
            join_ref: message.join_ref,
            ref: message.ref,
            topic: message.topic,
            status: :error,
            payload: reply
          })

        push_to_js_encoded(encoded)
        state
    end
  end

  defp push_to_js_encoded({:socket_push, opcode, payload}) do
    push_to_js(opcode, payload)
  end

  defp push_to_js(_opcode, payload) do
    data =
      case payload do
        list when is_list(list) -> IO.iodata_to_binary(list)
        binary when is_binary(binary) -> binary
        other -> inspect(other)
      end

    Popcorn.Wasm.send(%{type: "channel_msg", payload: data})
  end
end
