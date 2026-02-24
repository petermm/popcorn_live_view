defmodule WasmLiveView.Channel do
  defdelegate init(args), to: Phoenix.LiveView.Channel

  def child_spec(args) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [args]},
      restart: :temporary
    }
  end

  def start_link({_endpoint, from}) do
    GenServer.start_link(__MODULE__, from, [])
  end

  def handle_info({Phoenix.Channel, auth_payload, from, phx_socket}, ref) do
    Process.demonitor(ref)
    mount(auth_payload, from, phx_socket)
  end

  def handle_info(message, state) do
    Phoenix.LiveView.Channel.handle_info(message, state)
  end

  defdelegate handle_cast(message, state), to: Phoenix.LiveView.Channel
  defdelegate handle_call(message, from, state), to: Phoenix.LiveView.Channel
  defdelegate terminate(reason, state), to: Phoenix.LiveView.Channel
  defdelegate code_change(old_vsn, state, extra), to: Phoenix.LiveView.Channel

  defp extract_path_from_url(url) when is_binary(url) do
    case String.split(url, "//", parts: 2) do
      [_scheme, rest] ->
        case String.split(rest, "/", parts: 2) do
          [_host_port, path] -> "/" <> path
          [_host_only] -> "/"
        end

      _ ->
        url
    end
  end

  defp mount(%{"session" => raw_session} = params, from, phx_socket) do
    %Phoenix.Socket{private: %{connect_info: connect_info}} = phx_socket

    session_data = if is_binary(raw_session), do: Jason.decode!(raw_session), else: raw_session
    id = session_data["id"]

    path =
      case params do
        %{"redirect" => redirect_url} -> extract_path_from_url(redirect_url)
        _ -> session_data["path"] || "/"
      end

    path_segments = String.split(path, "/", trim: true)

    case Phoenix.Router.route_info(WasmLiveView.Router, "GET", path_segments, "localhost") do
      %{phoenix_live_view: {view, action, opts, live_session}} ->
        verified = %Phoenix.LiveView.Session{
          id: id,
          view: view,
          root_view: view,
          router: WasmLiveView.Router,
          session: %{},
          live_session_name: live_session[:name]
        }

        IO.inspect({verified})

        case load_live_view(view) do
          {:ok, config} ->
            Process.put(:"$process_label", {Phoenix.LiveView, view, phx_socket.topic})
            Process.put(:"$phx_transport_pid", phx_socket.transport_pid)

            uri = %URI{scheme: "http", host: "localhost", path: path, port: 80}

            route = %Phoenix.LiveView.Route{
              view: view,
              action: action,
              opts: opts,
              live_session: live_session,
              params: %{},
              uri: uri
            }

            Phoenix.LiveView.Channel.verified_mount(
              verified,
              config,
              route,
              "http://localhost#{path}",
              params,
              from,
              phx_socket,
              connect_info
            )

          {:error, _} ->
            GenServer.reply(from, {:error, %{reason: "stale"}})
            {:stop, :shutdown, :no_state}
        end

      _ ->
        IO.puts("[WasmLiveView.Channel] No route for path: #{path}")
        GenServer.reply(from, {:error, %{reason: "no_route"}})
        {:stop, :shutdown, :no_state}
    end
  end

  defp load_live_view(view) do
    # Make sure the view is loaded. Otherwise if the first request
    # ever is a LiveView connection, the view won't be loaded and
    # the mount/handle_params callbacks won't be invoked as they
    # are optional, leading to errors.
    {:ok, view.__live__()}
  rescue
    # If it fails, then the only possible answer is that the live
    # view has been renamed. So we force the client to reconnect.
    _ -> {:error, :stale}
  end
end
