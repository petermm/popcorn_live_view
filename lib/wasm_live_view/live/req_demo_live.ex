defmodule WasmLiveView.ReqDemoLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, current_route: :req_demo, url: "https://httpbin.org/get", result: nil, loading: false, error: nil)}
  end

  @impl true
  def handle_event("fetch", %{"url" => url}, socket) do
    lv = self()
    spawn(fn ->
      result =
        try do
          # URI.new!/1 uses :uri_string.parse (available in AtomVM) rather than
          # :re (unavailable). Passing a %URI{} struct to Req.get! makes
          # Req.merge/2 call URI.parse(%URI{}) which short-circuits without :re.
          uri = URI.new!(url)
          resp = Req.get!(uri, adapter: &WasmLiveView.WasmFetchAdapter.run/1, decode_body: false)
          {:ok, %{status: resp.status, body: resp.body}}
        catch
          _, reason -> {:error, inspect(reason)}
        end
      send(lv, {:fetch_result, result})
    end)
    {:noreply, assign(socket, loading: true, result: nil, error: nil)}
  end

  @impl true
  def handle_info({:fetch_result, {:ok, result}}, socket) do
    {:noreply, assign(socket, loading: false, result: result)}
  end

  def handle_info({:fetch_result, {:error, reason}}, socket) do
    {:noreply, assign(socket, loading: false, error: reason)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <.header>
      Req Demo
      <:subtitle>HTTP requests via Req with a browser fetch adapter — running in WASM.</:subtitle>
    </.header>

    <form phx-submit="fetch" class="flex gap-2 mt-6">
      <input type="text" name="url" value={@url} class="input input-bordered flex-1" />
      <.button type="submit" disabled={@loading}>
        {if @loading, do: "Fetching…", else: "Fetch"}
      </.button>
    </form>

    <div :if={@error} class="alert alert-error mt-4">
      {@error}
    </div>

    <div :if={@result} class="mt-6">
      <div class="badge badge-neutral mb-2">HTTP {@result.status}</div>
      <pre class="bg-base-200 rounded p-4 text-sm overflow-auto max-h-96">{@result.body}</pre>
    </div>
    """
  end
end
