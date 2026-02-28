defmodule WasmLiveView.ReqDemoLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents

  @demos [
    %{
      id: :uuid,
      label: "UUID",
      desc: "Generate a random UUID",
      url: "https://httpbin.org/uuid",
      method: :get,
      body: nil
    },
    %{
      id: :json,
      label: "Sample JSON",
      desc: "Fetch a canned JSON object",
      url: "https://httpbin.org/json",
      method: :get,
      body: nil
    },
    %{
      id: :headers,
      label: "Headers",
      desc: "Echo back the request headers as seen by the server",
      url: "https://httpbin.org/headers",
      method: :get,
      body: nil
    },
    %{
      id: :ip,
      label: "My IP",
      desc: "Return the caller's public IP address",
      url: "https://httpbin.org/ip",
      method: :get,
      body: nil
    },
    %{
      id: :teapot,
      label: "418 Teapot",
      desc: "HTTP 418 — I'm a teapot",
      url: "https://httpbin.org/status/418",
      method: :get,
      body: nil
    },
    %{
      id: :post,
      label: "POST JSON",
      desc: "POST a JSON body and see it echoed back by httpbin",
      url: "https://httpbin.org/post",
      method: :post,
      body: ~s({"hello": "from AtomVM", "runtime": "WASM", "library": "Req"})
    },
    %{
      id: :custom,
      label: "Custom URL",
      desc: "Enter your own GET endpoint",
      url: nil,
      method: :get,
      body: nil
    }
  ]

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       current_route: :req_demo,
       demos: @demos,
       active_demo: hd(@demos),
       result: nil,
       loading: false,
       error: nil
     )}
  end

  @impl true
  def handle_event("select_demo", %{"id" => id}, socket) do
    demo = Enum.find(@demos, fn d -> to_string(d.id) == id end)
    {:noreply, assign(socket, active_demo: demo, result: nil, error: nil)}
  end

  @impl true
  def handle_event("fetch", params, socket) do
    demo = socket.assigns.active_demo

    url =
      if demo.id == :custom,
        do: Map.get(params, "url", "https://httpbin.org/get"),
        else: demo.url

    method = demo.method
    body = demo.body
    lv = self()
    t0 = :erlang.monotonic_time(:millisecond)

    spawn(fn ->
      result =
        try do
          # URI.new!/1 uses :uri_string.parse (available in AtomVM) rather than
          # :re (unavailable in WASM). Passing a %URI{} to Req.get!/post! makes
          # Req call URI.parse(%URI{}) which is a no-op, bypassing regex entirely.
          uri = URI.new!(url)

          req_opts = [adapter: &WasmLiveView.WasmFetchAdapter.run/1, decode_body: false]

          req_opts =
            if body,
              do: req_opts ++ [body: body, headers: [{"content-type", "application/json"}]],
              else: req_opts

          resp =
            case method do
              :post -> Req.post!(uri, req_opts)
              _ -> Req.get!(uri, req_opts)
            end

          elapsed = :erlang.monotonic_time(:millisecond) - t0

          # Try JSON-decoding so we can tag the response as JSON.
          parsed =
            try do
              {:ok, Jason.decode!(resp.body)}
            catch
              _, _ -> :raw
            end

          {:ok,
           %{
             status: resp.status,
             headers: resp.headers,
             body: resp.body,
             parsed: parsed,
             elapsed_ms: elapsed
           }}
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

  defp status_class(s) when s >= 200 and s < 300, do: "badge-success"
  defp status_class(s) when s >= 300 and s < 400, do: "badge-warning"
  defp status_class(_), do: "badge-error"

  @impl true
  def render(assigns) do
    ~H"""
    <.header>
      Req HTTP Demo
      <:subtitle>
        Live HTTP requests via <strong>Req</strong> with a browser fetch adapter — running in AtomVM WASM.
      </:subtitle>
    </.header>

    <%!-- Demo selector --%>
    <div class="flex flex-wrap gap-2 mt-6">
      <button
        :for={demo <- @demos}
        phx-click="select_demo"
        phx-value-id={demo.id}
        class={"btn btn-sm #{if @active_demo.id == demo.id, do: "btn-primary", else: "btn-ghost border border-base-300"}"}
      >
        {demo.label}
      </button>
    </div>

    <%!-- Request card --%>
    <div class="mt-4 rounded-xl border border-base-300 bg-base-100 p-4 space-y-3">
      <p class="text-sm text-base-content/70">{@active_demo.desc}</p>

      <%!-- URL row for preset demos --%>
      <div
        :if={@active_demo.id != :custom}
        class="flex items-center gap-2 bg-base-200 rounded-lg px-3 py-2 font-mono text-xs"
      >
        <span class={"badge badge-xs #{if @active_demo.method == :post, do: "badge-warning", else: "badge-info"}"}>
          {String.upcase(to_string(@active_demo.method))}
        </span>
        <span class="break-all">{@active_demo.url}</span>
      </div>

      <%!-- POST body preview --%>
      <div :if={@active_demo.body}>
        <div class="text-xs text-base-content/50 mb-1">Request body</div>
        <pre class="bg-base-200 rounded-lg px-3 py-2 text-xs font-mono">{@active_demo.body}</pre>
      </div>

      <%!-- Custom URL form --%>
      <form :if={@active_demo.id == :custom} phx-submit="fetch" class="flex gap-2">
        <div class="flex items-center gap-2 flex-1 bg-base-200 rounded-lg px-3 py-1">
          <span class="badge badge-info badge-xs shrink-0">GET</span>
          <input
            type="text"
            name="url"
            value="https://httpbin.org/get"
            class="bg-transparent flex-1 text-xs font-mono outline-none"
            placeholder="https://..."
          />
        </div>
        <.button type="submit" disabled={@loading}>
          {if @loading, do: "Fetching…", else: "Fetch"}
        </.button>
      </form>

      <%!-- Fetch button for preset demos --%>
      <div :if={@active_demo.id != :custom}>
        <.button phx-click="fetch" disabled={@loading}>
          {if @loading, do: "Fetching…", else: "Fetch"}
        </.button>
      </div>
    </div>

    <%!-- AtomVM / URI.new! note --%>
    <div class="mt-4 rounded-lg bg-base-200 border border-base-300 px-4 py-3 text-xs text-base-content/70 space-y-1">
      <p class="font-semibold text-base-content/80">AtomVM constraint — no <code class="font-mono">:re</code> module</p>
      <p>
        Req normally calls <code class="font-mono">URI.parse/1</code> on a URL string,
        which uses <code class="font-mono">:re</code> internally — unavailable in WASM.
        Instead we build a <code class="font-mono">%URI{}</code> struct via
        <code class="font-mono">URI.new!/1</code> (backed by <code class="font-mono">:uri_string.parse/1</code>)
        and pass the struct to Req. Req then calls <code class="font-mono">URI.parse(%URI{})</code>,
        which is a no-op, and the regex path is never hit.
      </p>
    </div>

    <%!-- Error --%>
    <div :if={@error} class="alert alert-error mt-4 text-sm">
      {@error}
    </div>

    <%!-- Result --%>
    <div :if={@result} class="mt-6 space-y-4">
      <%!-- Status bar --%>
      <div class="flex items-center gap-3 bg-base-200 rounded-lg px-4 py-3">
        <span class={"badge badge-lg font-mono #{status_class(@result.status)}"}>
          {@result.status}
        </span>
        <span class="text-sm text-base-content/60">{@result.elapsed_ms} ms</span>
        <div class="ml-auto flex items-center gap-2">
          <span
            :if={match?({:ok, _}, @result.parsed)}
            class="badge badge-outline badge-sm font-mono"
          >
            JSON
          </span>
          <span class="text-xs text-base-content/40">{byte_size(@result.body)} B</span>
        </div>
      </div>

      <%!-- Response headers (collapsible) --%>
      <details class="collapse collapse-arrow bg-base-200 rounded-lg">
        <summary class="collapse-title text-sm font-medium py-3 min-h-0">
          Response Headers
          <span class="text-base-content/50 font-normal">({map_size(@result.headers)})</span>
        </summary>
        <div class="collapse-content pt-0">
          <table class="table table-xs font-mono w-full">
            <tbody>
              <tr :for={{k, v} <- Enum.sort(@result.headers)}>
                <td class="text-base-content/60 whitespace-nowrap pr-4 align-top w-48">{k}</td>
                <td class="break-all text-xs">{if is_list(v), do: Enum.join(v, ", "), else: v}</td>
              </tr>
            </tbody>
          </table>
        </div>
      </details>

      <%!-- Response body --%>
      <div>
        <div class="text-xs text-base-content/50 mb-2">Response Body</div>
        <pre class="bg-base-200 rounded-lg p-4 text-sm overflow-auto max-h-96 font-mono leading-relaxed">{@result.body}</pre>
      </div>
    </div>
    """
  end
end
