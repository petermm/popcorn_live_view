defmodule WasmLiveView.ReqDemoLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents

  @adapter &WasmLiveView.WasmFetchAdapter.run/1

  @demos [
    %{
      id: :uuid,
      label: "UUID",
      desc:
        "Basic GET — resp.body is automatically decoded to an Elixir map via decode_body: true",
      url: "https://httpbin.org/uuid",
      method: :get,
      body: nil,
      params: nil,
      extra_headers: nil,
      snippet: "Req.get!(uri, adapter: adapter)"
    },
    %{
      id: :params,
      label: "Query Params",
      desc: "Use params: to attach query params — Req builds and appends the query string",
      url: "https://httpbin.org/get",
      method: :get,
      body: nil,
      params: %{"hello" => "atomvm", "runtime" => "wasm"},
      extra_headers: nil,
      snippet: ~S|Req.get!(uri, params: %{hello: "atomvm", runtime: "wasm"})|
    },
    %{
      id: :headers,
      label: "Custom Headers",
      desc: "Send extra request headers and see them echoed back by the server",
      url: "https://httpbin.org/headers",
      method: :get,
      body: nil,
      params: nil,
      extra_headers: [{"x-powered-by", "AtomVM"}, {"x-runtime", "wasm"}],
      snippet: ~S|Req.get!(uri, headers: [{"x-powered-by", "AtomVM"}])|
    },
    %{
      id: :ip,
      label: "My IP",
      desc: "Minimal JSON response — just the caller's public IP address",
      url: "https://httpbin.org/ip",
      method: :get,
      body: nil,
      params: nil,
      extra_headers: nil,
      snippet: "Req.get!(uri)"
    },
    %{
      id: :teapot,
      label: "418 Teapot",
      desc: "Non-2xx status — body stays as binary, no JSON decode attempted",
      url: "https://httpbin.org/status/418",
      method: :get,
      body: nil,
      params: nil,
      extra_headers: nil,
      snippet: "resp.status == 418, resp.body is binary"
    },
    %{
      id: :gzip,
      label: "Gzip",
      desc:
        "httpbin serves a gzip-compressed body — but the browser's fetch() always decompresses and strips content-encoding before exposing the response. AtomVM sees plain text with no special handling needed.",
      url: "https://httpbin.org/gzip",
      method: :get,
      body: nil,
      params: nil,
      extra_headers: nil,
      snippet: "Req.get!(uri, adapter: adapter)  # regular adapter, browser decompresses transparently"
    },
    %{
      id: :post,
      label: "POST JSON",
      desc: "POST with JSON — use json: to let Req encode the body and set content-type",
      url: "https://httpbin.org/post",
      method: :post,
      body: %{"hello" => "from AtomVM", "runtime" => "wasm", "library" => "Req"},
      params: nil,
      extra_headers: nil,
      snippet: ~S|Req.post!(uri, json: %{hello: "from AtomVM"})|
    },
    %{
      id: :custom,
      label: "Custom",
      desc: "Enter your own GET endpoint",
      url: nil,
      method: :get,
      body: nil,
      params: nil,
      extra_headers: nil,
      snippet: nil
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

    lv = self()
    t0 = :erlang.monotonic_time(:millisecond)

    spawn(fn ->
      result =
        try do
          # URI.new!/1 uses :uri_string.parse (available in AtomVM) rather than
          # :re (unavailable in WASM). Passing a %URI{} to Req bypasses the
          # regex-based URL normalisation — URI.parse(%URI{}) is a no-op.
          # uri = URI.new!(url)
          uri = url
          # decode_body: true is the default — Req checks content-type via the MIME
          # package and calls Jason to decode JSON responses automatically.
          req_opts = [adapter: @adapter]

          # params: — Req appends these as a query string via URI.encode_query/1
          req_opts =
            if demo.params,
              do: req_opts ++ [params: demo.params],
              else: req_opts

          # headers: — extra request headers
          req_opts =
            if demo.extra_headers,
              do: req_opts ++ [headers: demo.extra_headers],
              else: req_opts

          resp =
            case demo.method do
              :post ->
                # Idiomatic Req would use json: %{...} to encode + set content-type.
                # We encode to a binary string here for WASM adapter compatibility.
                Req.post!(
                  uri,
                  req_opts ++
                    [
                      body: Jason.encode!(demo.body),
                      headers: [{"content-type", "application/json"}]
                    ]
                )

              _ ->
                Req.get!(uri, req_opts)
            end

          elapsed = :erlang.monotonic_time(:millisecond) - t0

          # resp.body is an Elixir map/list for JSON responses (Req decoded it),
          # or a binary for non-JSON. Re-encode to a pretty string for display.
          {body_text, decoded} =
            case resp.body do
              b when is_binary(b) ->
                {b, false}

              b ->
                text =
                  try do
                    Jason.encode!(b, pretty: true)
                  catch
                    _, _ -> inspect(b)
                  end

                {text, true}
            end

          {:ok,
           %{
             status: resp.status,
             headers: resp.headers,
             body_text: body_text,
             body_size: byte_size(body_text),
             decoded: decoded,
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
        <span :if={@active_demo.params} class="ml-auto text-base-content/40">
          +{map_size(@active_demo.params)} params
        </span>
      </div>

      <%!-- Code snippet --%>
      <div :if={@active_demo.snippet} class="font-mono text-xs bg-base-300/50 rounded-lg px-3 py-2 text-base-content/60">
        <span class="text-base-content/30 select-none"># </span>{@active_demo.snippet}
      </div>

      <%!-- Extra headers preview --%>
      <div :if={@active_demo.extra_headers}>
        <div class="text-xs text-base-content/50 mb-1">Extra request headers</div>
        <table class="table table-xs font-mono">
          <tbody>
            <tr :for={{k, v} <- @active_demo.extra_headers}>
              <td class="text-base-content/50 pr-4">{k}</td>
              <td>{v}</td>
            </tr>
          </tbody>
        </table>
      </div>

      <%!-- POST body preview --%>
      <div :if={@active_demo.body}>
        <div class="text-xs text-base-content/50 mb-1">Request body (json:)</div>
        <pre class="bg-base-200 rounded-lg px-3 py-2 text-xs font-mono">{Jason.encode!(@active_demo.body, pretty: true)}</pre>
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

    <%!-- AtomVM constraint note --%>
    <div class="mt-4 rounded-lg bg-base-200 border border-base-300 px-4 py-3 text-xs text-base-content/70 space-y-1">
      <p class="font-semibold text-base-content/80">AtomVM constraint — no <code class="font-mono">:re</code> module</p>
      <del>
        Req calls <code class="font-mono">URI.parse/1</code> on URL strings, which uses <code class="font-mono">:re</code> internally — unavailable in WASM.
        Instead we build a <code class="font-mono">%URI{}</code> via <code class="font-mono">URI.new!/1</code>
        (backed by <code class="font-mono">:uri_string.parse/1</code>) and pass the struct.
        <code class="font-mono">URI.parse(%URI{})</code> is then a no-op and the regex path is never hit.
      </del>
      <p>Solved by stubbing out the :re module</p>
    </div>

    <%!-- Gzip note --%>
    <div :if={@active_demo.id == :gzip} class="mt-4 rounded-lg bg-info/10 border border-info/30 px-4 py-3 text-xs text-base-content/70 space-y-1">
      <p class="font-semibold text-base-content/80">Browser decompression is transparent</p>
      <p>
        The browser's <code class="font-mono">fetch()</code> API decompresses gzip/br responses and strips
        <code class="font-mono">content-encoding</code> before exposing the body — so AtomVM sees plain text
        and Req needs no special handling. The same <code class="font-mono">WasmFetchAdapter</code> works.
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
          <span :if={@result.decoded} class="badge badge-success badge-outline badge-sm font-mono">
            decoded
          </span>
          <span :if={not @result.decoded} class="badge badge-ghost badge-sm font-mono">
            binary
          </span>
          <span class="text-xs text-base-content/40">{@result.body_size} B</span>
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
        <div class="text-xs text-base-content/50 mb-2">
          Response Body
          <span :if={@result.decoded} class="ml-1 text-base-content/30">
            (Jason-decoded + pretty-printed for display)
          </span>
        </div>
        <pre class="bg-base-200 rounded-lg p-4 text-sm overflow-auto max-h-96 font-mono leading-relaxed">{@result.body_text}</pre>
      </div>
    </div>
    """
  end
end
