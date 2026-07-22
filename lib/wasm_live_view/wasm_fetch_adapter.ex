defmodule WasmLiveView.WasmFetchAdapter do
  @moduledoc """
  A Req adapter that delegates HTTP requests to the browser's fetch API
  via Popcorn.Wasm.run_js/2, enabling Req to work inside AtomVM/WASM.
  """

  alias Popcorn.Wasm

  def run(request) do
    receiver = :"wasm_fetch_#{:erlang.unique_integer([:positive])}"
    Process.register(self(), receiver)

    url = URI.to_string(request.url)
    method = request.method |> to_string() |> String.upcase()
    headers =
      request.headers
      |> Enum.reject(fn {k, _v} -> String.downcase(k) == "user-agent" end)
      |> Enum.map(fn {k, v} -> [k, v] end)
    body = request.body || ""

    # Popcorn's run_js shadows `window` to the *parent* page (not the iframe).
    # Use window.fetch so the request shares the page origin/CORS context.
    # Bare `fetch` would use the iframe realm; both need remote CORS headers.
    Wasm.run_js!(
      """
      ({ wasm, args }) => {
        const doFetch = window.fetch.bind(window);
        doFetch(args.url, {
          method: args.method,
          headers: Object.fromEntries(args.headers),
          body: args.method === "GET" || args.method === "HEAD" ? undefined : args.body,
          mode: "cors",
          credentials: "omit",
          cache: "no-store"
        })
          .then(async r => {
            const body = await r.text();
            const headers = [...r.headers.entries()];
            wasm.cast(args.receiver, {ok: {status: r.status, headers, body}});
          })
          .catch(e => {
            const msg = (e && e.message) ? e.message : String(e);
            // Browsers often surface upstream 5xx without ACAO as a TypeError CORS failure.
            wasm.cast(args.receiver, {
              error: msg + " (browser blocked the response — remote must send Access-Control-Allow-Origin, or the origin may be down)"
            });
          });
      }
      """,
      %{url: url, method: method, headers: headers, body: body, receiver: receiver}
    )

    receive do
      {:emscripten, _} = raw ->
        Process.unregister(receiver)
        case Wasm.parse_message!(raw) do
          {:wasm_cast, %{"ok" => %{"status" => status, "headers" => resp_headers, "body" => body}}} ->
            headers = Enum.map(resp_headers, fn [k, v] -> {k, v} end)
            response = Req.Response.new(status: status, headers: headers, body: body)
            {request, response}
          {:wasm_cast, %{"error" => reason}} ->
            {request, RuntimeError.exception(reason)}
        end
    after
      30_000 ->
        Process.unregister(receiver)
        {request, RuntimeError.exception("fetch timeout")}
    end
  end
end
