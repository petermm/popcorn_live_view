defmodule WasmLiveView.RegexTesterLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents

  # Only metadata — no anonymous functions (can't be escaped into module attributes)
  @tests [
    {:match_literal, "Regex.match? — literal string"},
    {:match_no_match, "Regex.match? — no match returns false"},
    {:match_caseless, "Regex.match? — case insensitive /i"},
    {:run_basic, "Regex.run — returns first match"},
    {:run_no_match, "Regex.run — nil on no match"},
    {:run_groups, "Regex.run — multiple capture groups"},
    {:run_named, "Regex.named_captures — named groups"},
    {:scan_all, "Regex.scan — all matches"},
    {:scan_groups, "Regex.scan — with capture groups"},
    {:split_basic, "Regex.split — split on pattern"},
    {:split_limit, "Regex.split — with parts limit"},
    {:replace_basic, "Regex.replace — simple replacement"},
    {:replace_backref, "Regex.replace — with back-reference"},
    {:compile_ok, "Regex.compile — valid pattern returns {:ok, regex}"},
    {:compile_error, "Regex.compile — invalid pattern returns {:error, ...}"},
    {:unicode_match, "Unicode — match non-ASCII characters"},
    {:empty_match, "Empty string — match against empty pattern"},
    {:dotall, "Dotall /s — dot matches newline"},
    {:uri_parse, "URI.parse — full URL (uses :re internally)"},
    {:uri_parse_simple, "URI.parse — simple path-only URI"}
  ]

  # --- Test implementations (pattern-matched, no closures) ---

  defp run_test(:match_literal),
    do: Regex.match?(~r/hello/, "hello world") == true

  defp run_test(:match_no_match),
    do: Regex.match?(~r/xyz/, "hello world") == false

  defp run_test(:match_caseless),
    do: Regex.match?(~r/hello/i, "HELLO WORLD") == true

  defp run_test(:run_basic),
    do: Regex.run(~r/(\d+)/, "age: 42") == ["42", "42"]

  defp run_test(:run_no_match),
    do: Regex.run(~r/(\d+)/, "no numbers") == nil

  defp run_test(:run_groups),
    do: Regex.run(~r/(\w+)@(\w+)\.(\w+)/, "user@example.com") ==
          ["user@example.com", "user", "example", "com"]

  defp run_test(:run_named),
    do: Regex.named_captures(~r/(?<year>\d{4})-(?<month>\d{2})/, "2026-02-28") ==
          %{"year" => "2026", "month" => "02"}

  defp run_test(:scan_all),
    do: Regex.scan(~r/\d+/, "a1 b22 c333") == [["1"], ["22"], ["333"]]

  defp run_test(:scan_groups),
    do: Regex.scan(~r/(\w)(\d+)/, "a1 b22 c333") ==
          [["a1", "a", "1"], ["b22", "b", "22"], ["c333", "c", "333"]]

  defp run_test(:split_basic),
    do: Regex.split(~r/\s*,\s*/, "a, b,  c ,d") == ["a", "b", "c", "d"]

  defp run_test(:split_limit),
    do: Regex.split(~r/\s+/, "one two three four", parts: 3) == ["one", "two", "three four"]

  defp run_test(:replace_basic),
    do: Regex.replace(~r/\d+/, "v1.2.3", "X") == "vX.X.X"

  defp run_test(:replace_backref),
    do: Regex.replace(~r/(\w+)@(\w+)/, "user@host", "\\2/\\1") == "host/user"

  defp run_test(:compile_ok),
    do: match?({:ok, %Regex{}}, Regex.compile("\\d+"))

  defp run_test(:compile_error),
    do: match?({:error, _}, Regex.compile("[invalid"))

  defp run_test(:unicode_match),
    do: Regex.match?(~r/café/u, "I love café") == true

  defp run_test(:empty_match),
    do: Regex.match?(~r//, "") == true

  defp run_test(:dotall),
    do: Regex.match?(~r/hello.world/s, "hello\nworld") == true

  defp run_test(:uri_parse) do
    uri = URI.parse("https://httpbin.org:443/get?foo=bar#frag")

    uri.scheme == "https" and uri.host == "httpbin.org" and
      uri.port == 443 and uri.path == "/get" and
      uri.query == "foo=bar" and uri.fragment == "frag"
  end

  defp run_test(:uri_parse_simple) do
    uri = URI.parse("/hello/world")
    uri.path == "/hello/world" and uri.scheme == nil
  end

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       current_route: :regex_tester,
       tests: @tests,
       results: %{},
       loading: false,
       current_test: nil
     )}
  end

  @impl true
  def handle_event("run", _params, socket) do
    lv = self()

    spawn(fn ->
      Enum.each(@tests, fn {key, _desc} ->
        send(lv, {:test_start, key})

        result =
          try do
            case run_test(key) do
              true -> {:ok, "pass"}
              false -> {:error, "assertion failed"}
              other -> {:error, "unexpected: #{inspect(other)}"}
            end
          catch
            kind, reason ->
              {:error, "#{kind}: #{inspect(reason)}"}
          end

        send(lv, {:test_result, key, result})
      end)

      send(lv, :tests_done)
    end)

    {:noreply, assign(socket, loading: true, results: %{}, current_test: nil)}
  end

  @impl true
  def handle_info({:test_start, key}, socket) do
    {:noreply, assign(socket, current_test: key)}
  end

  def handle_info({:test_result, key, result}, socket) do
    {:noreply, assign(socket, results: Map.put(socket.assigns.results, key, result))}
  end

  def handle_info(:tests_done, socket) do
    {:noreply, assign(socket, loading: false, current_test: nil)}
  end

  defp pass_count(results) do
    Enum.count(results, fn {_, v} -> match?({:ok, _}, v) end)
  end

  @impl true
  def render(assigns) do
    total = length(assigns.tests)
    assigns = assign(assigns, total: total)

    ~H"""
    <.header>
      Regex Tester
      <:subtitle>
        Tests for the <code class="font-mono">:re</code> stub — JavaScript RegExp backing Elixir's Regex module in AtomVM WASM.
      </:subtitle>
    </.header>

    <div class="mt-8 flex flex-col gap-4 max-w-2xl">
      <div class="flex items-center gap-4">
        <.button phx-click="run" disabled={@loading}>
          {if @loading, do: "Running…", else: "Run All"}
        </.button>
        <span :if={@loading && @current_test} class="text-sm text-base-content/60 italic">
          running {@current_test}…
        </span>
        <span :if={map_size(@results) > 0 && !@loading} class="text-sm text-base-content/60">
          {pass_count(@results)}/{@total} passed
        </span>
      </div>

      <table class="table table-sm w-full">
        <thead>
          <tr>
            <th class="w-8"></th>
            <th>Test</th>
            <th>Result</th>
          </tr>
        </thead>
        <tbody>
          <tr :for={{key, desc} <- @tests} class="hover">
            <td><.status_icon key={key} results={@results} current={@current_test} /></td>
            <td class="text-sm">{desc}</td>
            <td class="text-sm"><.result_cell key={key} results={@results} /></td>
          </tr>
        </tbody>
      </table>

      <div class="rounded-lg bg-base-200 border border-base-300 px-4 py-3 text-xs text-base-content/70 space-y-1">
        <p class="font-semibold text-base-content/80">How it works</p>
        <p>
          The <code class="font-mono">:re</code> stub in <code class="font-mono">stubs/re_stub.erl</code>
          delegates <code class="font-mono">:re.compile/2</code>, <code class="font-mono">:re.run/3</code>,
          and <code class="font-mono">:re.inspect/2</code> to JavaScript's <code class="font-mono">RegExp</code>
          via <code class="font-mono">Popcorn.Wasm.run_js!</code>.
          Byte-accurate offsets are computed using <code class="font-mono">TextEncoder</code> (UTF-8).
        </p>
      </div>
    </div>
    """
  end

  defp status_icon(%{key: key, results: results, current: current} = assigns) do
    assigns =
      assign(assigns,
        ok: match?({:ok, _}, results[key]),
        err: match?({:error, _}, results[key]),
        spinning: current == key && !Map.has_key?(results, key)
      )

    ~H"""
    <span :if={@ok} class="text-success">✓</span>
    <span :if={@err} class="text-error">✗</span>
    <span :if={@spinning} class="loading loading-spinner loading-xs"></span>
    """
  end

  defp result_cell(%{key: key, results: results} = assigns) do
    assigns = assign(assigns, r: results[key])

    ~H"""
    <span :if={match?({:ok, _}, @r)} class="text-success">{elem(@r, 1)}</span>
    <span :if={match?({:error, _}, @r)} class="text-error">
      <details>
        <summary class="cursor-pointer">error</summary>
        <pre class="text-xs whitespace-pre-wrap mt-1">{elem(@r, 1)}</pre>
      </details>
    </span>
    """
  end
end
