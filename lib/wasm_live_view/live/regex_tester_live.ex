defmodule WasmLiveView.RegexTesterLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents

  # Only metadata — no anonymous functions (can't be escaped into module attributes)
  @expected_failures [
    :replace_backref_g_syntax,
    :compile_recursion_error,
    :compile_absent_operator_error
  ]

  @tests [
    {:match_literal, "Regex.match? — literal string"},
    {:match_no_match, "Regex.match? — no match returns false"},
    {:match_caseless, "Regex.match? — case insensitive /i"},
    {:match_anchor_start, "Regex.match? — ^ start anchor"},
    {:match_anchor_end, "Regex.match? — $ end anchor"},
    {:match_multiline, "Regex.match? — multiline /m anchors"},
    {:run_basic, "Regex.run — returns first match"},
    {:run_no_match, "Regex.run — nil on no match"},
    {:run_groups, "Regex.run — multiple capture groups"},
    {:run_named, "Regex.named_captures — named groups"},
    {:run_capture_first, "Regex.run — capture: :first"},
    {:run_capture_all_but_first, "Regex.run — capture: :all_but_first"},
    {:run_capture_none, "Regex.run — capture: :none"},
    {:run_capture_named_list, "Regex.run — capture by named group list"},
    {:run_capture_all_names, "Regex.run — capture: :all_names (alphabetical named groups)"},
    {:run_capture_missing_named, "Regex.run — missing named capture yields empty"},
    {:run_capture_mixed_list, "Regex.run — capture mixed list (index + name atoms/binaries)"},
    {:run_capture_index_oob, "Regex.run — out-of-range capture index yields empty"},
    {:run_return_list, "Regex.run — return: :list"},
    {:run_no_match_with_offset, "Regex.run — no match after offset returns nil"},
    {:run_return_index_ascii, "Regex.run — return: :index on ASCII"},
    {:run_return_index_unicode, "Regex.run — return: :index on UTF-8"},
    {:run_return_index_unmatched_group, "Regex.run — return:index unmatched group gives {-1,0}"},
    {:run_offset_ascii, "Regex.run — offset in bytes (ASCII)"},
    {:run_offset_unicode, "Regex.run — offset in bytes (UTF-8)"},
    {:run_lookahead, "Regex.run — positive lookahead"},
    {:run_negative_lookbehind, "Regex.run — negative lookbehind"},
    {:match_pcre_string_anchors, "Regex.match? — PCRE \\A and \\z anchors"},
    {:scan_all, "Regex.scan — all matches"},
    {:scan_groups, "Regex.scan — with capture groups"},
    {:scan_return_index, "Regex.scan — return: :index"},
    {:scan_capture_first, "Regex.scan — capture: :first"},
    {:scan_offset, "Regex.scan — offset skips earlier matches"},
    {:scan_zero_width_progress, "Regex.scan — zero-width global match makes progress"},
    {:scan_all_names, "Regex.scan — capture: :all_names with global matches"},
    {:split_basic, "Regex.split — split on pattern"},
    {:split_limit, "Regex.split — with parts limit"},
    {:split_parts_one, "Regex.split — parts: 1 returns original"},
    {:split_trim, "Regex.split — trim empty results"},
    {:split_include_captures, "Regex.split — include captures"},
    {:split_on_named_capture, "Regex.split — split on named capture"},
    {:split_on_named_capture_no_include, "Regex.split — split on named capture without include"},
    {:split_empty_input_trimmed, "Regex.split — empty input with trim"},
    {:split_empty_pattern, "Regex.split — empty regex around graphemes"},
    {:replace_basic, "Regex.replace — simple replacement"},
    {:replace_backref, "Regex.replace — with back-reference"},
    {:replace_whole_match_backref, "Regex.replace — whole-match backref \\0"},
    {:replace_backref_g_syntax, "Regex.replace — back-reference \\g{N} syntax (expected fail)"},
    {:replace_global_false, "Regex.replace — global: false replaces first only"},
    {:replace_function, "Regex.replace — replacement function"},
    {:replace_function_with_captures, "Regex.replace — replacement function with captures"},
    {:compile_ok, "Regex.compile — valid pattern returns {:ok, regex}"},
    {:compile_error, "Regex.compile — invalid pattern returns {:error, ...}"},
    {:compile_opts_binary, "Regex.compile — option string (i)"},
    {:compile_opts_list, "Regex.compile — option list (:caseless)"},
    {:compile_invalid_option, "Regex.compile — invalid option is rejected"},
    {:compile_pcre_named_capture, "Regex.compile — (?P<name>...) syntax"},
    {:compile_atomic_group, "Regex.compile — atomic groups (PCRE compatibility)"},
    {:compile_possessive_quantifier_error, "Regex.compile — possessive quantifier unsupported"},
    {:compile_conditional_group_error, "Regex.compile — conditional group unsupported"},
    {:compile_branch_reset_error, "PCRE2 target — branch reset (?|...) (compile fallback, partial)"},
    {:compile_recursion_error, "PCRE2 target — recursion (?R) (expected fail)"},
    {:compile_subroutine_error, "PCRE2 target — subroutine call (?1) (compile fallback, partial)"},
    {:compile_backtracking_verb_error,
     "PCRE2 target — backtracking verbs (*SKIP)(*FAIL) (compile fallback, partial)"},
    {:compile_callout_error, "PCRE2 target — callout (?C...) (compile fallback, partial)"},
    {:compile_absent_operator_error, "PCRE2 target — absent operator (?~...) (expected fail)"},
    {:compile_keep_operator_error, "PCRE2 target — keep operator \\K (compile fallback, partial)"},
    {:compile_keep_operator_runtime_semantics,
     "PCRE2 target — keep operator \\K runtime semantics (partial, fixed-prefix)"},
    {:compile_keep_operator_capture_left,
     "PCRE2 target — keep operator \\K keeps left captures available"},
    {:compile_keep_operator_index,
     "PCRE2 target — keep operator \\K adjusts whole-match index only"},
    {:compile_keep_operator_scan,
     "PCRE2 target — keep operator \\K works with Regex.scan global matches"},
    {:run_pcre_named_backref, "PCRE2 target — named backref (?P=name)"},
    {:match_pcre_Z_anchor, "PCRE2 target — \\Z end anchor before final newline"},
    {:match_pcre_R_newline, "PCRE2 target — \\R newline sequence class"},
    {:match_pcre_hv_classes, "PCRE2 target — \\h/\\H and \\v/\\V classes"},
    {:optional_unmatched_group, "Regex.run — unmatched optional capture is empty"},
    {:non_capturing_group, "Regex.run — non-capturing groups"},
    {:lookbehind, "Regex.run — lookbehind support"},
    {:non_greedy_quantifier, "Regex.run — non-greedy quantifier"},
    {:alternation_leftmost, "Regex.run — alternation prefers leftmost-first branch"},
    {:nested_optional_groups, "Regex.run — nested optionals keep unmatched capture empty"},
    {:unicode_property_class, "Unicode — property class \\p{Sc}"},
    {:unicode_posix_class, "Unicode — POSIX class [[:lower:]] with /u"},
    {:unicode_word_class, "Unicode — \\w with /u matches accented letters"},
    {:emoji_byte_index, "Unicode — emoji index is byte-accurate"},
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

  defp run_test(:match_anchor_start),
    do: Regex.match?(~r/^hello/, "hello world") == true

  defp run_test(:match_anchor_end),
    do: Regex.match?(~r/world$/, "hello world") == true

  defp run_test(:match_multiline),
    do: Regex.match?(~r/^world$/m, "hello\nworld") == true

  defp run_test(:run_basic),
    do: Regex.run(~r/(\d+)/, "age: 42") == ["42", "42"]

  defp run_test(:run_no_match),
    do: Regex.run(~r/(\d+)/, "no numbers") == nil

  defp run_test(:run_groups),
    do:
      Regex.run(~r/(\w+)@(\w+)\.(\w+)/, "user@example.com") ==
        ["user@example.com", "user", "example", "com"]

  defp run_test(:run_named),
    do:
      Regex.named_captures(~r/(?<year>\d{4})-(?<month>\d{2})/, "2026-02-28") ==
        %{"year" => "2026", "month" => "02"}

  defp run_test(:run_capture_first),
    do: Regex.run(~r/(\w+)@(\w+)/, "user@host", capture: :first) == ["user@host"]

  defp run_test(:run_capture_all_but_first),
    do: Regex.run(~r/(\w+)@(\w+)/, "user@host", capture: :all_but_first) == ["user", "host"]

  defp run_test(:run_capture_none),
    do: Regex.run(~r/(\w+)@(\w+)/, "user@host", capture: :none) == []

  defp run_test(:run_capture_named_list),
    do:
      Regex.run(~r/(?<user>\w+)@(?<host>\w+)/, "user@host", capture: ["host", "user"]) ==
        ["host", "user"]

  defp run_test(:run_capture_all_names),
    do: Regex.run(~r/(?<z>\d)(?<a>[a-z])/, "1a", capture: :all_names) == ["a", "1"]

  defp run_test(:run_capture_missing_named),
    do: Regex.run(~r/a(?<foo>b)c/, "abc", capture: ["foo", "bar"]) == ["b", ""]

  defp run_test(:run_capture_mixed_list),
    do:
      Regex.run(~r/(?<user>\w+)@(?<host>\w+)/, "user@host", capture: [0, :host, "user"]) ==
        ["user@host", "host", "user"]

  defp run_test(:run_capture_index_oob),
    do: Regex.run(~r/(ab)/, "ab", capture: [0, 1, 2]) == ["ab", "ab", ""]

  defp run_test(:run_return_list),
    do: Regex.run(~r/(\d+)/, "a42", return: :list) == [~c"42", ~c"42"]

  defp run_test(:run_no_match_with_offset),
    do: Regex.run(~r/\d+/, "abc123", offset: 6) == nil

  defp run_test(:run_return_index_ascii),
    do: Regex.run(~r/(\d+)/, "age: 42", return: :index) == [{5, 2}, {5, 2}]

  defp run_test(:run_return_index_unicode),
    do: Regex.run(~r/é/u, "aé", return: :index) == [{1, 2}]

  defp run_test(:run_return_index_unmatched_group),
    do: Regex.run(~r/(a)?b/, "b", return: :index) == [{0, 1}, {-1, 0}]

  defp run_test(:run_offset_ascii),
    do: Regex.run(~r/\d+/, "a1 b22 c333", offset: 3) == ["22"]

  defp run_test(:run_offset_unicode),
    do: Regex.run(~r/é/u, "éé", offset: 2) == ["é"]

  defp run_test(:run_lookahead),
    do: Regex.run(~r/\w+(?=:)/, "host:443") == ["host"]

  defp run_test(:run_negative_lookbehind),
    do: Regex.run(~r/(?<!#)\btag\b/, "tag #tag") == ["tag"]

  defp run_test(:match_pcre_string_anchors),
    do: Regex.match?(~r/\Ahello\z/, "hello") == true

  defp run_test(:scan_all),
    do: Regex.scan(~r/\d+/, "a1 b22 c333") == [["1"], ["22"], ["333"]]

  defp run_test(:scan_groups),
    do:
      Regex.scan(~r/(\w)(\d+)/, "a1 b22 c333") ==
        [["a1", "a", "1"], ["b22", "b", "22"], ["c333", "c", "333"]]

  defp run_test(:scan_return_index),
    do: Regex.scan(~r/=+/, "=ü†ƒ8===", return: :index) == [[{0, 1}], [{9, 3}]]

  defp run_test(:scan_capture_first),
    do: Regex.scan(~r/c(d|e)/, "abcd abce", capture: :first) == [["cd"], ["ce"]]

  defp run_test(:scan_offset),
    do: Regex.scan(~r/\d+/, "a1 b22 c333", offset: 3) == [["22"], ["333"]]

  defp run_test(:scan_zero_width_progress),
    do: length(Regex.scan(~r//, "abc")) == 4

  defp run_test(:scan_all_names),
    do:
      Regex.scan(~r/(?<z>\d)(?<a>[a-z])/, "1a2b", capture: :all_names) == [["a", "1"], ["b", "2"]]

  defp run_test(:split_basic),
    do: Regex.split(~r/\s*,\s*/, "a, b,  c ,d") == ["a", "b", "c", "d"]

  defp run_test(:split_limit),
    do: Regex.split(~r/\s+/, "one two three four", parts: 3) == ["one", "two", "three four"]

  defp run_test(:split_parts_one),
    do: Regex.split(~r/-/, "a-b-c", parts: 1) == ["a-b-c"]

  defp run_test(:split_trim),
    do: Regex.split(~r/-/, "-a-b--c", trim: true) == ["a", "b", "c"]

  defp run_test(:split_include_captures),
    do: Regex.split(~r/(x)/, "Elixir", include_captures: true) == ["Eli", "x", "ir"]

  defp run_test(:split_on_named_capture),
    do:
      Regex.split(~r/a(?<second>b)c/, "abc",
        on: [:second],
        include_captures: true
      ) == ["a", "b", "c"]

  defp run_test(:split_on_named_capture_no_include),
    do: Regex.split(~r/a(?<second>b)c/, "abc", on: [:second]) == ["a", "c"]

  defp run_test(:split_empty_input_trimmed),
    do: Regex.split(~r/-/, "", trim: true) == []

  defp run_test(:split_empty_pattern),
    do: Regex.split(~r//, "abc") == ["", "a", "b", "c", ""]

  defp run_test(:replace_basic),
    do: Regex.replace(~r/\d+/, "v1.2.3", "X") == "vX.X.X"

  defp run_test(:replace_backref),
    do: Regex.replace(~r/(\w+)@(\w+)/, "user@host", "\\2/\\1") == "host/user"

  defp run_test(:replace_whole_match_backref),
    do: Regex.replace(~r/\d+/, "abc123def", "[\\0]") == "abc[123]def"

  defp run_test(:replace_backref_g_syntax),
    do: Regex.replace(~r/(\w+)@(\w+)/, "user@host", "\\g{2}/\\g{1}") == "host/user"

  defp run_test(:replace_global_false),
    do: Regex.replace(~r/\d+/, "v1.2.3", "X", global: false) == "vX.2.3"

  defp run_test(:replace_function),
    do: Regex.replace(~r/\d+/, "a1b22", fn m -> "<#{m}>" end) == "a<1>b<22>"

  defp run_test(:replace_function_with_captures),
    do:
      Regex.replace(~r/(\w+)=(\d+)/, "a=1 b=22", fn _m, k, v -> "#{k}:#{v}" end) ==
        "a:1 b:22"

  defp run_test(:compile_ok),
    do: match?({:ok, %Regex{}}, Regex.compile("\\d+"))

  defp run_test(:compile_error),
    do: match?({:error, _}, Regex.compile("[invalid"))

  defp run_test(:compile_opts_binary),
    do: match?({:ok, %Regex{}}, Regex.compile("hello", "i"))

  defp run_test(:compile_opts_list),
    do: match?({:ok, %Regex{}}, Regex.compile("hello", [:caseless]))

  defp run_test(:compile_invalid_option),
    do: match?({:error, {:invalid_option, _}}, Regex.compile("hello", "z"))

  defp run_test(:compile_pcre_named_capture),
    do: match?({:ok, %Regex{}}, Regex.compile("(?P<word>\\w+)"))

  defp run_test(:compile_atomic_group),
    do: match?({:ok, %Regex{}}, Regex.compile("(?>a|ab)c"))

  defp run_test(:compile_possessive_quantifier_error),
    do: match?({:error, _}, Regex.compile("a++"))

  defp run_test(:compile_conditional_group_error),
    do: match?({:error, _}, Regex.compile("(a)?(?(1)b|c)"))

  defp run_test(:compile_branch_reset_error),
    do: match?({:ok, %Regex{}}, Regex.compile("(?|a|b)"))

  defp run_test(:compile_recursion_error),
    do: match?({:ok, %Regex{}}, Regex.compile("(?R)"))

  defp run_test(:compile_subroutine_error),
    do: match?({:ok, %Regex{}}, Regex.compile("(a)(?1)"))

  defp run_test(:compile_backtracking_verb_error),
    do: match?({:ok, %Regex{}}, Regex.compile("a(*SKIP)(*FAIL)|b"))

  defp run_test(:compile_callout_error),
    do: match?({:ok, %Regex{}}, Regex.compile("(?C1)a"))

  defp run_test(:compile_absent_operator_error),
    do: match?({:ok, %Regex{}}, Regex.compile("(?~foo)"))

  defp run_test(:compile_keep_operator_error),
    do: match?({:ok, %Regex{}}, Regex.compile("foo\\Kbar", "u"))

  defp run_test(:compile_keep_operator_runtime_semantics),
    do: Regex.run(Regex.compile!("foo\\Kbar", "u"), "foobar") == ["bar"]

  defp run_test(:compile_keep_operator_capture_left),
    do: Regex.run(Regex.compile!("(foo)\\Kbar", "u"), "foobar") == ["bar", "foo"]

  defp run_test(:compile_keep_operator_index),
    do: Regex.run(Regex.compile!("(foo)\\Kbar", "u"), "foobar", return: :index) == [{3, 3}, {0, 3}]

  defp run_test(:compile_keep_operator_scan),
    do: Regex.scan(Regex.compile!("foo\\Kbar", "u"), "foobar xx foobar") == [["bar"], ["bar"]]

  defp run_test(:run_pcre_named_backref),
    do:
      Regex.run(Regex.compile!("(?P<word>\\w+)-(?P=word)", "u"), "abc-abc") ==
        ["abc-abc", "abc"]

  defp run_test(:match_pcre_Z_anchor),
    do: Regex.match?(Regex.compile!("foo\\Z"), "foo\n") == true

  defp run_test(:match_pcre_R_newline),
    do:
      Regex.match?(Regex.compile!("a\\Rb", "u"), "a\nb") == true and
        Regex.match?(Regex.compile!("a\\Rb", "u"), "a\r\nb") == true

  defp run_test(:match_pcre_hv_classes),
    do:
      Regex.match?(Regex.compile!("^\\h+$", "u"), "\t ") == true and
        Regex.match?(Regex.compile!("^\\H+$", "u"), "ab") == true and
        Regex.match?(Regex.compile!("^\\v+$", "u"), "\n\r") == true and
        Regex.match?(Regex.compile!("^\\V+$", "u"), "ab") == true

  defp run_test(:optional_unmatched_group),
    do: Regex.run(~r/(a)?b/, "b") == ["b", ""]

  defp run_test(:non_capturing_group),
    do: Regex.run(~r/(?:ab)+/, "ababx") == ["abab"]

  defp run_test(:lookbehind),
    do: Regex.run(~r/(?<=#)\w+/, "tag #elixir") == ["elixir"]

  defp run_test(:non_greedy_quantifier),
    do: Regex.run(~r/<.*?>/, "<a><b>") == ["<a>"]

  defp run_test(:alternation_leftmost),
    do: Regex.run(~r/foo|foobar/, "foobar") == ["foo"]

  defp run_test(:nested_optional_groups),
    do: Regex.run(~r/(a(b)?)?c/, "c") == ["c", "", ""]

  defp run_test(:unicode_property_class),
    do: Regex.scan(~r/\p{Sc}/u, "$, £, and €") == [["$"], ["£"], ["€"]]

  defp run_test(:unicode_posix_class),
    do: Regex.match?(~r/^[[:lower:]]+$/u, "josé") == true

  defp run_test(:unicode_word_class),
    do: Regex.match?(~r/^\w+$/u, "mañana") == true

  defp run_test(:emoji_byte_index),
    do: Regex.run(~r/./u, "💡", return: :index) == [{0, 4}]

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
          run_test_with_timeout(key, 1_500)
          |> normalize_expected_failure(key)

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

  defp run_test_with_timeout(key, timeout_ms) do
    parent = self()
    ref = make_ref()

    pid =
      spawn(fn ->
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

        send(parent, {ref, result})
      end)

    receive do
      {^ref, result} ->
        result
    after
      timeout_ms ->
        Process.exit(pid, :kill)
        {:error, "timeout after #{timeout_ms}ms"}
    end
  end

  defp normalize_expected_failure(result, key) do
    if key in @expected_failures do
      case result do
        {:error, reason} -> {:ok, "expected fail: #{reason}"}
        {:ok, _} -> {:error, "unexpected pass (was marked expected fail)"}
      end
    else
      result
    end
  end

  defp expected_failure?(key), do: key in @expected_failures

  defp real_pass_count(results) do
    Enum.count(results, fn {k, v} -> !expected_failure?(k) and match?({:ok, _}, v) end)
  end

  defp expected_fail_count(results) do
    Enum.count(results, fn {k, v} -> expected_failure?(k) and match?({:ok, _}, v) end)
  end

  defp pass_count(results) do
    real_pass_count(results) + expected_fail_count(results)
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
          ({real_pass_count(@results)} real, {expected_fail_count(@results)} expected)
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
        <p>
          Known limitation: <code class="font-mono">Regex.replace</code> with
          <code class="font-mono">\\g{N}</code> back-references is expected to fail on current
          AtomVM/WASM builds due to missing NIF support (`binary:list_to_bin/1`) in the
          replacement parser path.
        </p>
        <p>
          Rows marked <code class="font-mono">(expected fail)</code> are tracked limitations;
          any unmarked failing row is a regression.
        </p>
        <p>
          Rows marked <code class="font-mono">(compile fallback, partial)</code> only guarantee
          compile compatibility or limited semantics, not full PCRE2 equivalence.
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
