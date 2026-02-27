defmodule WasmLiveView.DebugPackLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents

  @test_files [
    {"a.erl",
     """
     -module(a).
     -export([start/0]).
     start() ->
         b:start().
     """},
    {"b.erl",
     """
     -module(b).
     -export([start/0]).
     start() ->
         e:b_calls_me(),
         Module = get_module(),
         Module:test().
     get_module() ->
         c.
     """},
    {"c.erl",
     """
     -module(c).
     -export([test/0]).
     test() ->
         Literal = get_literal(),
         Module = maps:get(module, Literal),
         Module:c_calls_me().
     get_literal() ->
         \#{module => f}.
     """},
    {"d.erl",
     """
     -module(d).
     -export([no_one_calls_me/0]).
     no_one_calls_me() ->
         sorry.
     """},
    {"e.erl",
     """
     -module(e).
     -export([b_calls_me/0]).
     b_calls_me() ->
         ok.
     """},
    {"f.erl",
     """
     -module(f).
     -export([c_calls_me/0]).
     c_calls_me() ->
         ok.
     """},
    {"g.erl",
     """
     -module(g).
     -export([main/1]).
     main(_Args) ->
         c:test().
     """},
    {"x.erl",
     """
     -module(x).
     -export([start/0]).
     start() ->
         a:start().
     """}
  ]

  @api_tests [
    {:multi_module, "create_from_binaries([a,b,c]) → 3 elements"},
    {:io_format_regression, "hello_world with io:format → 1 element"},
    {:source_text, "greet + util from source text → 2 elements"},
    {:lib_option, "create_from_binaries([a,b], \#{lib: true}) → ok"},
    {:prune_drops_d, "prune([a…f]) drops unreachable d"},
    {:prune_sup_callback, "prune keeps my_worker (only in literal child spec map)"}
  ]

  @file_names Enum.map(@test_files, fn {name, _} -> name end)
  @api_test_names Enum.map(@api_tests, fn {name, _} -> name end)

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       current_route: :debug_pack,
       loading: false,
       files: @file_names,
       api_tests: @api_tests,
       api_test_names: @api_test_names,
       results: %{},
       current_file: nil
     )}
  end

  @impl true
  def render(assigns) do
    total = length(assigns.files) + length(assigns.api_test_names)

    assigns = assign(assigns, total: total)

    ~H"""
    <.header>
      Debug Pack — Test Suite
    </.header>

    <div class="mt-8 flex flex-col gap-4 max-w-2xl">
      <div class="flex items-center gap-4">
        <.button phx-click="run" variant="primary" disabled={@loading}>
          {if @loading, do: "Running…", else: "Run All"}
        </.button>
        <span :if={@loading && @current_file} class="text-sm text-base-content/60 italic">
          running {@current_file}…
        </span>
        <span :if={map_size(@results) > 0 && !@loading} class="text-sm text-base-content/60">
          {pass_count(@results)}/{@total} passed
        </span>
      </div>

      <h3 class="text-base font-semibold mt-4">Compile &amp; Pack (single module)</h3>
      <table class="table table-sm w-full">
        <thead>
          <tr>
            <th class="w-8"></th>
            <th>File</th>
            <th>Result</th>
          </tr>
        </thead>
        <tbody>
          <tr :for={file <- @files} class="hover">
            <td><.status_cell key={file} results={@results} current={@current_file} /></td>
            <td class="font-mono text-sm">{file}</td>
            <td class="text-sm"><.result_cell key={file} results={@results} /></td>
          </tr>
        </tbody>
      </table>

      <h3 class="text-base font-semibold mt-4">API Tests (multi-module &amp; options)</h3>
      <table class="table table-sm w-full">
        <thead>
          <tr>
            <th class="w-8"></th>
            <th>Test</th>
            <th>Result</th>
          </tr>
        </thead>
        <tbody>
          <tr :for={{name, desc} <- @api_tests} class="hover">
            <td><.status_cell key={name} results={@results} current={@current_file} /></td>
            <td class="text-sm">{desc}</td>
            <td class="text-sm"><.result_cell key={name} results={@results} /></td>
          </tr>
        </tbody>
      </table>
    </div>
    """
  end

  defp status_cell(%{key: key, results: results, current: current} = assigns) do
    assigns =
      assign(assigns,
        ok: match?({:ok, _}, results[key]),
        err: match?({:error, _}, results[key]),
        spinning: current == key && !Map.has_key?(results, key)
      )

    ~H"""
    <span :if={@ok}>✓</span>
    <span :if={@err}>✗</span>
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

  defp pass_count(results) do
    Enum.count(results, fn {_, v} -> match?({:ok, _}, v) end)
  end

  @impl true
  def handle_event("run", _params, socket) do
    lv = self()

    spawn(fn ->
      Enum.each(@test_files, fn {file, src} ->
        send(lv, {:pack_start, file})
        result = WasmLiveView.EvalInWasm.compile_and_pack(src)
        send(lv, {:pack_result, file, result})
      end)

      Enum.each(@api_tests, fn {name, _desc} ->
        send(lv, {:pack_start, name})
        result = WasmLiveView.EvalInWasm.run_api_test(name)
        send(lv, {:pack_result, name, result})
      end)

      send(lv, :pack_done)
    end)

    {:noreply, assign(socket, loading: true, results: %{}, current_file: nil)}
  end

  @impl true
  def handle_info({:pack_start, key}, socket) do
    {:noreply, assign(socket, current_file: key)}
  end

  def handle_info({:pack_result, key, result}, socket) do
    {:noreply, assign(socket, results: Map.put(socket.assigns.results, key, result))}
  end

  def handle_info(:pack_done, socket) do
    {:noreply, assign(socket, loading: false, current_file: nil)}
  end
end
