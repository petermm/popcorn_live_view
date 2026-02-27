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

  @file_names Enum.map(@test_files, fn {name, _} -> name end)

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       current_route: :debug_pack,
       loading: false,
       files: @file_names,
       results: %{},
       current_file: nil
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <.header>
      Debug Pack — Test Suite
    </.header>

    <div class="mt-8 flex flex-col gap-4 max-w-2xl">
      <p class="text-sm text-base-content/70">
        Compiles each test <code>.erl</code> snippet via the Popcorn compiler
        and packs it via <code>packbeam_api</code>.
      </p>

      <div class="flex items-center gap-4">
        <.button phx-click="run" variant="primary" disabled={@loading}>
          {if @loading, do: "Running…", else: "Run All"}
        </.button>
        <span :if={@loading && @current_file} class="text-sm text-base-content/60 italic">
          compiling {@current_file}…
        </span>
        <span :if={map_size(@results) > 0 && !@loading} class="text-sm text-base-content/60">
          {pass_count(@results)}/{length(@files)} passed
        </span>
      </div>

      <table class="table table-sm w-full mt-2">
        <thead>
          <tr>
            <th class="w-8"></th>
            <th>File</th>
            <th>Result</th>
          </tr>
        </thead>
        <tbody>
          <tr :for={file <- @files} class="hover">
            <td>
              <span :if={match?({:ok, _}, @results[file])}>✓</span>
              <span :if={match?({:error, _}, @results[file])}>✗</span>
              <span
                :if={@current_file == file && !Map.has_key?(@results, file)}
                class="loading loading-spinner loading-xs"
              >
              </span>
            </td>
            <td class="font-mono text-sm">{file}</td>
            <td class="text-sm">
              <span :if={match?({:ok, _}, @results[file])} class="text-success">
                {elem(@results[file], 1)}
              </span>
              <span :if={match?({:error, _}, @results[file])} class="text-error">
                <details>
                  <summary class="cursor-pointer">error</summary>
                  <pre class="text-xs whitespace-pre-wrap mt-1">{elem(@results[file], 1)}</pre>
                </details>
              </span>
            </td>
          </tr>
        </tbody>
      </table>
    </div>
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

      send(lv, :pack_done)
    end)

    {:noreply, assign(socket, loading: true, results: %{}, current_file: nil)}
  end

  @impl true
  def handle_info({:pack_start, file}, socket) do
    {:noreply, assign(socket, current_file: file)}
  end

  def handle_info({:pack_result, file, result}, socket) do
    {:noreply, assign(socket, results: Map.put(socket.assigns.results, file, result))}
  end

  def handle_info(:pack_done, socket) do
    {:noreply, assign(socket, loading: false, current_file: nil)}
  end
end
