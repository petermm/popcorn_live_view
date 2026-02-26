defmodule WasmLiveView.PackbeamLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :current_route, :packbeam) |> assign(result: nil, error: nil, loading: false)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <.header>
      Packbeam
    </.header>

    <div class="mt-8">
      <form phx-submit="save" class="flex flex-col gap-4 max-w-lg">
        <div>
          <label for="source" class="block text-sm font-medium leading-6 text-base-content mb-2">Erlang source</label>
          <textarea id="source" name="source" rows="6" class="textarea textarea-bordered w-full">-module(a).
-export([start/0]).
start() -> ok.</textarea>
        </div>
        <div>
          <.button type="submit" variant="primary" disabled={@loading}>
            {if @loading, do: "Processing…", else: "Submit"}
          </.button>
        </div>
      </form>

      <div :if={@error} class="alert alert-error mt-4">
        {@error}
      </div>

      <div :if={@result} class="mt-6">
        <div class="badge badge-neutral mb-2">Result</div>
        <pre class="bg-base-200 rounded p-4 text-sm overflow-auto max-h-96">{@result}</pre>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("save", %{"source" => source}, socket) do
    lv = self()
    spawn(fn ->
      result =
        try do
          input = WasmLiveView.EvalInWasm.compile_erlang(source)
          case input do
            {:ok, compiled} ->
              IO.inspect(compiled, label: "compiled!!")
              IO.inspect(:packbeam_api.module_info(:exports), label: "packbeam_api exports")
              result = :packbeam_api.create_from_binaries(compiled)
              IO.inspect(result, label: "create_from_binaries result")
              result
            {:error, reason} ->
              {:error, reason, "compile failed for: #{source}"}
          end
        catch
          kind, reason ->
            stacktrace = __STACKTRACE__
            IO.puts("** CATCH kind=#{inspect(kind)} reason=#{inspect(reason)}")
            IO.puts("** stacktrace:")
            Enum.each(stacktrace, fn frame -> IO.puts("   #{inspect(frame)}") end)
            {:error, {kind, inspect(reason), Exception.format_stacktrace(stacktrace)}}
        end
      send(lv, {:packbeam_result, result})
    end)
    {:noreply, assign(socket, loading: true, result: nil, error: nil)}
  end

  @impl true
  def handle_info({:packbeam_result, {:ok, avm_binary}}, socket) when is_binary(avm_binary) do
    encoded = Base.encode64(avm_binary)
    {:noreply, assign(socket, loading: false, result: "AVM file (#{byte_size(avm_binary)} bytes):\n\n#{encoded}")}
  end

  def handle_info({:packbeam_result, {:error, reason, input}}, socket) do
    error_msg = "#{reason}\n\nInput: #{input}"
    {:noreply, assign(socket, loading: false, error: error_msg)}
  end

  def handle_info({:packbeam_result, {:error, {kind, reason, stacktrace}}}, socket) do
    error_msg = "#{kind}: #{reason}\n\n#{stacktrace}"
    {:noreply, assign(socket, loading: false, error: error_msg)}
  end
end
