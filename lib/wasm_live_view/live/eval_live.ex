defmodule WasmLiveView.EvalLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, code: "", result: nil, current_route: :eval)}
  end

  @impl true
  def handle_event("eval", %{"code" => code}, socket) do
    result = WasmLiveView.EvalInWasm.eval_elixir(code)
    {:noreply, assign(socket, result: result, code: code)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <.header>
      Elixir eval
      <:subtitle>Evaluate Elixir code in the browser via Popcorn WASM.</:subtitle>
    </.header>

    <form phx-submit="eval" class="mt-6">
      <textarea
        name="code"
        rows="6"
        class="textarea textarea-bordered w-full font-mono text-sm"
        placeholder="Enter Elixir code..."
      >{@code}</textarea>

      <div class="mt-4">
        <.button type="submit">Evaluate</.button>
      </div>
    </form>

    <div :if={@result} class="mt-6">
      <div :if={match?({:ok, _}, @result)} class="bg-base-200 rounded-lg p-4 font-mono text-sm whitespace-pre-wrap">
        {elem(@result, 1)}
      </div>
      <div :if={match?({:error, _}, @result)} class="bg-error/10 text-error rounded-lg p-4 font-mono text-sm whitespace-pre-wrap">
        {elem(@result, 1)}
      </div>
    </div>
    """
  end
end
