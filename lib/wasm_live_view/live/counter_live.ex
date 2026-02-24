defmodule WasmLiveView.CounterLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, count: 0, current_route: :counter)}
  end

  @impl true
  def handle_event("increment", _params, socket) do
    {:noreply, update(socket, :count, &(&1 + 1))}
  end

  def handle_event("decrement", _params, socket) do
    {:noreply, update(socket, :count, &(&1 - 1))}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <.header>
      Counter
      <:subtitle>A real Phoenix LiveView running in the browser via Popcorn WASM.</:subtitle>
    </.header>

    <div class="text-6xl font-bold text-center my-8">
      {@count}
    </div>

    <div class="flex justify-center gap-2">
      <.button phx-click="decrement" class="btn btn-lg">-</.button>
      <.button phx-click="increment" class="btn btn-lg">+</.button>
    </div>
    """
  end
end
