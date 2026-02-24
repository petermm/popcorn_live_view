defmodule WasmLiveView.StreamsLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents

  @impl true
  def mount(_params, _session, socket) do
    items = for i <- 1..5, do: %{id: i, text: "Item #{i}"}

    {:ok,
     socket
     |> assign(:current_route, :streams)
     |> assign(:next_id, 6)
     |> stream(:items, items)}
  end

  @impl true
  def handle_event("prepend", _params, socket) do
    id = socket.assigns.next_id
    item = %{id: id, text: "Item #{id} (prepended)"}

    {:noreply,
     socket
     |> assign(:next_id, id + 1)
     |> stream_insert(:items, item, at: 0)}
  end

  def handle_event("append", _params, socket) do
    id = socket.assigns.next_id
    item = %{id: id, text: "Item #{id} (appended)"}

    {:noreply,
     socket
     |> assign(:next_id, id + 1)
     |> stream_insert(:items, item, at: -1)}
  end

  def handle_event("delete", %{"dom-id" => dom_id}, socket) do
    {:noreply, stream_delete_by_dom_id(socket, :items, dom_id)}
  end

  def handle_event("reset", _params, socket) do
    items = for i <- 1..3, do: %{id: i, text: "Reset item #{i}"}

    {:noreply,
     socket
     |> assign(:next_id, 4)
     |> stream(:items, items, reset: true)}
  end

  def handle_event("limit-5", _params, socket) do
    items = for i <- 1..10, do: %{id: i, text: "Item #{i}"}

    {:noreply,
     socket
     |> stream(:items, items, reset: true, limit: -5)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <.header>
      Streams
      <:subtitle>Efficient list rendering with stream/3 and stream_insert/4.</:subtitle>
    </.header>

    <div class="flex flex-wrap gap-2 my-4">
      <.button phx-click="prepend" variant="primary">Prepend</.button>
      <.button phx-click="append" variant="primary">Append</.button>
      <.button phx-click="reset" class="btn">Reset (3 items)</.button>
      <.button phx-click="limit-5" class="btn">Reset with limit: -5</.button>
    </div>

    <.table id="items" rows={@streams.items} row_id={fn {id, _} -> id end}>
      <:col :let={{_id, item}} label="ID">{item.id}</:col>
      <:col :let={{_id, item}} label="Text">{item.text}</:col>
      <:action :let={{id, _item}}>
        <.button phx-click="delete" phx-value-dom-id={id} class="btn btn-sm btn-error btn-soft">
          Delete
        </.button>
      </:action>
    </.table>
    """
  end
end
