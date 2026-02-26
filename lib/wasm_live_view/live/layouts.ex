defmodule WasmLiveView.Layouts do
  use Phoenix.Component

  @nav_items [
    %{path: "/", label: "Notes", key: :home},
    %{path: "/counter", label: "Counter", key: :counter},
    %{path: "/streams", label: "Streams", key: :streams},
    %{path: "/eval", label: "Eval", key: :eval},
    %{path: "/interop", label: "JS Interop", key: :interop},
    %{path: "/notes-persisted", label: "Notes (Persisted)", key: :notes_persisted},
  ]

  def app(assigns) do
    assigns = assign(assigns, :nav_items, @nav_items)

    ~H"""
    <div class="flex min-h-screen">
      <nav class="w-56 bg-base-200 border-r border-base-300 p-6 shrink-0">
        <div class="text-xl font-bold mb-6">LiveView WASM</div>
        <ul class="menu w-full gap-1 p-0">
          <li :for={item <- @nav_items}>
            <.link
              navigate={item.path}
              class={if item.key == assigns[:current_route], do: "menu-active", else: ""}
            >
              {item.label}
            </.link>
          </li>
        </ul>
      </nav>
      <main class="flex-1 p-10 max-w-3xl">
        {@inner_content}
      </main>
    </div>
    """
  end
end
