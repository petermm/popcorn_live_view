defmodule WasmLiveView.Layouts do
  use Phoenix.Component

  @script_prefix Enum.reduce(WasmLiveView.Endpoint.script_name(), "", fn part, acc -> acc <> "/" <> part end)

  @nav_items [
    %{path: @script_prefix <> "/", label: "Notes", key: :home},
    %{path: @script_prefix <> "/counter", label: "Counter", key: :counter},
    %{path: @script_prefix <> "/streams", label: "Streams", key: :streams},
    %{path: @script_prefix <> "/eval", label: "Eval", key: :eval},
    %{path: @script_prefix <> "/interop", label: "JS Interop", key: :interop},
    %{path: @script_prefix <> "/notes-persisted", label: "Notes (Persisted)", key: :notes_persisted},
    %{path: @script_prefix <> "/notes-sqlite", label: "Notes (SQLite)", key: :notes_sqlite},
    %{path: @script_prefix <> "/req-demo", label: "Req Demo", key: :req_demo},
    %{path: @script_prefix <> "/packbeam", label: "Packbeam", key: :packbeam}
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
