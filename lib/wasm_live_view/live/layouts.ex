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
    %{path: @script_prefix <> "/packbeam", label: "Packbeam", key: :packbeam},
    %{path: @script_prefix <> "/debug-pack", label: "Debug Pack", key: :debug_pack},
    %{path: @script_prefix <> "/weather", label: "LocalWeather", key: :weather}
  ]

  def app(assigns) do
    assigns = assign(assigns, :nav_items, @nav_items)

    ~H"""
    <div class="drawer lg:drawer-open">
      <input id="nav-drawer" type="checkbox" class="drawer-toggle" />
      <div class="drawer-content flex flex-col min-h-screen">
        <%!-- Mobile top bar --%>
        <div class="navbar bg-base-200 border-b border-base-300 lg:hidden">
          <label for="nav-drawer" class="btn btn-ghost btn-square drawer-button">
            <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" class="inline-block h-5 w-5 stroke-current">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 6h16M4 12h16M4 18h16"></path>
            </svg>
          </label>
          <span class="text-lg font-bold ml-2">LiveView WASM</span>
        </div>
        <main class="flex-1 p-6 lg:p-10 max-w-3xl w-full">
          {@inner_content}
        </main>
      </div>
      <div class="drawer-side z-40">
        <label for="nav-drawer" aria-label="close sidebar" class="drawer-overlay"></label>
        <nav class="w-56 min-h-full bg-base-200 border-r border-base-300 p-6">
          <div class="text-xl font-bold mb-6 hidden lg:block">LiveView WASM</div>
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
      </div>
    </div>
    """
  end
end
