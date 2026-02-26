defmodule WasmLiveView.InteropDemoLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents
  alias Phoenix.LiveView.JS

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       current_route: :interop,
       # phx-hook demo
       hook_log: [],
       # push_event demo
       random_color: "#6366f1"
     )}
  end

  # Called by the JS Hook via pushEvent("hook-ping", %{msg: ...})
  @impl true
  def handle_event("hook-ping", %{"msg" => msg}, socket) do
    entry = "JS → Server: #{msg}"
    {:noreply, update(socket, :hook_log, fn log -> [entry | log] |> Enum.take(5) end)}
  end

  # Server sends a random color to the JS Hook via push_event
  def handle_event("push-color", _params, socket) do
    color = Enum.random(~w[#ef4444 #f97316 #22c55e #3b82f6 #a855f7 #ec4899])
    {:noreply,
     socket
     |> assign(:random_color, color)
     |> push_event("set-color", %{color: color})}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <.header>
      JS Interop Demo
      <:subtitle>Three patterns for LiveView ↔ JavaScript communication.</:subtitle>
    </.header>

    <%!-- 1. phx-hook: JS Hook ↔ Server --%>
    <section class="mb-8">
      <h2 class="text-lg font-semibold mb-1">1. phx-hook</h2>
      <p class="text-sm text-base-content/60 mb-3">
        A JS Hook calls <code>pushEvent("hook-ping")</code> on mount and every 3 s.
        The server appends each message to the log below.
      </p>
      <div id="ping-hook" phx-hook="Ping"></div>
      <div class="bg-base-200 rounded p-3 text-sm font-mono min-h-[80px]">
        <div :if={@hook_log == []}>waiting for first ping…</div>
        <div :for={entry <- @hook_log}>{entry}</div>
      </div>
    </section>

    <%!-- 2. push_event: Server → JS Hook --%>
    <section class="mb-8">
      <h2 class="text-lg font-semibold mb-1">2. push_event (server → JS)</h2>
      <p class="text-sm text-base-content/60 mb-3">
        Clicking the button triggers a server event that calls <code>push_event/3</code>.
        The JS Hook listens with <code>handleEvent</code> and updates the swatch.
      </p>
      <div class="flex items-center gap-4">
        <.button phx-click="push-color">Send random color</.button>
        <div
          id="color-swatch"
          phx-hook="ColorSwatch"
          class="w-12 h-12 rounded-lg border border-base-300 transition-colors duration-300"
          style={"background-color: #{@random_color}"}
        >
        </div>
        <code class="text-sm">{@random_color}</code>
      </div>
    </section>

    <%!-- 3. JS commands: client-side only --%>
    <section class="mb-8">
      <h2 class="text-lg font-semibold mb-1">3. JS commands (client-side)</h2>
      <p class="text-sm text-base-content/60 mb-3">
        <code>JS.toggle()</code> and <code>JS.dispatch()</code> run entirely on the client — no round-trip needed.
        The box listens for the <code>my-app:flash</code> DOM event and flashes yellow.
      </p>
      <div class="flex gap-2 mb-3">
        <.button phx-click={JS.toggle(to: "#js-box")}>Toggle (JS.toggle)</.button>
        <.button phx-click={JS.dispatch("my-app:flash", to: "#js-box")}>
          Dispatch custom event
        </.button>
      </div>
      <div
        id="js-box"
        phx-hook="FlashBox"
        class="w-full h-20 bg-primary/20 border border-primary rounded-lg flex items-center justify-center font-semibold transition-colors duration-500"
      >
        I respond to JS commands &amp; custom DOM events
      </div>
    </section>
    """
  end
end
