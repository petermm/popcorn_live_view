import "phoenix_html";
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";
import PopcornTransport from "./popcorn_transport.js";

// JS Interop Demo hooks
const Hooks = {};

// Ping hook: pushes an event to the server on mount, then every 3 seconds
Hooks.Ping = {
  mounted() {
    this.send();
    this.timer = setInterval(() => this.send(), 3000);
  },
  destroyed() {
    clearInterval(this.timer);
  },
  send() {
    this.pushEvent("hook-ping", { msg: `ping at ${new Date().toLocaleTimeString()}` }, () => {});
  },
};

// ColorSwatch hook: listens for push_event("set-color") from the server
Hooks.ColorSwatch = {
  mounted() {
    this.handleEvent("set-color", ({ color }) => {
      this.el.style.backgroundColor = color;
    });
  },
};

// FlashBox hook: listens for the custom "my-app:flash" DOM event and flashes yellow
Hooks.FlashBox = {
  mounted() {
    this.el.addEventListener("my-app:flash", () => {
      this.el.classList.add("!bg-warning", "text-warning-content");
      setTimeout(() => {
        this.el.classList.remove("!bg-warning", "text-warning-content");
      }, 500);
    });
  },
};

async function setup() {
  console.log("[WasmLiveView] Initializing Popcorn...");

  // Popcorn is loaded dynamically (external to the bundle, served from /wasm/)
  const { Popcorn } = await import("/wasm/popcorn.js");

  const popcorn = await Popcorn.init({
    bundlePath: "/wasm/bundle.avm",
    wasmDir: "/wasm/",
    onStdout: (msg) => console.log("[WASM stdout]", msg),
    onStderr: (msg) => console.error("[WASM stderr]", msg),
  });

  console.log("[WasmLiveView] Popcorn ready, setting up LiveSocket...");

  // Make Popcorn instance available to the transport
  PopcornTransport.setPopcornInstance(popcorn);

  // Create stub DOM element for LiveView to attach to.
  // The session is a JSON map (not a signed token) - our modified
  // LiveView.Channel accepts this when connect_info[:popcorn] is set.
  const viewId = "phx-wasm-1";
  const path = window.location.pathname || "/";
  const session = JSON.stringify({
    path: path,
    id: viewId,
  });

  const container = document.getElementById("app");
  container.innerHTML = `
    <div id="${viewId}"
         data-phx-main
         data-phx-session='${session}'
         data-phx-static="">
    </div>
  `;

  // Create LiveSocket with our custom transport
  const liveSocket = new LiveSocket("/live", Socket, {
    transport: PopcornTransport,
    params: { _csrf_token: "unused" },
    hooks: Hooks,
  });

  liveSocket.connect();

  console.log("[WasmLiveView] LiveSocket connected");

  // Expose for debugging
  window.liveSocket = liveSocket;
  window.popcorn = popcorn;
}

setup().catch((err) => {
  console.error("[WasmLiveView] Setup failed:", err);
});
