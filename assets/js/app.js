import "phoenix_html";
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";
import PopcornTransport from "./popcorn_transport.js";
import { ErlangCodeEditor } from "./erlang_code_editor.js";
import { hooks as colocatedHooks } from "phoenix-colocated/wasm_live_view";

// Derive site root from app.js location (handles GitHub Pages subpath).
const BASE_URL = new URL("..", import.meta.url).href;
const BASE_PATH = new URL(BASE_URL).pathname.replace(/\/$/, "");

const Hooks = {
  ...colocatedHooks,
  ErlangCodeEditor,
};

async function setup() {
  console.log("[WasmLiveView] Initializing Popcorn...");

  // Popcorn JS + AtomVM are served from /wasm/ (copied from @swmansion/popcorn at cook time)
  const { Popcorn } = await import(
    /* @vite-ignore */ BASE_URL + "wasm/popcorn.mjs"
  );

  // Absolute path so GH Pages subpaths resolve correctly (Popcorn 0.3 uses bundlePaths).
  const bundlePath = new URL("wasm/bundle.avm", BASE_URL).pathname;

  const popcorn = await Popcorn.init({
    bundlePaths: [bundlePath],
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
  const path = window.location.pathname.slice(BASE_PATH.length) || "/";
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

(window.__sqliteReady || Promise.resolve())
  .then(() => setup())
  .catch((err) => {
    console.error("[WasmLiveView] Setup failed:", err);
  });
