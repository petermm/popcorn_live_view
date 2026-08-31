import "phoenix_html";
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";
import { Popcorn } from "@swmansion/popcorn";
import PopcornTransport from "./popcorn_transport.js";
import { ErlangCodeEditor } from "./erlang_code_editor.js";
import { hooks as colocatedHooks } from "phoenix-colocated/wasm_live_view";

const BASE_URL = new URL("..", import.meta.url).href;
const BASE_PATH = new URL(BASE_URL).pathname.replace(/\/$/, "");

const Hooks = {
  ...colocatedHooks,
  ErlangCodeEditor,
};

async function setup() {
  console.log("[WasmLiveView] Initializing Popcorn (OTP/BEAM wasm)...");

  const result = await Popcorn.init({
    beam: {
      otpAssetsRoot: new URL("./otp/", import.meta.url).href,
    },
    onStdout: (msg) => console.log("[WASM stdout]", msg),
    onStderr: (msg) => console.error("[WASM stderr]", msg),
    onError: (event) => console.error("[WASM error]", event),
  });

  if (!result.ok) {
    console.error("[WasmLiveView] Popcorn failed to boot:", result.error);
    throw result.error;
  }

  const popcorn = result.data;
  console.log("[WasmLiveView] Popcorn ready, setting up LiveSocket...");

  PopcornTransport.setPopcornInstance(popcorn);

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

  const liveSocket = new LiveSocket("/live", Socket, {
    transport: PopcornTransport,
    params: { _csrf_token: "unused" },
    hooks: Hooks,
  });

  liveSocket.connect();

  console.log("[WasmLiveView] LiveSocket connected");

  window.liveSocket = liveSocket;
  window.popcorn = popcorn;
}

(window.__sqliteReady || Promise.resolve())
  .then(() => setup())
  .catch((err) => {
    console.error("[WasmLiveView] Setup failed:", err);
  });
