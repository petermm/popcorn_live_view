import "phoenix_html";
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";
import PopcornTransport from "./popcorn_transport.js";

// Derive site root from app.js location (handles GitHub Pages subpath).
const BASE_URL = new URL("..", import.meta.url).href;
const BASE_PATH = new URL(BASE_URL).pathname.replace(/\/$/, "");

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

// OPFS helpers: load/save the entire SQLite DB binary
async function opfsLoad() {
  try {
    const root = await navigator.storage.getDirectory();
    const fh = await root.getFileHandle("popcorn_notes.sqlite");
    const file = await fh.getFile();
    return new Uint8Array(await file.arrayBuffer());
  } catch {
    return null; // file doesn't exist yet
  }
}

async function opfsSave(db) {
  const data = db.export();
  const root = await navigator.storage.getDirectory();
  const fh = await root.getFileHandle("popcorn_notes.sqlite", { create: true });
  const writable = await fh.createWritable();
  await writable.write(data);
  await writable.close();
}

async function setupSQLite() {
  // sql-wasm.js is a UMD script — load as classic script to set window.initSqlJs
  await new Promise((resolve, reject) => {
    const s = document.createElement("script");
    s.src = BASE_URL + "sql-wasm.js";
    s.onload = resolve;
    s.onerror = reject;
    document.head.appendChild(s);
  });

  const SQL = await window.initSqlJs({ locateFile: () => BASE_URL + "sql-wasm.wasm" });

  // Load existing DB from OPFS if present, otherwise start fresh
  const existing = await opfsLoad();
  const db = existing ? new SQL.Database(existing) : new SQL.Database();

  db.run(`CREATE TABLE IF NOT EXISTS notes (
    id INTEGER PRIMARY KEY,
    title TEXT NOT NULL,
    body TEXT,
    inserted_at INTEGER NOT NULL,
    updated_at INTEGER NOT NULL
  )`);

  window.__sqliteDB = db;
  // Fire-and-forget save — called by Elixir after each write via run_js
  window.__sqliteSave = () => opfsSave(db).catch(console.error);
  console.log("[WasmLiveView] SQLite ready, persisted via OPFS");
}

async function setup() {
  console.log("[WasmLiveView] Initializing Popcorn...");

  // Popcorn is loaded dynamically (external to the bundle, served from /wasm/)
  const { Popcorn } = await import(/* @vite-ignore */ BASE_URL + "wasm/popcorn.js");

  const popcorn = await Popcorn.init({
    bundlePath: new URL(BASE_URL).pathname.slice(1) + "wasm/bundle.avm",
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

setupSQLite()
  .then(() => setup())
  .catch((err) => {
    console.error("[WasmLiveView] Setup failed:", err);
  });
