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
    this.pushEvent(
      "hook-ping",
      { msg: `ping at ${new Date().toLocaleTimeString()}` },
      () => {},
    );
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

// EaselCanvas hook: renders Easel ops on a <canvas> element
Hooks.EaselCanvas = {
  executeOps(ops) {
    const ctx = this.context;
    for (const [op, args] of ops) {
      if (op === "set") {
        ctx[args[0]] = args[1];
      } else if (typeof ctx[op] === "function") {
        ctx[op](...args);
      }
    }
  },
  draw() {
    const dpr = window.devicePixelRatio || 1;
    const w = parseInt(this.el.getAttribute("width")) || this.el.width;
    const h = parseInt(this.el.getAttribute("height")) || this.el.height;
    if (this.el.width !== w * dpr || this.el.height !== h * dpr) {
      this.el.width = w * dpr;
      this.el.height = h * dpr;
      this.el.style.width = w + "px";
      this.el.style.height = h + "px";
    }
    const ctx = this.context;
    ctx.setTransform(1, 0, 0, 1, 0, 0);
    ctx.clearRect(0, 0, this.el.width, this.el.height);
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    const ops = JSON.parse(this.el.dataset.ops || "[]");
    if (ops.length > 0) this.executeOps(ops);
  },
  mounted() {
    this.context = this.el.getContext("2d");
    this.draw();
  },
  updated() {
    requestAnimationFrame(() => this.draw());
  },
};

// Geolocation hook: gets browser geolocation when triggered
Hooks.Geolocation = {
  mounted() {
    this.handleEvent("check-permission", () => {
      if (!navigator.permissions) return;
      navigator.permissions
        .query({ name: "geolocation" })
        .then((result) => {
          if (result.state !== "denied") {
            this.pushEvent("permission-result", { granted: true });
          }
        })
        .catch(() => {});
    });

    this.handleEvent("request-location", () => {
      if (!navigator.geolocation) {
        this.pushEvent("location-error", {
          error: "Geolocation not supported",
        });
        return;
      }

      navigator.geolocation.getCurrentPosition(
        (position) => {
          this.pushEvent("location-found", {
            lat: position.coords.latitude,
            lon: position.coords.longitude,
          });
        },
        (err) => {
          this.pushEvent("location-error", { error: err.message });
        },
        { enableHighAccuracy: true, timeout: 10000 },
      );
    });
  },
};

// IexTerminal hook: renders a ghostty-web terminal and bridges tty_data push_events.
// ghostty-web is a drop-in xterm.js replacement backed by Ghostty's VT parser (WASM).
// The ES module auto-resolves its ghostty-vt.wasm via import.meta.url from jsDelivr.
const GHOSTTY_CDN = "https://cdn.jsdelivr.net/npm/ghostty-web@0.4.0/dist/ghostty-web.js";
let ghosttyModule = null;

// Singleton terminal state — persisted across LiveView mount/destroy cycles so
// navigating away and back keeps the terminal exactly as the user left it.
let iexTerm = null;         // ghostty Terminal instance (never disposed)
let iexTermEl = null;       // wrapper div that holds the terminal's DOM
let iexDataDisposable = null; // current onData listener (replaced on each mount)

Hooks.IexTerminal = {
  async mounted() {
    if (!ghosttyModule) {
      ghosttyModule = await import(/* @vite-ignore */ GHOSTTY_CDN);
      await ghosttyModule.init();
    }
    const { Terminal } = ghosttyModule;

    if (!iexTerm) {
      // First mount: create the terminal and a persistent wrapper div.
      iexTermEl = document.createElement("div");
      iexTermEl.style.cssText = "width:100%;height:100%;";
      iexTerm = new Terminal({
        cursorBlink: true,
        scrollback: 5000,
        theme: {
          background: "#1e1e1e",
          foreground: "#d4d4d4",
          cursor: "#d4d4d4",
        },
      });
      iexTerm.open(iexTermEl);
    }

    // Move terminal into the hook element (was parked on body while away).
    this.el.innerHTML = "";
    iexTermEl.style.display = "";
    this.el.appendChild(iexTermEl);
    this.term = iexTerm;

    // History — AtomVM's edlin doesn't handle arrow-key history.
    // Persisted in localStorage so it survives page refreshes.
    this.histIdx = -1;      // -1 = not navigating
    this.curInput = "";     // chars typed since last Enter
    this.savedInput = "";   // stashed when history nav starts
    try {
      this.history = JSON.parse(localStorage.getItem("iex_history") || "[]");
    } catch {
      this.history = [];
    }

    // Replace the previous onData listener so pushEvent targets this socket.
    if (iexDataDisposable) iexDataDisposable.dispose();
    iexDataDisposable = iexTerm.onData((data) => {
      // Arrow up: \x1b[A (normal cursor mode) or \x1bOA (application cursor mode).
      // IEx may switch the terminal to application mode during init.
      if (data === "\x1b[A" || data === "\x1bOA") {
        this._histBack();
        return;
      }
      // Arrow down: same dual-sequence reasoning.
      if (data === "\x1b[B" || data === "\x1bOB") {
        this._histForward();
        return;
      }

      if (data === "\r") {
        const cmd = this.curInput.trim();
        if (cmd && this.history[this.history.length - 1] !== cmd) {
          this.history.push(cmd);
          if (this.history.length > 200) this.history.shift();
          localStorage.setItem("iex_history", JSON.stringify(this.history));
        }
        this.curInput = "";
        this.histIdx = -1;
        this.savedInput = "";
      } else if (data === "\x7f" || data === "\b") {
        if (this.curInput.length > 0) this.curInput = this.curInput.slice(0, -1);
      } else if (!data.startsWith("\x1b")) {
        this.curInput += data;
      }

      this.pushEvent("send-input", { data });
    });

    // Clear in-memory and persisted history on demand.
    this.handleEvent("clear-history", () => {
      this.history = [];
      this.histIdx = -1;
      this.savedInput = "";
      localStorage.removeItem("iex_history");
    });

    // Tell the server the terminal is ready — it starts or reconnects IexShell.
    this.pushEvent("terminal-ready", {});

    // Decode base64 tty bytes and write to the terminal in chunks.
    // ghostty-web has a WASM allocator bug where large single writes cause
    // "offset is out of bounds" after memory has grown. Chunking avoids it.
    const writeTty = (data) => {
      const text = new TextDecoder().decode(
        Uint8Array.from(atob(data), (c) => c.charCodeAt(0))
      );
      const CHUNK = 512;
      for (let i = 0; i < text.length; i += CHUNK) {
        iexTerm.write(text.slice(i, i + CHUNK));
      }
    };

    // Live terminal output from the server.
    this.handleEvent("tty-data", ({ data }) => writeTty(data));
  },

  // Replace the current IEx input line with newCmd.
  // Send exactly curInput.length backspaces — IEx erases only the typed input
  // (not the prompt) and echoes the deletion back to the terminal.
  // Never write to this.term directly; IEx owns the prompt display.
  _replaceInput(newCmd) {
    const bsp = "\x7f".repeat(this.curInput.length);
    this.curInput = newCmd;
    this.pushEvent("send-input", { data: bsp + newCmd });
  },

  _histBack() {
    if (this.history.length === 0) return;
    if (this.histIdx === -1) {
      this.savedInput = this.curInput;
      this.histIdx = this.history.length - 1;
    } else if (this.histIdx > 0) {
      this.histIdx--;
    } else {
      return;  // already at oldest entry
    }
    this._replaceInput(this.history[this.histIdx]);
  },

  _histForward() {
    if (this.histIdx === -1) return;
    if (this.histIdx < this.history.length - 1) {
      this.histIdx++;
      this._replaceInput(this.history[this.histIdx]);
    } else {
      // Past the newest entry → restore what the user was typing
      this.histIdx = -1;
      this._replaceInput(this.savedInput);
      this.savedInput = "";
    }
  },

  destroyed() {
    // Park the terminal on body (hidden) instead of disposing it.
    // The next mount will move it back — preserving all terminal state.
    if (iexTermEl) {
      iexTermEl.style.display = "none";
      document.body.appendChild(iexTermEl);
    }
    if (iexDataDisposable) {
      iexDataDisposable.dispose();
      iexDataDisposable = null;
    }
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

  const SQL = await window.initSqlJs({
    locateFile: () => BASE_URL + "sql-wasm.wasm",
  });

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
  const { Popcorn } = await import(
    /* @vite-ignore */ BASE_URL + "wasm/popcorn.js"
  );

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
