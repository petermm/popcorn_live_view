import "phoenix_html";
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";
import PopcornTransport from "./popcorn_transport.js";
import { hooks as colocatedHooks } from "phoenix-colocated/wasm_live_view";

// Derive site root from app.js location (handles GitHub Pages subpath).
const BASE_URL = new URL("..", import.meta.url).href;
const BASE_PATH = new URL(BASE_URL).pathname.replace(/\/$/, "");

// JS Interop Demo hooks
const Hooks = { ...colocatedHooks };

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

// WokwiEmbed hook: embeds the Wokwi ESP32 simulator and bridges serial monitor output.
// @wokwi/client is loaded dynamically from jsDelivr CDN at runtime.
const WOKWI_CLIENT_CDN =
  "https://cdn.jsdelivr.net/npm/@wokwi/client@0.26.0/+esm";

const wokwiDefaultDiagram = JSON.stringify({
  version: 1,
  author: "WasmLiveView",
  editor: "wokwi",
  serialMonitor: { display: "never" },
  parts: [
    {
      type: "board-esp32-devkit-c-v4",
      id: "esp",
      top: 0,
      left: 0,
      attrs: {},
    },
    {
      type: "wokwi-led",
      id: "led1",
      top: 25.2,
      left: 138.2,
      attrs: { color: "red", flip: "" },
    },
    {
      type: "wokwi-resistor",
      id: "r1",
      top: 100.8,
      left: 124.25,
      rotate: 90,
      attrs: { value: "1000" },
    },
    {
      type: "wokwi-pushbutton",
      id: "btn1",
      top: 60,
      left: -90,
      attrs: { color: "blue", label: "BTN", key: "b", bounce: "0" },
    },
  ],
  connections: [
    ["esp:TX", "$serialMonitor:RX", "", []],
    ["esp:RX", "$serialMonitor:TX", "", []],
    ["led1:C", "r1:1", "green", ["v0"]],
    ["esp:GND.3", "r1:2", "black", ["h33.64", "v57.6", "h19.2"]],
    ["esp:2", "led1:A", "green", ["h0"]],
    ["btn1:1.r", "esp:4", "blue", ["h0", "v0"]],
    ["btn1:2.r", "esp:GND.1", "black", ["h0", "v19.2"]],
  ],
  dependencies: {},
});

Hooks.WokwiEmbed = {
  async mounted() {
    this.flashUploadSeq = 0;
    this.cachedFlashSections = {};
    // Create and mount the Wokwi iframe.
    // experimental/embed blocks framing; experimental/viewer?api=1 allows it and
    // supports the full @wokwi/client MessagePort API.
    const iframe = document.createElement("iframe");
    //iframe.src = "https://wokwi.com/experimental/viewer";

    iframe.src =
      "https://wokwi.com/experimental/embed?client_id=wokwi_client_omf4ejkz6n6twj7d3x2eqmyp";
    //+
    //  "?api=1" +
    //  "&diagram=https%3A%2F%2Fdocs.espressif.com%2Fprojects%2Farduino-esp32-wokwi-test%2Fen%2Fdocs-embed%2F_static%2Fbinaries%2Flibraries%2FESP32%2Fexamples%2FAnalogRead%2Fesp32%2Fdiagram.esp32.json" +
    //  "&firmware=https%3A%2F%2Fdocs.espressif.com%2Fprojects%2Farduino-esp32-wokwi-test%2Fen%2Fdocs-embed%2F_static%2Fbinaries%2Flibraries%2FESP32%2Fexamples%2FAnalogRead%2Fesp32%2FAnalogRead.ino.merged.bin";
    iframe.style.cssText = "width:100%;height:100%;border:none;";
    iframe.setAttribute("loading", "lazy");
    iframe.setAttribute("credentialless", "");
    iframe.referrerPolicy = "no-referrer";
    this.el.appendChild(iframe);
    this.wokwiIframe = iframe;
    this.wokwiClient = null;

    // Server pushes "wokwi-flash" when user clicks Flash & Run
    this.handleEvent("wokwi-flash", async ({ flasher_args, flash_files }) => {
      if (!this.wokwiClient) return;
      try {
        const flasherArgs = flasher_args || {};
        const flashFiles = flash_files || {};
        const flashMap = flasherArgs.flash_files || {};
        const flashSections = [];
        const runId = `${Date.now()}-${this.flashUploadSeq++}`;

        for (const [offsetHex, sourceName] of Object.entries(flashMap)) {
          const offsetKey = offsetHex.toLowerCase();
          if (
            sourceName !== "main.avm" &&
            this.cachedFlashSections[offsetKey]
          ) {
            flashSections.push({
              offset: parseInt(offsetHex, 16),
              file: this.cachedFlashSections[offsetKey],
            });
            continue;
          }

          let bytes;
          const content = flashFiles[sourceName];
          if (typeof content === "string") {
            // Backward-compat: base64 string payload.
            bytes = Uint8Array.from(atob(content), (c) => c.charCodeAt(0));
          } else if (Array.isArray(content)) {
            // Preferred: raw byte list from LiveView push_event.
            bytes = Uint8Array.from(content);
          } else if (content instanceof Uint8Array) {
            bytes = content;
          } else if (content instanceof ArrayBuffer) {
            bytes = new Uint8Array(content);
          } else {
            const firmwareUrl = `${BASE_PATH}/wokwi/firmware/${sourceName}`;
            const resp = await fetch(firmwareUrl);
            if (!resp.ok) {
              throw new Error(`failed to fetch ${firmwareUrl}: ${resp.status}`);
            }
            bytes = new Uint8Array(await resp.arrayBuffer());
          }

          const uploadName = `flash-${offsetHex}-${runId}.bin`;
          await this.wokwiClient.fileUpload(uploadName, bytes);
          if (sourceName !== "main.avm") {
            this.cachedFlashSections[offsetKey] = uploadName;
          }
          flashSections.push({
            offset: parseInt(offsetHex, 16),
            file: uploadName,
          });
        }

        this.wokwiClient.simStart({
          firmware: flashSections,
          flashSize: flasherArgs.flash_settings?.flash_size,
        });
      } catch (err) {
        console.error("[WokwiEmbed] AVM upload failed:", err);
        this.pushEvent("serial-output", {
          text: `\n[wokwi upload error] ${String(err)}\n`,
        });
      }
    });

    this.handleEvent("download-avm", ({ filename, bytes }) => {
      const data = Array.isArray(bytes)
        ? Uint8Array.from(bytes)
        : new Uint8Array();
      const blob = new Blob([data], { type: "application/octet-stream" });
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = filename || "main.avm";
      document.body.appendChild(a);
      a.click();
      a.remove();
      URL.revokeObjectURL(url);
    });

    // The Wokwi iframe posts a MessagePort when ready
    const handleMessage = async (event) => {
      if (event.origin !== "https://wokwi.com") return;
      if (!this.wokwiIframe || event.source !== this.wokwiIframe.contentWindow)
        return;
      if (!event.data || !event.data.port) return;

      const { MessagePortTransport, APIClient } = await import(
        /* @vite-ignore */ WOKWI_CLIENT_CDN
      );
      const transport = new MessagePortTransport(event.data.port);
      const client = new APIClient(transport);
      this.wokwiClient = client;

      await client.connected;

      client.onConnected = async () => {
        this.pushEvent("wokwi-connected", {});
        await client.serialMonitorListen();
        const serialDisplay = this.el.dataset.serialMonitor || "never";
        const diagram = JSON.parse(wokwiDefaultDiagram);
        diagram.serialMonitor = { display: serialDisplay };
        await client.fileUpload("diagram.json", JSON.stringify(diagram));
      };

      client.listen("serial-monitor:data", (evt) => {
        const bytes = new Uint8Array(evt.payload.bytes);
        const text = new TextDecoder().decode(bytes);
        this.pushEvent("serial-output", { text });
      });

      client.onError = (err) => console.error("[WokwiEmbed] error:", err);
    };

    window.addEventListener("message", handleMessage);
    this._handleMessage = handleMessage;
  },

  destroyed() {
    if (this._handleMessage) {
      window.removeEventListener("message", this._handleMessage);
      this._handleMessage = null;
    }
    this.wokwiIframe = null;
    this.wokwiClient = null;
    this.cachedFlashSections = {};
  },
};

Hooks.WokwiSerialAutoScroll = {
  mounted() {
    this._raf = null;
    this._scrollToBottom();
  },

  updated() {
    this._scrollToBottom();
  },

  destroyed() {
    if (this._raf != null) {
      cancelAnimationFrame(this._raf);
      this._raf = null;
    }
  },

  _scrollToBottom() {
    if (this._raf != null) cancelAnimationFrame(this._raf);
    this._raf = requestAnimationFrame(() => {
      this.el.scrollTop = this.el.scrollHeight;
      this._raf = null;
    });
  },
};

// IexTerminal hook: renders a ghostty-web terminal and bridges tty_data push_events.
// ghostty-web is a drop-in xterm.js replacement backed by Ghostty's VT parser (WASM).
// The ES module auto-resolves its ghostty-vt.wasm via import.meta.url from jsDelivr.
const GHOSTTY_CDN =
  "https://cdn.jsdelivr.net/npm/ghostty-web@0.4.0/dist/ghostty-web.js";
let ghosttyModule = null;

// Singleton terminal state — persisted across LiveView mount/destroy cycles so
// navigating away and back keeps the terminal exactly as the user left it.
let iexTerm = null; // ghostty Terminal instance (never disposed)
let iexTermEl = null; // wrapper div that holds the terminal's DOM
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
    iexTerm.focus();

    // History — AtomVM's edlin doesn't handle arrow-key history.
    // Persisted in localStorage so it survives page refreshes.
    this.histIdx = -1; // -1 = not navigating
    this.curInput = ""; // chars typed since last Enter
    this.savedInput = ""; // stashed when history nav starts
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
        if (this.curInput.length > 0)
          this.curInput = this.curInput.slice(0, -1);
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
        Uint8Array.from(atob(data), (c) => c.charCodeAt(0)),
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
      return; // already at oldest entry
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

(window.__sqliteReady || Promise.resolve())
  .then(() => setup())
  .catch((err) => {
    console.error("[WasmLiveView] Setup failed:", err);
  });
