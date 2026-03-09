defmodule WasmLiveView.IexLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :terminal_app}

  import WasmLiveViewWeb.CoreComponents

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, current_route: :iex, shell_pid: nil)}
  end

  # JS hook fires this once the terminal is ready to display output.
  # Reuses the existing IexShell across navigations (terminal state is preserved
  # by the JS hook); only starts a fresh shell on first visit.
  @impl true
  def handle_event("terminal-ready", _params, socket) do
    socket =
      case Process.whereis(:iex_shell) do
        nil ->
          {:ok, shell_pid} = WasmLiveView.IexShell.start(lv_pid: self())
          assign(socket, shell_pid: shell_pid)

        shell_pid ->
          :ok = WasmLiveView.IexShell.reconnect(self())
          assign(socket, shell_pid: shell_pid)
      end

    {:noreply, socket}
  end

  @impl true
  def handle_event("clear-history", _params, socket) do
    {:noreply, push_event(socket, "clear-history", %{})}
  end

  @impl true
  def handle_event("send-input", %{"data" => data}, socket) do
    if pid = socket.assigns.shell_pid do
      WasmLiveView.IexShell.send_input(pid, data)
    end

    {:noreply, socket}
  end

  @impl true
  def handle_info({:tty_data, data}, socket) do
    {:noreply, push_event(socket, "tty-data", %{data: Base.encode64(data)})}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <.header>
      IEX
      <:subtitle>
        Interactive Elixir shell running in WASM. Adapted from
        <a href="https://github.com/software-mansion/popcorn/tree/main/examples/iex_wasm" target="_blank" rel="noopener noreferrer" class="link link-primary">software-mansion/popcorn iex_wasm</a>.
      </:subtitle>
    </.header>

    <div
      id="iex-terminal"
      phx-hook=".IexTerminal"
      phx-update="ignore"
      class="w-full rounded-lg overflow-hidden border border-base-300"
      style="height: 500px; background: #1e1e1e;"
    >
    </div>

    <div class="mt-2 flex justify-end">
      <button phx-click="clear-history" class="btn btn-xs btn-ghost text-base-content/50">
        Clear history
      </button>
    </div>

    <div class="mt-1 text-sm text-base-content/60 space-y-1">
      <p>
        Runs on <strong>AtomVM</strong> — a lightweight BEAM implementation.
        Most basic Elixir expressions work. HTTP via <code>Req</code> is
        available — <code>WasmFetchAdapter</code> is set as the default adapter
        at startup, so <code>Req.get!("https://…")</code> works directly.
      </p>
      <p>
        Arrow-up/down history is implemented in the JS hook (AtomVM's
        <code>edlin</code> doesn't handle escape sequences). Up to 200 entries,
        persisted in <code>localStorage</code>.
      </p>
    </div>

    <script :type={Phoenix.LiveView.ColocatedHook} name=".IexTerminal">
    const GHOSTTY_CDN =
      "https://cdn.jsdelivr.net/npm/ghostty-web@0.4.0/dist/ghostty-web.js";
    let ghosttyModule = null;

    // Singleton terminal state — persisted across LiveView mount/destroy cycles so
    // navigating away and back keeps the terminal exactly as the user left it.
    let iexTerm = null;
    let iexTermEl = null;
    let iexDataDisposable = null;

    export default {
      async mounted() {
        if (!ghosttyModule) {
          ghosttyModule = await import(/* @vite-ignore */ GHOSTTY_CDN);
          await ghosttyModule.init();
        }
        const { Terminal } = ghosttyModule;

        if (!iexTerm) {
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

        this.el.innerHTML = "";
        iexTermEl.style.display = "";
        this.el.appendChild(iexTermEl);
        this.term = iexTerm;
        iexTerm.focus();

        this.histIdx = -1;
        this.curInput = "";
        this.savedInput = "";
        try {
          this.history = JSON.parse(localStorage.getItem("iex_history") || "[]");
        } catch {
          this.history = [];
        }

        if (iexDataDisposable) iexDataDisposable.dispose();
        iexDataDisposable = iexTerm.onData((data) => {
          if (data === "\x1b[A" || data === "\x1bOA") {
            this._histBack();
            return;
          }
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

        this.handleEvent("clear-history", () => {
          this.history = [];
          this.histIdx = -1;
          this.savedInput = "";
          localStorage.removeItem("iex_history");
        });

        this.pushEvent("terminal-ready", {});

        const writeTty = (data) => {
          const text = new TextDecoder().decode(
            Uint8Array.from(atob(data), (c) => c.charCodeAt(0)),
          );
          const CHUNK = 512;
          for (let i = 0; i < text.length; i += CHUNK) {
            iexTerm.write(text.slice(i, i + CHUNK));
          }
        };

        this.handleEvent("tty-data", ({ data }) => writeTty(data));
      },

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
          return;
        }
        this._replaceInput(this.history[this.histIdx]);
      },

      _histForward() {
        if (this.histIdx === -1) return;
        if (this.histIdx < this.history.length - 1) {
          this.histIdx++;
          this._replaceInput(this.history[this.histIdx]);
        } else {
          this.histIdx = -1;
          this._replaceInput(this.savedInput);
          this.savedInput = "";
        }
      },

      destroyed() {
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
    </script>
    """
  end
end
