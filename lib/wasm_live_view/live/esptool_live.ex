defmodule WasmLiveView.EsptoolLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :terminal_app}

  @flash_address 0x210000

  @impl true
  def mount(params, session, socket) do
    case WasmLiveView.WokwiLive.mount(params, session, socket) do
      {:ok, socket} ->
        {:ok,
         assign(socket,
           current_route: :esptool,
           screen_title: "ESPTool ESP32 AtomVM"
         )}

      other ->
        other
    end
  end

  defdelegate handle_event(event, params, socket), to: WasmLiveView.WokwiLive

  @impl true
  def handle_info({:packbeam_result, :flash, {:ok, avm_binary}}, socket)
      when is_binary(avm_binary) do
    {:noreply,
     socket
     |> assign(packing: false)
     |> push_event("esptool-flash", %{
       filename: "main.avm",
       bytes: :binary.bin_to_list(avm_binary),
       address: @flash_address
     })}
  end

  def handle_info(message, socket), do: WasmLiveView.WokwiLive.handle_info(message, socket)

  @impl true
  def render(assigns) do
    ~H"""
    <div
      id="esptool-actions"
      phx-hook=".EsptoolActions"
      class="grid gap-4 h-full min-h-0"
      style="grid-template-rows: auto minmax(0, 3fr) minmax(0, 2fr);"
    >
      <div class="flex items-center justify-between">
        <h1 class="text-xl font-bold">{@screen_title}</h1>
        <span class="badge badge-info">Serial Monitor Only</span>
      </div>

      <div class="card bg-base-200 shadow min-h-0 h-full">
        <div class="card-body p-4 flex flex-col min-h-0 h-full">
          <h2 class="card-title text-sm mb-4">AtomVM Erlang Code</h2>
          <div class="flex gap-6 items-start">
            <div class="tabs tabs-lifted tabs-vertical gap-1">
              <%= for example <- Map.keys(@code_examples) do %>
                <button
                  phx-click="select-example"
                  phx-value-example={example}
                  class={[
                    "tab tab-lg font-semibold",
                    if(@current_example == example,
                      do: "tab-active bg-base-100",
                      else: "hover:bg-base-300"
                    )
                  ]}
                >
                  {String.capitalize(example)}
                </button>
              <% end %>
            </div>
          </div>
          <form phx-change="code-changed" class="flex flex-col flex-1 min-h-0">
            <textarea
              name="code"
              class="textarea textarea-bordered font-mono text-xs w-full flex-1 min-h-0"
              style="resize: none; height: 100%;"
              phx-debounce="500"
            >{@code}</textarea>
          </form>
          <div class="flex justify-end gap-2 mt-2">
            <button
              class={["btn btn-sm btn-ghost", if(@packing, do: "btn-disabled")]}
              disabled={@packing}
              phx-click="download-avm"
            >
              Download AVM
            </button>
            <button
              data-role="flash-to-device"
              data-idle-label="Flash to Device"
              data-busy-label="Flashing Device..."
              class={["btn btn-sm btn-primary", if(@packing, do: "btn-disabled")]}
              disabled={@packing}
              phx-click="flash"
            >
              {if @packing, do: "Packing AVM...", else: "Flash to Device"}
            </button>
          </div>
        </div>
      </div>

      <div class="card bg-base-200 shadow min-h-0 h-full">
        <div class="card-body p-4 flex flex-col min-h-0">
          <div class="flex items-center justify-between mb-2">
            <h2 class="card-title text-sm">Serial Monitor</h2>
            <button class="btn btn-xs btn-ghost" phx-click="clear-output">Clear</button>
          </div>
          <div
            id="esptool-serial-output"
            phx-hook=".EsptoolSerialAutoScroll"
            class="bg-base-300 rounded-lg p-3 overflow-auto font-mono text-xs flex-1 min-h-0"
          >
            <pre class="text-success whitespace-pre-wrap break-all">{if @output == "", do: "Waiting for esptool output...", else: @output}</pre>
          </div>
        </div>
      </div>
    </div>

    <script :type={Phoenix.LiveView.ColocatedHook} name=".EsptoolActions">
    const ESP32TOOL_CDN =
      "https://cdn.jsdelivr.net/npm/esp32tool@1.6.3/js/modules/esptool.js";
    const DEFAULT_FLASH_ADDRESS = 0x210000;

    export default {
      async mounted() {
        this.esp32tool = null;
        this.flashInProgress = false;
        this.flashButton = this.el.querySelector('[data-role="flash-to-device"]');
        this.lastProgressBucket = -1;

        try {
          const mod = await import(/* @vite-ignore */ ESP32TOOL_CDN);
          this.esp32tool = mod;
          this.pushEvent("serial-output", {
            text:
              `\n[esptool] Loaded browser entry from CDN: ${ESP32TOOL_CDN}\n` +
              `[esptool] connect() ${typeof mod.connect === "function" ? "is available" : "is missing"}\n`,
          });
        } catch (err) {
          console.error("[EsptoolActions] failed to load esp32tool:", err);
          this.pushEvent("serial-output", {
            text: `\n[esptool] Failed to load esp32tool from CDN: ${String(err)}\n`,
          });
        }

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

        this.handleEvent("esptool-flash", async ({ filename, bytes, address }) => {
          if (this.flashInProgress) {
            this._appendSerialOutput("[esptool] Flash already in progress.");
            return;
          }

          if (!this.esp32tool?.connect) {
            this._appendSerialOutput("[esptool] esp32tool is not loaded yet.");
            return;
          }

          const imageBytes = this._toUint8Array(bytes);
          const flashAddress = Number.isFinite(address)
            ? address
            : DEFAULT_FLASH_ADDRESS;

          if (imageBytes.byteLength === 0) {
            this._appendSerialOutput("[esptool] Refusing to flash an empty AVM.");
            return;
          }

          let loader = null;

          this.flashInProgress = true;
          this.lastProgressBucket = -1;
          this._setFlashButtonState(true);
          this._appendSerialOutput(
            `[esptool] Connecting to device to flash ${filename || "main.avm"} ` +
              `to ${this._formatHex(flashAddress)}...`
          );

          try {
            loader = await this.esp32tool.connect(this._buildLogger());
            await loader.initialize();
            loader = await loader.runStub();
            this._appendSerialOutput("[esptool] Starting flash...");

            await loader.flashData(
              imageBytes.buffer,
              (written, total) => this._reportProgress(written, total),
              flashAddress,
              true
            );

            this._appendSerialOutput(
              `[esptool] Flash complete at ${this._formatHex(flashAddress)}.`
            );

            try {
              const portChanged = await loader.resetToFirmware();
              this._appendSerialOutput(
                portChanged
                  ? "[esptool] Device reset requested. Reselect the new USB port if it changed."
                  : "[esptool] Device reset to firmware."
              );
            } catch (resetErr) {
              console.warn("[EsptoolActions] reset failed:", resetErr);
              this._appendSerialOutput(
                `[esptool] Flash succeeded, but reset failed: ${String(resetErr)}`
              );
            }
          } catch (err) {
            console.error("[EsptoolActions] flash failed:", err);
            this._appendSerialOutput(`[esptool] Flash failed: ${String(err)}`);
          } finally {
            this.flashInProgress = false;
            this._setFlashButtonState(false);

            if (loader?.disconnect) {
              try {
                await loader.disconnect();
              } catch (disconnectErr) {
                console.debug(
                  "[EsptoolActions] disconnect after flash failed:",
                  disconnectErr
                );
              }
            }
          }
        });
      },

      _appendSerialOutput(text) {
        const line = text.endsWith("\n") ? text : `${text}\n`;
        this.pushEvent("serial-output", { text: line });
      },

      _buildLogger() {
        return {
          log: (msg) => this._appendSerialOutput(`[esp32tool] ${msg}`),
          error: (msg) => this._appendSerialOutput(`[esp32tool] ${msg}`),
          debug: (msg) => console.debug("[esp32tool]", msg),
        };
      },

      _formatHex(value) {
        return `0x${Number(value).toString(16).toUpperCase()}`;
      },

      _reportProgress(written, total) {
        if (!total) return;

        const bucket = Math.min(100, Math.floor((written / total) * 10) * 10);
        if (bucket <= this.lastProgressBucket) return;

        this.lastProgressBucket = bucket;
        this._appendSerialOutput(
          `[esptool] Flash progress: ${bucket}% (${written}/${total} bytes)`
        );
      },

      _setFlashButtonState(isBusy) {
        if (!this.flashButton) return;

        this.flashButton.disabled = isBusy;
        this.flashButton.classList.toggle("btn-disabled", isBusy);
        this.flashButton.textContent = isBusy
          ? this.flashButton.dataset.busyLabel || "Flashing Device..."
          : this.flashButton.dataset.idleLabel || "Flash to Device";
      },

      _toUint8Array(bytes) {
        if (Array.isArray(bytes)) return Uint8Array.from(bytes);
        if (bytes instanceof Uint8Array) return bytes;
        if (bytes instanceof ArrayBuffer) return new Uint8Array(bytes);
        return new Uint8Array();
      },
    };
    </script>

    <script :type={Phoenix.LiveView.ColocatedHook} name=".EsptoolSerialAutoScroll">
    export default {
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
    </script>
    """
  end
end
