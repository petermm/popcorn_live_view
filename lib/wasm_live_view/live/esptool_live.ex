defmodule WasmLiveView.EsptoolLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :terminal_app}

  @flash_address 0x210000
  @default_flash_mode :flash_only

  @impl true
  def mount(params, session, socket) do
    case WasmLiveView.WokwiLive.mount(params, session, socket) do
      {:ok, socket} ->
        {:ok,
         assign(socket,
           current_route: :esptool,
           screen_title: "ESPTool ESP32 AtomVM",
           flash_mode: @default_flash_mode
         )}

      other ->
        other
    end
  end

  @impl true
  def handle_event("flash", params, socket) do
    socket = assign(socket, :flash_mode, :flash_only)
    WasmLiveView.WokwiLive.handle_event("flash", params, socket)
  end

  @impl true
  def handle_event("flash-with-monitor", _params, socket) do
    socket = assign(socket, :flash_mode, :flash_and_monitor)
    WasmLiveView.WokwiLive.handle_event("flash", %{}, socket)
  end

  def handle_event(event, params, socket),
    do: WasmLiveView.WokwiLive.handle_event(event, params, socket)

  @impl true
  def handle_info({:packbeam_result, :flash, {:ok, avm_binary}}, socket)
      when is_binary(avm_binary) do
    monitor = socket.assigns.flash_mode == :flash_and_monitor

    {:noreply,
     socket
     |> assign(packing: false, flash_mode: @default_flash_mode)
     |> push_event("esptool-flash", %{
       filename: "main.avm",
       bytes: :binary.bin_to_list(avm_binary),
       address: @flash_address,
       monitor: monitor
     })}
  end

  def handle_info({:packbeam_result, _action, {:error, reason}}, socket) do
    {:noreply,
     socket
     |> assign(packing: false, flash_mode: @default_flash_mode)
     |> update(:output, &(&1 <> "\n[packbeam error] " <> reason <> "\n"))}
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
            <button
              data-role="flash-to-device-and-monitor"
              data-idle-label="Flash to Device & Monitor"
              data-busy-label="Flashing & Starting Monitor..."
              class={["btn btn-sm btn-secondary", if(@packing, do: "btn-disabled")]}
              disabled={@packing}
              phx-click="flash-with-monitor"
            >
              {if @packing, do: "Packing AVM...", else: "Flash to Device & Monitor"}
            </button>
            <button
              data-role="serial-monitor-only"
              data-idle-label="Serial Monitor"
              data-active-label="Stop Serial Monitor"
              data-busy-label="Opening Monitor..."
              class="btn btn-sm btn-outline"
              type="button"
            >
              Serial Monitor
            </button>
          </div>
        </div>
      </div>

      <div class="card bg-base-200 shadow min-h-0 h-full">
        <div class="card-body p-4 flex flex-col min-h-0">
          <div class="flex items-center justify-between mb-2">
            <h2 class="card-title text-sm">Serial Monitor</h2>
            <div class="flex items-center gap-2">
              <button
                data-role="reset-esp32"
                data-idle-label="Reset ESP32"
                data-busy-label="Resetting..."
                class="btn btn-xs btn-warning"
                type="button"
              >
                Reset ESP32
              </button>
              <button class="btn btn-xs btn-ghost" phx-click="clear-output">Clear</button>
            </div>
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
        this.flashButtons = Array.from(
          this.el.querySelectorAll(
            '[data-role="flash-to-device"], [data-role="flash-to-device-and-monitor"]'
          )
        );
        this.lastProgressBucket = -1;
        this.selectedPort = null;
        this.monitorDecoder = new TextDecoder();
        this.monitorPort = null;
        this.monitorReader = null;
        this.monitorStopRequested = false;
        this.monitorTask = null;
        this.monitorOpening = false;
        this.serialMonitorButton = this.el.querySelector(
          '[data-role="serial-monitor-only"]'
        );
        this.resetEsp32Button = this.el.querySelector('[data-role="reset-esp32"]');
        this.resetInProgress = false;
        this.monitorClickHandler = async (event) => {
          event.preventDefault();
          await this._handleSerialMonitorClick();
        };
        this.resetClickHandler = async (event) => {
          event.preventDefault();
          await this._handleResetEsp32Click();
        };

        if (this.serialMonitorButton) {
          this.serialMonitorButton.addEventListener("click", this.monitorClickHandler);
          this._syncSerialMonitorButton();
        }

        if (this.resetEsp32Button) {
          this.resetEsp32Button.addEventListener("click", this.resetClickHandler);
          this._syncResetButton();
        }

        try {
          const mod = await import(/* @vite-ignore */ ESP32TOOL_CDN);
          this.esp32tool = mod;
          this.pushEvent("serial-output", {
            text:
              `\n[esptool] Loaded browser entry from CDN: ${ESP32TOOL_CDN}\n` +
              `[esptool] connect() ${typeof mod.connect === "function" ? "is available" : "is missing"}\n` +
              `[esptool] connectWithPort() ${typeof mod.connectWithPort === "function" ? "is available" : "is missing"}\n`,
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

        this.handleEvent("esptool-flash", async ({ filename, bytes, address, monitor }) => {
          if (this.flashInProgress) {
            this._appendSerialOutput("[esptool] Flash already in progress.");
            return;
          }

          if (!this.esp32tool?.connect) {
            this._appendSerialOutput("[esptool] esp32tool is not loaded yet.");
            return;
          }

          await this._stopMonitor({ closePort: true, quiet: true });

          const imageBytes = this._toUint8Array(bytes);
          const flashAddress = Number.isFinite(address)
            ? address
            : DEFAULT_FLASH_ADDRESS;
          const monitorAfterFlash = !!monitor;

          if (imageBytes.byteLength === 0) {
            this._appendSerialOutput("[esptool] Refusing to flash an empty AVM.");
            return;
          }

          let loader = null;
          let disconnectLoader = true;

          this.flashInProgress = true;
          this.lastProgressBucket = -1;
          this._setFlashButtonsState(true);
          this._appendSerialOutput(
            `[esptool] Connecting to device to flash ${filename || "main.avm"} ` +
              `to ${this._formatHex(flashAddress)}...`
          );

          try {
            loader = await this._connectLoader();
            this.selectedPort = loader?.port || this.selectedPort;
            await loader.initialize();
            loader = await loader.runStub();
            this.selectedPort = loader?.port || this.selectedPort;
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

            if (monitorAfterFlash) {
              try {
                if (typeof loader.enterConsoleMode !== "function") {
                  this._appendSerialOutput(
                    "[monitor] This esp32tool build cannot switch into console mode automatically."
                  );

                  const portChanged = await loader.resetToFirmware();
                  this._appendSerialOutput(
                    portChanged
                      ? "[esptool] Device reset requested. Reselect the new USB port if it changed."
                      : "[esptool] Device reset to firmware."
                  );
                } else {
                  this._appendSerialOutput(
                    "[monitor] Resetting into firmware and attaching serial monitor..."
                  );

                  const portChanged = await loader.enterConsoleMode();

                  if (portChanged) {
                    this._appendSerialOutput(
                      "[monitor] Device switched USB ports after reset. Reselect the firmware port to continue monitoring."
                    );
                  } else {
                    if (typeof loader.releaseReaderWriter === "function") {
                      await loader.releaseReaderWriter();
                    }

                    const port = loader?.port || this.selectedPort;
                    await this._startMonitor(port);
                    disconnectLoader = false;
                  }
                }
              } catch (monitorErr) {
                console.error("[EsptoolActions] monitor setup failed:", monitorErr);
                this._appendSerialOutput(
                  `[monitor] Flash succeeded, but monitor startup failed: ${String(monitorErr)}`
                );
              }
            } else {
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
            }
          } catch (err) {
            console.error("[EsptoolActions] flash failed:", err);
            this._appendSerialOutput(`[esptool] Flash failed: ${String(err)}`);
          } finally {
            this.flashInProgress = false;
            this._setFlashButtonsState(false);

            if (disconnectLoader && loader?.disconnect) {
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

      destroyed() {
        if (this.serialMonitorButton && this.monitorClickHandler) {
          this.serialMonitorButton.removeEventListener("click", this.monitorClickHandler);
        }

        if (this.resetEsp32Button && this.resetClickHandler) {
          this.resetEsp32Button.removeEventListener("click", this.resetClickHandler);
        }

        this._stopMonitor({ closePort: true, quiet: true });
      },

      async _handleSerialMonitorClick() {
        if (this.flashInProgress || this.resetInProgress) {
          this._appendSerialOutput(
            "[monitor] Wait for the current flash operation to finish first."
          );
          return;
        }

        if (this.monitorReader || this.monitorTask) {
          await this._stopMonitor({ closePort: true });
          return;
        }

        this.monitorOpening = true;
        this._syncSerialMonitorButton();

        try {
          const port = await this._requestMonitorPort();
          await this._startMonitor(port);
        } catch (err) {
          if (err?.name === "NotFoundError") {
            this._appendSerialOutput("[monitor] Serial monitor selection cancelled.");
          } else {
            console.error("[EsptoolActions] monitor open failed:", err);
            this._appendSerialOutput(
              `[monitor] Could not start serial monitor: ${String(err)}`
            );
          }
        } finally {
          this.monitorOpening = false;
          this._syncSerialMonitorButton();
        }
      },

      async _handleResetEsp32Click() {
        if (this.flashInProgress || this.monitorOpening || this.resetInProgress) {
          this._appendSerialOutput(
            "[esptool] Wait for the current serial operation to finish first."
          );
          return;
        }

        if (!this.selectedPort) {
          this._appendSerialOutput(
            "[esptool] Connect to the ESP32 first before sending reset."
          );
          return;
        }

        if (!this.esp32tool?.connectWithPort) {
          this._appendSerialOutput(
            "[esptool] esp32tool reset support is not loaded yet."
          );
          return;
        }

        const port = this.selectedPort;
        const wasMonitoring = !!(this.monitorReader || this.monitorTask);

        this.resetInProgress = true;
        this._syncSerialMonitorButton();
        this._syncResetButton();
        this._appendSerialOutput("[esptool] Sending reset to ESP32...");

        try {
          if (wasMonitoring) {
            await this._stopMonitor({ closePort: false, quiet: true });
          }

          const loader = await this.esp32tool.connectWithPort(
            port,
            this._buildLogger()
          );

          if (typeof loader.resetInConsoleMode === "function") {
            await loader.resetInConsoleMode();
          } else if (typeof loader.hardResetToFirmware === "function") {
            await loader.hardResetToFirmware();
          } else {
            throw new Error("esp32tool reset support is unavailable.");
          }

          this._appendSerialOutput("[esptool] Reset command sent.");

          if (wasMonitoring) {
            await this._delay(250);
            await this._startMonitor(port);
          }
        } catch (err) {
          console.error("[EsptoolActions] reset failed:", err);
          this._appendSerialOutput(`[esptool] Reset failed: ${String(err)}`);

          if (wasMonitoring) {
            try {
              await this._startMonitor(port);
            } catch (monitorErr) {
              console.error("[EsptoolActions] monitor resume failed:", monitorErr);
              this._appendSerialOutput(
                `[monitor] Could not resume serial monitor after reset failure: ${String(monitorErr)}`
              );
            }
          }
        } finally {
          this.resetInProgress = false;
          this._syncSerialMonitorButton();
          this._syncResetButton();
        }
      },

      async _requestMonitorPort() {
        if (this.selectedPort) {
          return this.selectedPort;
        }

        const requestSerialPort = globalThis.requestSerialPort;

        if (typeof requestSerialPort === "function") {
          return await requestSerialPort();
        }

        if (!navigator.serial?.requestPort) {
          throw new Error(
            "Web Serial API is not supported in this browser."
          );
        }

        return await navigator.serial.requestPort();
      },

      async _connectLoader() {
        const logger = this._buildLogger();

        if (this.selectedPort && this.esp32tool?.connectWithPort) {
          try {
            this._appendSerialOutput("[esptool] Reusing previously selected serial port...");
            return await this.esp32tool.connectWithPort(this.selectedPort, logger);
          } catch (err) {
            console.warn("[EsptoolActions] failed to reuse port:", err);
            this._appendSerialOutput(
              `[esptool] Reusing the previous port failed: ${String(err)}`
            );
          }
        }

        return await this.esp32tool.connect(logger);
      },

      async _startMonitor(port) {
        if (!port) {
          throw new Error("No serial port is available for monitoring.");
        }

        await this._stopMonitor({ closePort: false, quiet: true });

        if (!port.readable && typeof port.open === "function") {
          await port.open({ baudRate: 115200 });
        }

        if (!port.readable) {
          throw new Error("Readable stream not available for firmware monitoring.");
        }

        this.selectedPort = port;
        this.monitorPort = port;
        this.monitorStopRequested = false;
        this.monitorDecoder = new TextDecoder();
        this._syncSerialMonitorButton();
        this._syncResetButton();

        const reader = port.readable.getReader();
        this.monitorReader = reader;

        this._appendSerialOutput("[monitor] Serial monitor attached at 115200 baud.");

        this.monitorTask = (async () => {
          try {
            while (true) {
              const { value, done } = await reader.read();
              if (done) break;
              if (!value || value.length === 0) continue;

              const text = this.monitorDecoder.decode(value, { stream: true });
              if (text) {
                this.pushEvent("serial-output", { text });
              }
            }

            const tail = this.monitorDecoder.decode();
            if (tail) {
              this.pushEvent("serial-output", { text: tail });
            }

            if (!this.monitorStopRequested) {
              this._appendSerialOutput("[monitor] Serial monitor stopped.");
            }
          } catch (err) {
            if (!this.monitorStopRequested) {
              console.error("[EsptoolActions] monitor failed:", err);
              this._appendSerialOutput(
                `[monitor] Serial monitor failed: ${String(err)}`
              );
            }
          } finally {
            try {
              reader.releaseLock();
            } catch (releaseErr) {
              console.debug(
                "[EsptoolActions] monitor reader release failed:",
                releaseErr
              );
            }

            if (this.monitorReader === reader) {
              this.monitorReader = null;
            }

            this.monitorTask = null;
            this.monitorStopRequested = false;
            this._syncSerialMonitorButton();
          }
        })();
      },

      async _stopMonitor({ closePort = false, quiet = false } = {}) {
        const port = this.monitorPort || this.selectedPort;
        const hadMonitor = !!(this.monitorReader || this.monitorTask);

        if (hadMonitor) {
          this.monitorStopRequested = true;

          if (!quiet) {
            this._appendSerialOutput("[monitor] Stopping serial monitor...");
          }

          if (this.monitorReader) {
            try {
              await this.monitorReader.cancel();
            } catch (err) {
              console.debug("[EsptoolActions] monitor cancel failed:", err);
            }
          }

          if (this.monitorTask) {
            try {
              await this.monitorTask;
            } catch (err) {
              console.debug("[EsptoolActions] monitor task failed:", err);
            }
          }
        }

        if (closePort && port && (port.readable || port.writable)) {
          try {
            await port.close();
          } catch (err) {
            console.debug("[EsptoolActions] monitor port close failed:", err);
          }
        }

        this.monitorPort = closePort ? null : port;
        this._syncSerialMonitorButton();
        this._syncResetButton();
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

      _setFlashButtonsState(isBusy) {
        for (const button of this.flashButtons) {
          button.disabled = isBusy;
          button.classList.toggle("btn-disabled", isBusy);
          button.textContent = isBusy
            ? button.dataset.busyLabel || "Flashing Device..."
            : button.dataset.idleLabel || "Flash to Device";
        }

        this._syncSerialMonitorButton();
        this._syncResetButton();
      },

      _syncSerialMonitorButton() {
        if (!this.serialMonitorButton) return;

        const active = !!(this.monitorReader || this.monitorTask);
        const busy = this.flashInProgress || this.monitorOpening || this.resetInProgress;

        this.serialMonitorButton.disabled = busy;
        this.serialMonitorButton.classList.toggle("btn-disabled", busy);
        this.serialMonitorButton.textContent = busy
          ? this.serialMonitorButton.dataset.busyLabel || "Opening Monitor..."
          : active
            ? this.serialMonitorButton.dataset.activeLabel || "Stop Serial Monitor"
            : this.serialMonitorButton.dataset.idleLabel || "Serial Monitor";
      },

      _syncResetButton() {
        if (!this.resetEsp32Button) return;

        const busy = this.flashInProgress || this.monitorOpening || this.resetInProgress;
        const enabled = !!this.selectedPort && !busy;

        this.resetEsp32Button.disabled = !enabled;
        this.resetEsp32Button.classList.toggle("btn-disabled", !enabled);
        this.resetEsp32Button.textContent = this.resetInProgress
          ? this.resetEsp32Button.dataset.busyLabel || "Resetting..."
          : this.resetEsp32Button.dataset.idleLabel || "Reset ESP32";
      },

      _delay(ms) {
        return new Promise((resolve) => setTimeout(resolve, ms));
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
