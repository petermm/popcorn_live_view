defmodule WasmLiveView.WokwiLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :terminal_app}

  @flasher_args %{
    "write_flash_args" => ["--flash_mode", "dio", "--flash_size", "8MB", "--flash_freq", "40m"],
    "flash_settings" => %{"flash_mode" => "dio", "flash_size" => "8MB", "flash_freq" => "40m"},
    "flash_files" => %{
      "0x1000" => "bootloader.bin",
      "0x10000" => "atomvm-esp32.bin",
      "0x8000" => "partition-table.bin",
      "0x1D0000" => "elixir_esp32boot.avm",
      "0x210000" => "main.avm"
    },
    "extra_esptool_args" => %{
      "after" => "hard_reset",
      "before" => "default_reset",
      "stub" => true,
      "chip" => "esp32"
    }
  }

  @code_examples %{
    "blinky" => """
    -module(blinky).
    -export([start/0]).

    start() ->
        GPIO = gpio:open(),
        io:format("Hello World!~n"),
        gpio:set_direction(GPIO, 2, output),
        loop(GPIO, off).

    loop(GPIO, off) ->
        gpio:set_level(GPIO, 2, 0),
        timer:sleep(500),
        io:format("ON~n~n"),
        loop(GPIO, on);
    loop(GPIO, on) ->
        gpio:set_level(GPIO, 2, 1),
        timer:sleep(500),
        io:format("OFF~n~n"),
        loop(GPIO, off).
    """,
    "button" => """
    -module(button).
    -export([start/0]).

    start() ->
        GPIO = gpio:open(),
        Pin = 4,
        gpio:set_pin_mode(Pin, input),
        gpio:set_pin_pull(Pin, up),
        gpio:set_direction(GPIO, Pin, input),
        gpio:set_direction(GPIO, 2, output),
        gpio:set_level(GPIO, 2, 0),
        % try changing from falling to both or rising and press button
        ok = gpio:set_int(GPIO, Pin, falling),
        io:format("Waiting for button press on GPIO ~p...~n", [Pin]),
        loop(GPIO, 0).

    loop(GPIO, Count) ->
        receive
            {gpio_interrupt, 4} ->
                NewCount = Count + 1,
                io:format("Button pressed! Count: ~p~n", [NewCount]),
                gpio:set_level(GPIO, 2, NewCount rem 2),
                loop(GPIO, NewCount)
        end.
    """,
    "wifi" => """
    -module(wifi).
    -export([start/0]).

    start() ->
        io:format("WiFi example starting...~n"),
        Config = [
            {sta, [
                {connected, fun sta_connected/0},
                {got_ip, fun sta_got_ip/1},
                {disconnected, fun sta_disconnected/0},
                {ssid, "Wokwi-GUEST"},
                {psk, ""}
            ]},
            {sntp, [
                {host, "time.aws.com"},
                {synchronized, fun sntp_synchronized/1}
            ]}
        ],
        {ok, _} = network:start(Config),
        io:format("Network started, waiting for WiFi and SNTP...~n"),
        wait_for_sntp(0).

    sta_connected() ->
        io:format("WiFi connected!~n").

    sta_got_ip(IpInfo) ->
        io:format("Got IP: ~p~n", [IpInfo]).

    sta_disconnected() ->
        io:format("WiFi disconnected.~n").

    sntp_synchronized({_Sec, _Usec}) ->
        io:format("Time synchronized via SNTP!~n").

    wait_for_sntp(Count) when Count >= 60 ->
        io:format("Timeout waiting for SNTP.~n"),
        network:stop(),
        ok;
    wait_for_sntp(Count) ->
        {{Year, _Month, _Day}, _Time} = erlang:universaltime(),
        case Year of
            1970 ->
                io:format("waiting for sntp~n"),
                timer:sleep(500),
                wait_for_sntp(Count + 1);
            _ ->
                io:format("Time is valid (Year: ~p). Closing connection.~n", [Year]),
                network:stop(),
                ok
        end.
    """
  }

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       current_route: :wokwi,
       current_example: "blinky",
       code: Map.fetch!(@code_examples, "blinky"),
       code_examples: @code_examples,
       start_module: "blinky",
       output: "",
       connected: false,
       packing: false
     )}
  end

  @impl true
  def handle_event("code-changed", params, socket) do
    code = Map.get(params, "code", socket.assigns.code)
    {:noreply, assign(socket, code: code)}
  end

  @impl true
  def handle_event("select-example", %{"example" => example}, socket) do
    code = Map.fetch!(@code_examples, example)
    {:noreply, assign(socket, current_example: example, code: code, start_module: example)}
  end

  @impl true
  def handle_event("serial-output", %{"text" => text}, socket) do
    {:noreply, update(socket, :output, &(&1 <> text))}
  end

  @impl true
  def handle_event("wokwi-connected", _params, socket) do
    {:noreply, assign(socket, :connected, true)}
  end

  @impl true
  def handle_event("clear-output", _params, socket) do
    {:noreply, assign(socket, :output, "")}
  end

  @impl true
  def handle_event("flash", _params, socket) do
    start_pack_job(socket, :flash)
  end

  @impl true
  def handle_event("download-avm", _params, socket) do
    start_pack_job(socket, :download)
  end

  defp start_pack_job(socket, action) do
    lv = self()
    source = socket.assigns.code
    start_module = socket.assigns.start_module |> String.trim()

    spawn(fn ->
      result =
        with {:ok, compiled} <- WasmLiveView.EvalInWasm.compile_erlang(source),
             {:ok, avm_binary} <- pack_avm(compiled, start_module) do
          {:ok, avm_binary}
        else
          {:error, reason} -> {:error, inspect(reason)}
          other -> {:error, inspect(other)}
        end

      send(lv, {:packbeam_result, action, result})
    end)

    {:noreply, assign(socket, packing: true)}
  end

  defp pack_avm(_compiled, ""), do: {:error, "start module is required"}

  defp pack_avm(compiled, start_module) do
    opts = %{start_module: String.to_atom(start_module)}
    :packbeam_api.create_from_binaries(compiled, opts)
  end

  @impl true
  def handle_info({:packbeam_result, :flash, {:ok, avm_binary}}, socket)
      when is_binary(avm_binary) do
    {:noreply,
     socket
     |> assign(packing: false)
     |> push_event("wokwi-flash", %{
       flasher_args: @flasher_args,
       flash_files: %{"main.avm" => :binary.bin_to_list(avm_binary)}
     })}
  end

  def handle_info({:packbeam_result, :download, {:ok, avm_binary}}, socket)
      when is_binary(avm_binary) do
    {:noreply,
     socket
     |> assign(packing: false)
     |> push_event("download-avm", %{
       filename: "main.avm",
       bytes: :binary.bin_to_list(avm_binary)
     })}
  end

  def handle_info({:packbeam_result, _action, {:error, reason}}, socket) do
    {:noreply,
     socket
     |> assign(packing: false)
     |> update(:output, &(&1 <> "\n[packbeam error] " <> reason <> "\n"))}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="grid gap-4 h-full min-h-0" style="grid-template-rows: auto minmax(0, 3fr) minmax(0, 2fr);">
      <div class="flex items-center justify-between">
        <h1 class="text-xl font-bold">Wokwi ESP32 AtomVM</h1>
        <span class={["badge", if(@connected, do: "badge-success", else: "badge-warning")]}>
          {if @connected, do: "Connected", else: "Connecting..."}
        </span>
      </div>

      <div class="grid lg:grid-cols-2 gap-4 min-h-0 h-full">
        <%!-- Code Editor --%>
         <div class="card bg-base-200 shadow min-h-0 h-full">
           <div class="card-body p-4 flex flex-col min-h-0 h-full">
             <h2 class="card-title text-sm mb-4">AtomVM Erlang Code</h2>
             <div class="flex gap-6 items-start">
               <div class="tabs tabs-lifted tabs-vertical gap-1">
                 <%= for example <- Map.keys(@code_examples) do %>
                   <button
                     phx-click="select-example"
                     phx-value-example={example}
                     class={["tab tab-lg font-semibold", if(@current_example == example, do: "tab-active bg-base-100", else: "hover:bg-base-300")]}
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
                class={[
                  "btn btn-sm btn-primary",
                  if(not @connected or @packing, do: "btn-disabled")
                ]}
                disabled={not @connected or @packing}
                phx-click="flash"
              >
                {if @packing, do: "Packing AVM...", else: "\u25B6 Pack & Run"}
              </button>
            </div>
          </div>
        </div>

        <%!-- Wokwi Simulator --%>
        <div class="card bg-base-200 shadow min-h-0 h-full">
          <div class="card-body p-4 flex flex-col min-h-0 h-full">
            <div
              id="wokwi-embed"
              phx-hook=".WokwiEmbed"
              phx-update="ignore"
              class="rounded-lg overflow-hidden flex-1 min-h-0"
            ></div>
          </div>
        </div>
      </div>

      <%!-- Serial Monitor --%>
      <div class="card bg-base-200 shadow min-h-0 h-full">
        <div class="card-body p-4 flex flex-col min-h-0">
          <div class="flex items-center justify-between mb-2">
            <h2 class="card-title text-sm">Serial Monitor</h2>
            <button class="btn btn-xs btn-ghost" phx-click="clear-output">Clear</button>
          </div>
          <div
            id="wokwi-serial-output"
            phx-hook=".WokwiSerialAutoScroll"
            class="bg-base-300 rounded-lg p-3 overflow-auto font-mono text-xs flex-1 min-h-0"
          >
            <pre class="text-success whitespace-pre-wrap break-all">{if @output == "", do: "Waiting for simulation output...", else: @output}</pre>
          </div>
        </div>
      </div>
    </div>

    <script :type={Phoenix.LiveView.ColocatedHook} name=".WokwiEmbed">
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

    export default {
      async mounted() {
        this.flashUploadSeq = 0;
        this.cachedFlashSections = {};
        const baseUrl = new URL("..", import.meta.url).href;
        const basePath = new URL(baseUrl).pathname.replace(/\/$/, "");

        // Create and mount the Wokwi iframe.
        // experimental/embed blocks framing; experimental/viewer?api=1 allows it and
        // supports the full @wokwi/client MessagePort API.
        const iframe = document.createElement("iframe");
        iframe.src =
          "https://wokwi.com/experimental/embed?client_id=wokwi_client_omf4ejkz6n6twj7d3x2eqmyp";
        iframe.style.cssText = "width:100%;height:100%;border:none;";
        iframe.setAttribute("loading", "lazy");
        iframe.setAttribute("credentialless", "");
        iframe.referrerPolicy = "no-referrer";
        this.el.appendChild(iframe);
        this.wokwiIframe = iframe;
        this.wokwiClient = null;

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
                bytes = Uint8Array.from(atob(content), (c) => c.charCodeAt(0));
              } else if (Array.isArray(content)) {
                bytes = Uint8Array.from(content);
              } else if (content instanceof Uint8Array) {
                bytes = content;
              } else if (content instanceof ArrayBuffer) {
                bytes = new Uint8Array(content);
              } else {
                const firmwareUrl = `${basePath}/wokwi/firmware/${sourceName}`;
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
    </script>

    <script :type={Phoenix.LiveView.ColocatedHook} name=".WokwiSerialAutoScroll">
    export default {
      mounted() {
        this._raf = null;
        this._bottomThreshold = 24;
        this._shouldAutoScroll = true;
        this._onScroll = () => {
          this._shouldAutoScroll = this._isNearBottom();
        };

        this.el.addEventListener("scroll", this._onScroll, { passive: true });
        this._scrollToBottom();
      },

      beforeUpdate() {
        this._shouldAutoScroll = this._shouldAutoScroll || this._isNearBottom();
      },

      updated() {
        if (this._shouldAutoScroll) {
          this._scrollToBottom();
        }
      },

      destroyed() {
        if (this._onScroll) {
          this.el.removeEventListener("scroll", this._onScroll);
        }

        if (this._raf != null) {
          cancelAnimationFrame(this._raf);
          this._raf = null;
        }
      },

      _isNearBottom() {
        return (
          this.el.scrollHeight - (this.el.scrollTop + this.el.clientHeight) <=
          this._bottomThreshold
        );
      },

      _scrollToBottom() {
        if (this._raf != null) cancelAnimationFrame(this._raf);
        this._raf = requestAnimationFrame(() => {
          this.el.scrollTop = this.el.scrollHeight;
          this._shouldAutoScroll = true;
          this._raf = null;
        });
      },
    };
    </script>
    """
  end
end
