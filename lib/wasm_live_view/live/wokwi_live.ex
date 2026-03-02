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

  @default_code """
  -module(blinky).
  -export([start/0]).

  start() ->
      GPIO = gpio:open(),
      gpio:set_direction(GPIO, 2, output),
      loop(GPIO, off).

  loop(GPIO, off) ->
      gpio:set_level(GPIO, 2, 0),
      timer:sleep(500),
      loop(GPIO, on);
  loop(GPIO, on) ->
      gpio:set_level(GPIO, 2, 1),
      timer:sleep(500),
      loop(GPIO, off).
  """

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       current_route: :wokwi,
       code: @default_code,
       start_module: "blinky",
       output: "",
       connected: false,
       packing: false
     )}
  end

  @impl true
  def handle_event("code-changed", params, socket) do
    code = Map.get(params, "code", socket.assigns.code)
    start_module = Map.get(params, "start_module", socket.assigns.start_module)
    {:noreply, assign(socket, code: code, start_module: start_module)}
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
    <div class="grid gap-4 h-full min-h-0" style="grid-template-rows: auto auto minmax(0, 1fr);">
      <div class="flex items-center justify-between">
        <h1 class="text-xl font-bold">Wokwi ESP32 AtomVM</h1>
        <span class={["badge", if(@connected, do: "badge-success", else: "badge-warning")]}>
          {if @connected, do: "Connected", else: "Connecting..."}
        </span>
      </div>

      <div class="grid lg:grid-cols-2 gap-4 min-h-0">
        <%!-- Code Editor --%>
        <div class="card bg-base-200 shadow">
          <div class="card-body p-4">
            <h2 class="card-title text-sm">AtomVM Erlang Code</h2>
            <form phx-change="code-changed">
              <input
                type="text"
                name="start_module"
                value={@start_module}
                class="input input-bordered input-sm w-full mb-2 font-mono text-xs"
                placeholder="Start module (e.g. myapp)"
                phx-debounce="300"
              />
              <textarea
                name="code"
                class="textarea textarea-bordered font-mono text-xs w-full"
                style="height: 360px; resize: none;"
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
                {if @packing, do: "Packing AVM...", else: "\u25B6 Flash & Run"}
              </button>
            </div>
          </div>
        </div>

        <%!-- Wokwi Simulator --%>
        <div class="card bg-base-200 shadow">
          <div class="card-body p-4">
            <h2 class="card-title text-sm">ESP32 Simulation</h2>
            <div
              id="wokwi-embed"
              phx-hook="WokwiEmbed"
              phx-update="ignore"
              class="rounded-lg overflow-hidden"
              style="height: 390px;"
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
            phx-hook="WokwiSerialAutoScroll"
            class="bg-base-300 rounded-lg p-3 overflow-auto font-mono text-xs flex-1 min-h-0"
          >
            <pre class="text-success whitespace-pre-wrap break-all">{if @output == "", do: "Waiting for simulation output...", else: @output}</pre>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
