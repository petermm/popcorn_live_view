defmodule WasmLiveView.WokwiVerticalLive do
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
    <div class="grid gap-4 h-full min-h-0" style="grid-template-rows: auto minmax(0, 1fr);">
      <div class="flex items-center justify-between">
        <h1 class="text-xl font-bold">Wokwi ESP32 AtomVM</h1>
        <span class={["badge", if(@connected, do: "badge-success", else: "badge-warning")]}>
          {if @connected, do: "Connected", else: "Connecting..."}
        </span>
      </div>

      <div class="grid gap-4 min-h-0 h-full" style="grid-template-columns: 1fr 2fr;">
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
            <h2 class="card-title text-sm">ESP32 Simulation</h2>
            <div
              id="wokwi-embed"
              phx-hook="WokwiEmbed"
              phx-update="ignore"
              data-serial-monitor="always"
              class="rounded-lg overflow-hidden flex-1 min-h-0"
            ></div>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
