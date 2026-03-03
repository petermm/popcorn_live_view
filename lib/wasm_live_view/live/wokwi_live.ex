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
        io:format("Hello World!\\n"),
        gpio:set_direction(GPIO, 2, output),
        loop(GPIO, off).

    loop(GPIO, off) ->
        gpio:set_level(GPIO, 2, 0),
        timer:sleep(500),
        io:format("ON\\n\\n"),
        loop(GPIO, on);
    loop(GPIO, on) ->
        gpio:set_level(GPIO, 2, 1),
        timer:sleep(500),
        io:format("OFF\\n\\n"),
        loop(GPIO, off).
    """,
    "wifi" => """
    %
    % This file is part of AtomVM.
    %
    % Copyright 2020 Fred Dushin <fred@dushin.net>
    %
    % Licensed under the Apache License, Version 2.0 (the "License");
    % you may not use this file except in compliance with the License.
    % You may obtain a copy of the License at
    %
    %    http://www.apache.org/licenses/LICENSE-2.0
    %
    % Unless required by applicable law or agreed to in writing, software
    % distributed under the License is distributed on an "AS IS" BASIS,
    % WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    % See the License for the specific language governing permissions and
    % limitations under the License.
    %
    % SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
    %

    -module(wifi).

    -export([start/0]).

    start() ->
        case verify_platform(atomvm:platform()) of
            ok ->
                % test that sta_rssi() can be safely called, when network is not started
                {error, network_down} = network:sta_rssi(),
                ok = start_network(),
                ok = network:stop(),
                start_network(),
                loop(0);
            Error ->
                Error
        end.

    start_network() ->
        Config = [
            {ap, [
                {ap_started, fun ap_started/0},
                {sta_connected, fun sta_connected/1},
                {sta_ip_assigned, fun sta_ip_assigned/1},
                {sta_disconnected, fun sta_disconnected/1}
                | []
            ]},
            {sta, [
                {connected, fun connected/0},
                {got_ip, fun got_ip/1},
                {disconnected, fun disconnected/0}
                | [
                    {dhcp_hostname, "my_device_name"},
                    {ssid, "Wokwi-GUEST"},
                    {psk, ""}
                ]
            ]},
            {sntp, [
                {host, "time.aws.com"},
                {synchronized, fun sntp_synchronized/1}
            ]}
        ],
        case network:start(Config) of
            {ok, _Pid} ->
                % test that sta_rssi() can be safely called
                % when network is just started, but may not yet be connected.
                network:sta_rssi(),
                io:format("Network started.~n"),
                ok;
            Error ->
                Error
        end.

    ap_started() ->
        io:format("AP started.~n").

    sta_connected(Mac) ->
        io:format("STA connected with mac ~p~n", [Mac]).

    sta_disconnected(Mac) ->
        io:format("STA disconnected with mac ~p~n", [Mac]).

    sta_ip_assigned(Address) ->
        io:format("STA assigned address ~p~n", [Address]).

    connected() ->
        io:format("STA connected.~n").

    got_ip(IpInfo) ->
        io:format("Got IP: ~p.~n", [IpInfo]).

    disconnected() ->
        io:format("STA disconnected.~n").

    sntp_synchronized({TVSec, TVUsec}) ->
        io:format("Synchronized time with SNTP server. TVSec=~p TVUsec=~p~n", [TVSec, TVUsec]).

    verify_platform(esp32) ->
        ok;
    verify_platform(Platform) ->
        {error, {unsupported_platform, Platform}}.

    loop(Count) when Count =:= 40 ->
        io:format("Never got SNTP.~n"),
        network:stop();
    loop(Count) ->
        timer:sleep(500),
        {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
        io:format("Date: ~p/~p/~p ~p:~p:~p (~pms)~n", [
            Year, Month, Day, Hour, Minute, Second, erlang:system_time(millisecond)
        ]),
        case Year of
            1970 ->
                loop(Count + 1);
            _ ->
                io:format("Got SNTP.~n"),
                network:stop()
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
             <form phx-change="code-changed">
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
