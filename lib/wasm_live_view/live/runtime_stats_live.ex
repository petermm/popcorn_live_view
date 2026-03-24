defmodule WasmLiveView.RuntimeStatsLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents

  @refresh_interval 2_000

  @probe_keys [
    :process_count,
    :atom_count,
    :port_count,
    :schedulers,
    :otp_release,
    :version,
    :machine,
    :wordsize,
    :platform,
    :memory
  ]

  # Each stat probe runs in a spawned process so a NIF-not-found crash
  # (which bypasses try/catch in AtomVM) only kills the probe, not the LiveView.
  defp run_probe(:process_count), do: :erlang.system_info(:process_count)
  defp run_probe(:atom_count), do: :erlang.system_info(:atom_count)
  defp run_probe(:port_count), do: :erlang.system_info(:port_count)
  defp run_probe(:schedulers), do: :erlang.system_info(:schedulers)
  defp run_probe(:otp_release), do: :erlang.system_info(:otp_release) |> to_string()
  defp run_probe(:version), do: :erlang.system_info(:version) |> to_string()
  defp run_probe(:machine), do: :erlang.system_info(:machine) |> to_string()
  defp run_probe(:wordsize), do: :erlang.system_info(:wordsize)
  defp run_probe(:platform), do: :atomvm.platform() |> to_string()
  defp run_probe(:memory), do: :erlang.memory()

  @impl true
  def mount(_params, _session, socket) do
    socket =
      assign(socket,
        current_route: :runtime_stats,
        auto_refresh: true,
        mounted_at: System.monotonic_time(:millisecond),
        erlang_stats: %{},
        js_stats: %{},
        js_stats_receiver: nil,
        available_probes: @probe_keys,
        failed_probes: []
      )

    socket =
      if connected?(socket) do
        socket
        |> ensure_js_stats_receiver()
        |> run_probes()
        |> schedule_refresh()
        |> fetch_js_stats()
      else
        socket
      end

    {:ok, socket}
  end

  @impl true
  def handle_info(:refresh, socket) do
    socket =
      socket
      |> run_probes()
      |> fetch_js_stats()
      |> update_uptime()

    socket =
      if socket.assigns.auto_refresh,
        do: schedule_refresh(socket),
        else: socket

    {:noreply, socket}
  end

  def handle_info({:probe_result, key, value}, socket) do
    stats = Map.put(socket.assigns.erlang_stats, key, value)
    {:noreply, assign(socket, erlang_stats: stats)}
  end

  def handle_info({:probe_failed, key}, socket) do
    failed =
      if key in socket.assigns.failed_probes,
        do: socket.assigns.failed_probes,
        else: [key | socket.assigns.failed_probes]

    available = Enum.reject(socket.assigns.available_probes, &(&1 == key))
    {:noreply, assign(socket, failed_probes: failed, available_probes: available)}
  end

  def handle_info({:js_stats, stats}, socket) do
    {:noreply, assign(socket, js_stats: stats)}
  end

  def handle_info(_msg, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("toggle-auto-refresh", _params, socket) do
    auto = !socket.assigns.auto_refresh

    socket =
      if auto,
        do: schedule_refresh(assign(socket, auto_refresh: true)),
        else: assign(socket, auto_refresh: false)

    {:noreply, socket}
  end

  def handle_event("refresh", _params, socket) do
    socket =
      socket
      |> run_probes()
      |> fetch_js_stats()
      |> update_uptime()

    {:noreply, socket}
  end

  def handle_event("stress-atoms", _params, socket) do
    spawn(fn ->
      for i <- 1..1000 do
        String.to_atom("stress_atom_#{:erlang.unique_integer([:positive])}_#{i}")
      end
    end)

    {:noreply, socket}
  end

  def handle_event("stress-processes", _params, socket) do
    for _ <- 1..1000 do
      spawn(fn ->
        receive do
          :stop -> :ok
        after
          30_000 -> :ok
        end
      end)
    end

    {:noreply, socket}
  end

  def handle_event("stress-messages", _params, socket) do
    parent = self()

    spawn(fn ->
      for _ <- 1..1000 do
        spawn(fn ->
          send(parent, {:stress_msg, :erlang.unique_integer()})
        end)
      end
    end)

    {:noreply, socket}
  end

  def handle_event("stress-binaries", _params, socket) do
    spawn(fn ->
      for _ <- 1..100 do
        _bin = :binary.copy(<<"x">>, 10_000)
      end
    end)

    {:noreply, socket}
  end

  defp schedule_refresh(socket) do
    Process.send_after(self(), :refresh, @refresh_interval)
    socket
  end

  defp update_uptime(socket) do
    elapsed = System.monotonic_time(:millisecond) - socket.assigns.mounted_at
    stats = Map.put(socket.assigns.erlang_stats, :uptime_ms, elapsed)
    assign(socket, erlang_stats: stats)
  end

  # Run each probe in a separate spawned process.
  # If the probe calls a missing NIF, only that process crashes.
  # We monitor it to detect failures and stop retrying failed probes.
  defp run_probes(socket) do
    lv = self()

    for key <- socket.assigns.available_probes do
      spawn(fn ->
        ref =
          Process.monitor(
            spawn(fn ->
              result = run_probe(key)
              send(lv, {:probe_result, key, result})
            end)
          )

        receive do
          {:DOWN, ^ref, :process, _, :normal} -> :ok
          {:DOWN, ^ref, :process, _, _reason} -> send(lv, {:probe_failed, key})
        after
          1_000 -> send(lv, {:probe_failed, key})
        end
      end)
    end

    # Also compute uptime
    update_uptime(socket)
  end

  defp ensure_js_stats_receiver(socket) do
    receiver = :"runtime_stats_#{System.unique_integer([:positive])}"
    lv = self()

    spawn(fn -> start_js_stats_receiver(receiver, lv) end)

    receive do
      {:js_stats_receiver_ready, ^receiver} ->
        assign(socket, js_stats_receiver: receiver)
    after
      1_000 ->
        assign(socket, js_stats_receiver: receiver)
    end
  end

  defp start_js_stats_receiver(receiver, lv) do
    Process.register(self(), receiver)
    send(lv, {:js_stats_receiver_ready, receiver})
    ref = Process.monitor(lv)

    try do
      js_stats_receiver_loop(receiver, lv, ref)
    after
      safe_unregister(receiver)
    end
  end

  defp js_stats_receiver_loop(receiver, lv, ref) do
    receive do
      {:DOWN, ^ref, :process, ^lv, _reason} ->
        :ok

      {:emscripten, _} = raw ->
        handle_js_stats_message(raw, lv)
        js_stats_receiver_loop(receiver, lv, ref)

      _msg ->
        js_stats_receiver_loop(receiver, lv, ref)
    end
  end

  defp handle_js_stats_message(raw, lv) do
    try do
      case Popcorn.Wasm.parse_message!(raw) do
        {:wasm_cast, %{"js_stats" => stats}} ->
          send(lv, {:js_stats, stats})

        _ ->
          :ok
      end
    catch
      _, _ -> :ok
    end
  end

  defp safe_unregister(receiver) do
    try do
      Process.unregister(receiver)
    catch
      _, _ -> :ok
    end
  end

  defp fetch_js_stats(%{assigns: %{js_stats_receiver: nil}} = socket), do: socket

  defp fetch_js_stats(socket) do
    receiver = socket.assigns.js_stats_receiver

    spawn(fn ->
      try do
        Popcorn.Wasm.run_js!(
          """
          ({ wasm, args, iframeWindow }) => {
            const stats = {};

            if (performance.memory) {
              stats.usedJSHeapSize = performance.memory.usedJSHeapSize;
              stats.totalJSHeapSize = performance.memory.totalJSHeapSize;
              stats.jsHeapSizeLimit = performance.memory.jsHeapSizeLimit;
            }

            stats.hardwareConcurrency = navigator.hardwareConcurrency || 0;
            stats.userAgent = navigator.userAgent;
            stats.language = navigator.language;

            // Get WASM linear memory size
            let memBytes = null;
            let memSource = null;

            // Check if wasmMemory was exposed on Module (by popcorn_iframe.js)
            if (wasm.wasmMemory && wasm.wasmMemory.buffer) {
              memBytes = wasm.wasmMemory.buffer.byteLength;
              memSource = "wasmMemory";
            }

            // Fallback: Use _malloc to probe memory. Allocate 1 byte, read the
            // pointer value, then use cwrap to find the buffer backing it.
            if (memBytes === null && wasm._malloc && wasm.cwrap) {
              try {
                // cwrap('malloc', 'number', ['number']) might give us access to HEAPU8
                // Actually, try to find SharedArrayBuffer/ArrayBuffer among iframe globals
                const win = iframeWindow;
                for (const name of Object.getOwnPropertyNames(win)) {
                  try {
                    const val = win[name];
                    if (val instanceof SharedArrayBuffer && val.byteLength >= 1048576 && val.byteLength % 65536 === 0) {
                      memBytes = val.byteLength;
                      memSource = "SharedArrayBuffer:" + name;
                      break;
                    }
                    if (val instanceof WebAssembly.Memory) {
                      memBytes = val.buffer.byteLength;
                      memSource = "WebAssembly.Memory:" + name;
                      break;
                    }
                  } catch(e) {}
                }
              } catch(e) {}
            }

            if (memBytes !== null) {
              stats.wasmMemoryBytes = memBytes;
              stats.wasmMemoryPages = memBytes / 65536;
              stats.wasmMemSource = memSource;
            }

            // Convert booleans to strings for AtomVM compatibility
            stats.crossOriginIsolated = self.crossOriginIsolated ? "yes" : "no";

            wasm.cast(args.receiver, { js_stats: stats });
          }
          """,
          %{receiver: receiver}
        )
      catch
        _, _ -> :ok
      end
    end)

    socket
  end

  defp format_bytes(nil), do: "N/A"

  defp format_bytes(bytes) when is_integer(bytes) and bytes >= 1_048_576 do
    whole = div(bytes, 1_048_576)
    frac = div(rem(bytes, 1_048_576) * 100, 1_048_576)
    "#{whole}.#{String.pad_leading(to_string(frac), 2, "0")} MB"
  end

  defp format_bytes(bytes) when is_integer(bytes) and bytes >= 1_024 do
    whole = div(bytes, 1_024)
    frac = div(rem(bytes, 1_024) * 100, 1_024)
    "#{whole}.#{String.pad_leading(to_string(frac), 2, "0")} KB"
  end

  defp format_bytes(bytes) when is_integer(bytes), do: "#{bytes} B"
  defp format_bytes(_), do: "N/A"

  defp format_uptime(nil), do: "N/A"
  defp format_uptime("N/A"), do: "N/A"

  defp format_uptime(ms) when is_integer(ms) do
    seconds = div(ms, 1000)
    minutes = div(seconds, 60)
    hours = div(minutes, 60)

    cond do
      hours > 0 -> "#{hours}h #{rem(minutes, 60)}m #{rem(seconds, 60)}s"
      minutes > 0 -> "#{minutes}m #{rem(seconds, 60)}s"
      true -> "#{seconds}s"
    end
  end

  defp format_uptime(_), do: "N/A"

  defp stat_value(stats, key, default \\ "N/A") do
    case Map.get(stats, key) do
      nil -> default
      val -> val
    end
  end

  defp js_stat(stats, key) do
    case Map.get(stats, key) do
      nil -> nil
      val -> val
    end
  end

  defp memory_value(stats, key) do
    stats
    |> Map.get(:memory, [])
    |> get_keyword(key)
    |> format_bytes()
  end

  defp get_keyword(list, key) when is_list(list) do
    case List.keyfind(list, key, 0) do
      {^key, val} -> val
      nil -> nil
    end
  end

  defp get_keyword(_, _), do: nil

  @impl true
  def render(assigns) do
    ~H"""
    <.header>
      Runtime Stats
      <:subtitle>Live WASM runtime statistics from the AtomVM Erlang VM running in your browser.</:subtitle>
    </.header>

    <div class="flex flex-wrap gap-2 my-4">
      <.button phx-click="refresh" class="btn btn-sm btn-outline">
        Refresh
      </.button>
      <label class="label cursor-pointer gap-2">
        <span class="label-text">Auto-refresh</span>
        <input
          type="checkbox"
          class="toggle toggle-sm toggle-primary"
          checked={@auto_refresh}
          phx-click="toggle-auto-refresh"
        />
      </label>
    </div>

    <div class="card bg-base-200 shadow mb-6">
      <div class="card-body p-4">
        <h3 class="card-title text-sm">Stress Test</h3>
        <div class="flex flex-wrap gap-2">
          <.button phx-click="stress-atoms" class="btn btn-sm btn-warning btn-outline">
            +1000 Atoms
          </.button>
          <.button phx-click="stress-processes" class="btn btn-sm btn-warning btn-outline">
            +1000 Processes
          </.button>
          <.button phx-click="stress-messages" class="btn btn-sm btn-warning btn-outline">
            +1000 Messages
          </.button>
          <.button phx-click="stress-binaries" class="btn btn-sm btn-warning btn-outline">
            +1 MB Binaries
          </.button>
        </div>
      </div>
    </div>

    <%!-- Key Metrics --%>
    <div class="stats stats-vertical sm:stats-horizontal shadow w-full mb-6">
      <div class="stat">
        <div class="stat-title">JS Heap Used</div>
        <div class="stat-value text-lg">{format_bytes(js_stat(@js_stats, "usedJSHeapSize"))}</div>
      </div>
      <div class="stat">
        <div class="stat-title">Processes</div>
        <div class="stat-value text-lg">{stat_value(@erlang_stats, :process_count)}</div>
      </div>
      <div class="stat">
        <div class="stat-title">Atoms</div>
        <div class="stat-value text-lg">{stat_value(@erlang_stats, :atom_count)}</div>
      </div>
      <div class="stat">
        <div class="stat-title">Uptime</div>
        <div class="stat-value text-lg">{format_uptime(stat_value(@erlang_stats, :uptime_ms))}</div>
      </div>
    </div>

    <%!-- System Info --%>
    <div class="card bg-base-200 shadow mb-6">
      <div class="card-body p-4">
        <h3 class="card-title text-sm">System Info</h3>
        <div class="grid grid-cols-2 sm:grid-cols-3 gap-2 text-sm">
          <div>
            <span class="text-base-content/60">Platform:</span>
            <span class="font-mono">{stat_value(@erlang_stats, :platform)}</span>
          </div>
          <div>
            <span class="text-base-content/60">Machine:</span>
            <span class="font-mono">{stat_value(@erlang_stats, :machine)}</span>
          </div>
          <div>
            <span class="text-base-content/60">OTP:</span>
            <span class="font-mono">{stat_value(@erlang_stats, :otp_release)}</span>
          </div>
          <div>
            <span class="text-base-content/60">ERTS:</span>
            <span class="font-mono">{stat_value(@erlang_stats, :version)}</span>
          </div>
          <div>
            <span class="text-base-content/60">Word Size:</span>
            <span class="font-mono">{word_size_label(stat_value(@erlang_stats, :wordsize))}</span>
          </div>
          <div>
            <span class="text-base-content/60">Schedulers:</span>
            <span class="font-mono">{stat_value(@erlang_stats, :schedulers)}</span>
          </div>
          <div>
            <span class="text-base-content/60">Ports:</span>
            <span class="font-mono">{stat_value(@erlang_stats, :port_count)}</span>
          </div>
        </div>
      </div>
    </div>

    <%!-- Memory Breakdown (only if erlang:memory/0 is available) --%>
    <div :if={is_list(@erlang_stats[:memory]) and @erlang_stats[:memory] != []} class="card bg-base-200 shadow mb-6">
      <div class="card-body p-4">
        <h3 class="card-title text-sm">Erlang Memory Breakdown</h3>
        <div class="overflow-x-auto">
          <table class="table table-sm table-zebra">
            <thead>
              <tr>
                <th>Category</th>
                <th class="text-right">Size</th>
              </tr>
            </thead>
            <tbody>
              <tr :for={{key, label} <- memory_categories()}>
                <td>{label}</td>
                <td class="text-right font-mono">{memory_value(@erlang_stats, key)}</td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    </div>

    <%!-- Memory --%>
    <div :if={@js_stats != %{}} class="card bg-base-200 shadow mb-6">
      <div class="card-body p-4">
        <h3 class="card-title text-sm">Memory</h3>
        <div class="overflow-x-auto">
          <table class="table table-sm table-zebra">
            <thead>
              <tr>
                <th>Category</th>
                <th class="text-right">Size</th>
              </tr>
            </thead>
            <tbody>
              <tr :if={js_stat(@js_stats, "wasmMemoryBytes")}>
                <td>WASM Linear Memory</td>
                <td class="text-right font-mono">{format_bytes(js_stat(@js_stats, "wasmMemoryBytes"))}</td>
              </tr>
              <tr :if={js_stat(@js_stats, "wasmMemoryPages")}>
                <td>WASM Pages (64 KB each)</td>
                <td class="text-right font-mono">{js_stat(@js_stats, "wasmMemoryPages")}</td>
              </tr>
              <tr :if={js_stat(@js_stats, "wasmMemSource")}>
                <td>WASM Memory Source</td>
                <td class="text-right font-mono text-xs">{js_stat(@js_stats, "wasmMemSource")}</td>
              </tr>
              <tr :if={js_stat(@js_stats, "usedJSHeapSize")}>
                <td>JS Heap Used</td>
                <td class="text-right font-mono">{format_bytes(js_stat(@js_stats, "usedJSHeapSize"))}</td>
              </tr>
              <tr :if={js_stat(@js_stats, "totalJSHeapSize")}>
                <td>JS Heap Total</td>
                <td class="text-right font-mono">{format_bytes(js_stat(@js_stats, "totalJSHeapSize"))}</td>
              </tr>
              <tr :if={js_stat(@js_stats, "jsHeapSizeLimit")}>
                <td>JS Heap Limit</td>
                <td class="text-right font-mono">{format_bytes(js_stat(@js_stats, "jsHeapSizeLimit"))}</td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    </div>

    <%!-- Browser Info --%>
    <div :if={@js_stats != %{}} class="card bg-base-200 shadow mb-6">
      <div class="card-body p-4">
        <h3 class="card-title text-sm">Browser</h3>
        <div class="grid grid-cols-2 sm:grid-cols-3 gap-2 text-sm">
          <div :if={js_stat(@js_stats, "hardwareConcurrency")}>
            <span class="text-base-content/60">CPU Cores:</span>
            <span class="font-mono">{js_stat(@js_stats, "hardwareConcurrency")}</span>
          </div>
          <div :if={js_stat(@js_stats, "language")}>
            <span class="text-base-content/60">Language:</span>
            <span class="font-mono">{js_stat(@js_stats, "language")}</span>
          </div>
          <div :if={js_stat(@js_stats, "crossOriginIsolated")}>
            <span class="text-base-content/60">Cross-Origin Isolated:</span>
            <span class="font-mono">{js_stat(@js_stats, "crossOriginIsolated")}</span>
          </div>
          <div :if={js_stat(@js_stats, "userAgent")} class="col-span-full">
            <span class="text-base-content/60">User Agent:</span>
            <div class="font-mono text-xs break-all mt-1">{js_stat(@js_stats, "userAgent")}</div>
          </div>
        </div>
      </div>
    </div>

    <%!-- Failed Probes --%>
    <div :if={@failed_probes != []} class="card bg-base-200 shadow mb-6">
      <div class="card-body p-4">
        <h3 class="card-title text-sm">Unavailable in AtomVM</h3>
        <div class="flex flex-wrap gap-2">
          <span :for={probe <- Enum.sort(@failed_probes)} class="badge badge-ghost badge-sm">
            {to_string(probe)}
          </span>
        </div>
      </div>
    </div>
    """
  end

  defp word_size_label(4), do: "32-bit"
  defp word_size_label(8), do: "64-bit"
  defp word_size_label(val), do: to_string(val)

  defp memory_categories do
    [
      {:total, "Total"},
      {:processes, "Processes"},
      {:processes_used, "Processes (used)"},
      {:atom, "Atom"},
      {:atom_used, "Atom (used)"},
      {:binary, "Binary"},
      {:code, "Code"},
      {:ets, "ETS"},
      {:system, "System"}
    ]
  end
end
