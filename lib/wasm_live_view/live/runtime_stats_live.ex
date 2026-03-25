defmodule WasmLiveView.RuntimeStatsLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents

  @refresh_interval 2_000
  @max_recent_processes 10
  @max_process_snapshot 15

  @js_stats_receivers [
    :runtime_stats_js_1,
    :runtime_stats_js_2,
    :runtime_stats_js_3,
    :runtime_stats_js_4
  ]

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

  @stress_actions [
    %{event: "stress-atoms", label: "+1000 Atoms"},
    %{event: "stress-processes", label: "+1000 Processes"},
    %{event: "stress-messages", label: "+1000 Messages"},
    %{event: "stress-binaries", label: "+1 MB Binaries"}
  ]

  @system_info_fields [
    %{label: "Platform", key: :platform},
    %{label: "Machine", key: :machine},
    %{label: "OTP", key: :otp_release},
    %{label: "ERTS", key: :version},
    %{label: "Word Size", key: :wordsize, formatter: :word_size},
    %{label: "Schedulers", key: :schedulers},
    %{label: "Ports", key: :port_count}
  ]

  @browser_fields [
    %{label: "CPU Cores", key: "hardwareConcurrency"},
    %{label: "Language", key: "language"},
    %{label: "Cross-Origin Isolated", key: "crossOriginIsolated"},
    %{label: "User Agent", key: "userAgent", full_width: true}
  ]

  @js_memory_fields [
    %{label: "WASM Linear Memory", key: "wasmMemoryBytes", formatter: :bytes},
    %{label: "WASM Pages (64 KB each)", key: "wasmMemoryPages"},
    %{label: "WASM Memory Source", key: "wasmMemSource", class: "text-xs"},
    %{label: "JS Heap Used", key: "usedJSHeapSize", formatter: :bytes},
    %{label: "JS Heap Total", key: "totalJSHeapSize", formatter: :bytes},
    %{label: "JS Heap Limit", key: "jsHeapSizeLimit", formatter: :bytes}
  ]

  @memory_categories [
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

  @initial_process_delta %{added: [], removed_count: 0, total: 0}

  @js_stats_script """
  ({ wasm, args, iframeWindow }) => {
    const stats = {};
    const perfMemory = globalThis.performance?.memory;

    if (perfMemory) {
      stats.usedJSHeapSize = perfMemory.usedJSHeapSize;
      stats.totalJSHeapSize = perfMemory.totalJSHeapSize;
      stats.jsHeapSizeLimit = perfMemory.jsHeapSizeLimit;
    }

    stats.hardwareConcurrency = navigator.hardwareConcurrency || 0;
    stats.userAgent = navigator.userAgent;
    stats.language = navigator.language;

    let memBytes = null;
    let memSource = null;

    const wasmMemorySources = [
      ["wasm.wasmMemory.buffer", wasm.wasmMemory?.buffer],
      ["wasm.HEAPU8.buffer", wasm.HEAPU8?.buffer],
      ["wasm.HEAP8.buffer", wasm.HEAP8?.buffer]
    ];

    for (const [source, buffer] of wasmMemorySources) {
      if (buffer && typeof buffer.byteLength === "number") {
        memBytes = buffer.byteLength;
        memSource = source;
        break;
      }
    }

    if (memBytes !== null) {
      stats.wasmMemoryBytes = memBytes;
      stats.wasmMemoryPages = Math.trunc(memBytes / 65536);
      stats.wasmMemSource = memSource;
    }

    stats.crossOriginIsolated = iframeWindow.crossOriginIsolated ? "yes" : "no";

    wasm.cast(args.receiver, { js_stats: stats, sample_id: args.sample_id });
  }
  """

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
      socket
      |> assign(initial_assigns())
      |> maybe_connect()

    {:ok, socket}
  end

  @impl true
  def handle_info(:refresh, socket) do
    socket =
      socket
      |> assign(:refresh_timer_ref, nil)
      |> maybe_refresh_from_timer()

    {:noreply, socket}
  end

  def handle_info(
        {:js_stats, sample_id, stats},
        %{assigns: %{current_js_sample_id: sample_id}} = socket
      ) do
    {:noreply, assign(socket, :js_stats, stats)}
  end

  def handle_info({:js_stats, _sample_id, _stats}, socket) do
    {:noreply, socket}
  end

  def handle_info({:js_stats_receiver_unavailable, _receiver}, socket) do
    {:noreply, assign(socket, :js_stats_receiver, nil)}
  end

  def handle_info(_msg, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("toggle-auto-refresh", _params, socket) do
    {:noreply, set_auto_refresh(socket, !socket.assigns.auto_refresh)}
  end

  def handle_event("stress-atoms", _params, socket) do
    spawn_stress(socket, fn ->
      for i <- 1..1000 do
        String.to_atom("stress_atom_#{:erlang.unique_integer([:positive])}_#{i}")
      end
    end)
  end

  def handle_event("stress-processes", _params, socket) do
    spawn_stress(socket, fn ->
      for _ <- 1..1000 do
        spawn(fn ->
          receive do
            :stop -> :ok
          after
            30_000 -> :ok
          end
        end)
      end
    end)
  end

  def handle_event("stress-messages", _params, socket) do
    parent = self()

    spawn_stress(socket, fn ->
      for _ <- 1..1000 do
        spawn(fn ->
          send(parent, {:stress_msg, :erlang.unique_integer()})
        end)
      end
    end)
  end

  def handle_event("stress-binaries", _params, socket) do
    spawn_stress(socket, fn ->
      for _ <- 1..100 do
        _bin = :binary.copy(<<"x">>, 10_000)
      end
    end)
  end

  defp initial_assigns do
    %{
      current_route: :runtime_stats,
      auto_refresh: true,
      session_started_at: System.monotonic_time(:millisecond),
      session_uptime_ms: 0,
      refresh_timer_ref: nil,
      erlang_stats: %{},
      js_stats: %{},
      js_stats_receiver: nil,
      current_js_sample_id: nil,
      process_snapshot: [],
      previous_process_ids: MapSet.new(),
      process_delta: @initial_process_delta,
      available_probes: @probe_keys,
      failed_probes: []
    }
  end

  defp maybe_connect(socket) do
    if connected?(socket) do
      socket
      |> ensure_js_stats_receiver()
      |> request_js_stats()
      |> refresh_stats()
      |> schedule_refresh()
    else
      socket
    end
  end

  defp maybe_refresh_from_timer(socket) do
    if socket.assigns.auto_refresh do
      socket
      |> refresh_stats()
      |> schedule_refresh()
    else
      socket
    end
  end

  defp set_auto_refresh(socket, true) do
    socket
    |> assign(:auto_refresh, true)
    |> refresh_stats()
    |> schedule_refresh()
  end

  defp set_auto_refresh(socket, false) do
    socket
    |> assign(:auto_refresh, false)
    |> cancel_refresh_timer()
  end

  defp spawn_stress(socket, fun) do
    spawn(fun)
    {:noreply, socket}
  end

  defp schedule_refresh(socket) do
    socket = cancel_refresh_timer(socket)
    timer_ref = Process.send_after(self(), :refresh, @refresh_interval)
    assign(socket, :refresh_timer_ref, timer_ref)
  end

  defp cancel_refresh_timer(%{assigns: %{refresh_timer_ref: nil}} = socket), do: socket

  defp cancel_refresh_timer(socket) do
    Process.cancel_timer(socket.assigns.refresh_timer_ref)
    assign(socket, :refresh_timer_ref, nil)
  end

  defp refresh_stats(socket) do
    socket
    |> update_session_uptime()
    |> collect_runtime_stats()
  end

  defp update_session_uptime(socket) do
    elapsed = System.monotonic_time(:millisecond) - socket.assigns.session_started_at
    assign(socket, :session_uptime_ms, elapsed)
  end

  defp request_js_stats(socket) do
    sample_id = System.unique_integer([:positive])

    socket
    |> assign(:current_js_sample_id, sample_id)
    |> fetch_js_stats(sample_id)
  end

  # Probe directly in the LiveView process so the page does not inflate its own
  # process_count metric by spawning helper samplers on every refresh.
  defp collect_runtime_stats(socket) do
    {stats, failed_probes} =
      Enum.reduce(socket.assigns.available_probes, {%{}, socket.assigns.failed_probes}, &collect_probe/2)

    socket
    |> assign(
      erlang_stats: stats,
      failed_probes: failed_probes,
      available_probes: Enum.reject(socket.assigns.available_probes, &(&1 in failed_probes))
    )
    |> update_process_snapshot()
  end

  defp collect_probe(key, {stats, failed_probes}) do
    case safe_run_probe(key) do
      {:ok, value} ->
        {Map.put(stats, key, value), failed_probes}

      :error ->
        {stats, append_unique(failed_probes, key)}
    end
  end

  defp safe_run_probe(key) do
    if probe_supported?(key) do
      safe_call(fn -> {:ok, run_probe(key)} end, :error)
    else
      :error
    end
  end

  defp probe_supported?(:platform) do
    Code.ensure_loaded?(:atomvm) and function_exported?(:atomvm, :platform, 0)
  end

  defp probe_supported?(:memory), do: function_exported?(:erlang, :memory, 0)
  defp probe_supported?(_key), do: true

  defp update_process_snapshot(socket) do
    pids = safe_processes()
    current_ids = MapSet.new(pids)
    previous_ids = socket.assigns.previous_process_ids

    assign(socket,
      process_snapshot: build_process_snapshot(pids),
      previous_process_ids: current_ids,
      process_delta: build_process_delta(previous_ids, current_ids, length(pids))
    )
  end

  defp build_process_snapshot(pids) do
    pids
    |> Enum.map(&process_snapshot_entry/1)
    |> Enum.sort_by(fn entry -> {-entry.memory, -entry.message_queue_len, entry.pid} end)
    |> Enum.take(@max_process_snapshot)
  end

  defp build_process_delta(previous_ids, current_ids, total) do
    %{
      added: added_process_ids(previous_ids, current_ids),
      removed_count: MapSet.size(MapSet.difference(previous_ids, current_ids)),
      total: total
    }
  end

  defp added_process_ids(previous_ids, current_ids) do
    if MapSet.size(previous_ids) == 0 do
      []
    else
      current_ids
      |> MapSet.difference(previous_ids)
      |> MapSet.to_list()
      |> Enum.map(&:erlang.pid_to_list/1)
      |> Enum.sort()
      |> Enum.take(@max_recent_processes)
    end
  end

  defp process_snapshot_entry(pid) do
    %{
      pid: :erlang.pid_to_list(pid),
      memory: process_info_value(pid, :memory),
      message_queue_len: process_info_value(pid, :message_queue_len),
      heap_size: process_info_value(pid, :heap_size),
      stack_size: process_info_value(pid, :stack_size)
    }
  end

  defp safe_processes do
    if function_exported?(:erlang, :processes, 0) do
      safe_call(fn -> :erlang.processes() end, [])
    else
      []
    end
  end

  defp process_info_value(pid, key) do
    safe_call(
      fn ->
        case :erlang.process_info(pid, key) do
          {^key, value} when is_integer(value) -> value
          _ -> 0
        end
      end,
      0
    )
  end

  defp ensure_js_stats_receiver(%{assigns: %{js_stats_receiver: receiver}} = socket)
       when not is_nil(receiver),
       do: socket

  defp ensure_js_stats_receiver(socket) do
    with receiver when not is_nil(receiver) <- pick_js_stats_receiver() do
      lv = self()
      spawn(fn -> start_js_stats_receiver(receiver, lv) end)

      receive do
        {:js_stats_receiver_ready, ^receiver} ->
          assign(socket, :js_stats_receiver, receiver)

        {:js_stats_receiver_unavailable, ^receiver} ->
          socket
      after
        1_000 ->
          socket
      end
    else
      _ -> socket
    end
  end

  defp pick_js_stats_receiver do
    Enum.find(@js_stats_receivers, &(Process.whereis(&1) == nil))
  end

  defp start_js_stats_receiver(receiver, lv) do
    case safe_register(receiver) do
      :ok ->
        send(lv, {:js_stats_receiver_ready, receiver})
        ref = Process.monitor(lv)

        try do
          js_stats_receiver_loop(lv, ref)
        after
          safe_unregister(receiver)
        end

      :error ->
        send(lv, {:js_stats_receiver_unavailable, receiver})
    end
  end

  defp safe_register(receiver) do
    safe_call(
      fn ->
        Process.register(self(), receiver)
        :ok
      end,
      :error
    )
  end

  defp safe_unregister(receiver) do
    safe_call(
      fn ->
        Process.unregister(receiver)
        :ok
      end,
      :ok
    )
  end

  defp js_stats_receiver_loop(lv, ref) do
    receive do
      {:DOWN, ^ref, :process, ^lv, _reason} ->
        :ok

      {:emscripten, _} = raw ->
        handle_js_stats_message(raw, lv)
        js_stats_receiver_loop(lv, ref)

      _msg ->
        js_stats_receiver_loop(lv, ref)
    end
  end

  defp handle_js_stats_message(raw, lv) do
    safe_call(
      fn ->
        case Popcorn.Wasm.parse_message!(raw) do
          {:wasm_cast, %{"js_stats" => stats, "sample_id" => sample_id}} ->
            send(lv, {:js_stats, sample_id, stats})

          _ ->
            :ok
        end
      end,
      :ok
    )
  end

  defp fetch_js_stats(%{assigns: %{js_stats_receiver: nil}} = socket, _sample_id), do: socket

  defp fetch_js_stats(socket, sample_id) do
    safe_call(
      fn ->
        Popcorn.Wasm.run_js!(@js_stats_script, %{
          receiver: socket.assigns.js_stats_receiver,
          sample_id: sample_id
        })
      end,
      :ok
    )

    socket
  end

  defp safe_call(fun, fallback) do
    fun.()
  rescue
    _ -> fallback
  catch
    _, _ -> fallback
  end

  defp append_unique(list, value) do
    if value in list, do: list, else: [value | list]
  end

  defp has_erlang_memory?(stats), do: match?([_ | _], stats[:memory])

  defp format_bytes(nil), do: "N/A"

  defp format_bytes(bytes) when is_integer(bytes) and bytes >= 1_073_741_824 do
    whole = div(bytes, 1_073_741_824)
    frac = div(rem(bytes, 1_073_741_824) * 100, 1_073_741_824)
    "#{whole}.#{String.pad_leading(to_string(frac), 2, "0")} GiB"
  end

  defp format_bytes(bytes) when is_integer(bytes) and bytes >= 1_048_576 do
    whole = div(bytes, 1_048_576)
    frac = div(rem(bytes, 1_048_576) * 100, 1_048_576)
    "#{whole}.#{String.pad_leading(to_string(frac), 2, "0")} MiB"
  end

  defp format_bytes(bytes) when is_integer(bytes) and bytes >= 1_024 do
    whole = div(bytes, 1_024)
    frac = div(rem(bytes, 1_024) * 100, 1_024)
    "#{whole}.#{String.pad_leading(to_string(frac), 2, "0")} KiB"
  end

  defp format_bytes(bytes) when is_integer(bytes), do: "#{bytes} B"
  defp format_bytes(_bytes), do: "N/A"

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

  defp format_uptime(_ms), do: "N/A"

  defp stat_value(stats, key, default \\ "N/A"), do: Map.get(stats, key, default)
  defp js_stat(stats, key), do: Map.get(stats, key)

  defp memory_entry(stats, key) do
    case List.keyfind(Map.get(stats, :memory, []), key, 0) do
      {^key, value} -> value
      nil -> nil
    end
  end

  defp memory_value(stats, key), do: stats |> memory_entry(key) |> format_bytes()

  defp primary_memory_label(erlang_stats, js_stats) do
    cond do
      is_integer(memory_entry(erlang_stats, :total)) -> "Erlang Total Memory"
      is_integer(js_stat(js_stats, "wasmMemoryBytes")) -> "WASM Linear Memory"
      is_integer(js_stat(js_stats, "usedJSHeapSize")) -> "Browser JS Heap"
      true -> "Memory"
    end
  end

  defp primary_memory_value(erlang_stats, js_stats) do
    cond do
      is_integer(memory_entry(erlang_stats, :total)) ->
        format_bytes(memory_entry(erlang_stats, :total))

      is_integer(js_stat(js_stats, "wasmMemoryBytes")) ->
        format_bytes(js_stat(js_stats, "wasmMemoryBytes"))

      is_integer(js_stat(js_stats, "usedJSHeapSize")) ->
        format_bytes(js_stat(js_stats, "usedJSHeapSize"))

      true ->
        "N/A"
    end
  end

  defp primary_memory_note(erlang_stats, js_stats) do
    cond do
      is_integer(memory_entry(erlang_stats, :total)) -> "Direct from erlang:memory/0"
      is_integer(js_stat(js_stats, "wasmMemoryBytes")) -> "Direct from the module's linear memory"
      is_integer(js_stat(js_stats, "usedJSHeapSize")) -> "Chromium-only browser metric"
      true -> "Unavailable in this runtime/browser"
    end
  end

  defp system_info_rows(stats) do
    Enum.map(@system_info_fields, fn field ->
      %{
        label: field.label,
        value: format_system_info_value(stat_value(stats, field.key), Map.get(field, :formatter))
      }
    end)
  end

  defp browser_rows(js_stats) do
    @browser_fields
    |> Enum.map(fn field ->
      %{
        label: field.label,
        value: js_stat(js_stats, field.key),
        full_width: Map.get(field, :full_width, false)
      }
    end)
    |> Enum.reject(&is_nil(&1.value))
  end

  defp js_memory_rows(js_stats) do
    @js_memory_fields
    |> Enum.map(fn field ->
      value = js_stat(js_stats, field.key)

      %{
        label: field.label,
        value: format_row_value(value, Map.get(field, :formatter)),
        class: Map.get(field, :class)
      }
    end)
    |> Enum.reject(&is_nil(&1.value))
  end

  defp format_system_info_value(value, :word_size), do: word_size_label(value)
  defp format_system_info_value(value, _formatter), do: value

  defp format_row_value(value, :bytes), do: format_bytes(value)
  defp format_row_value(value, _formatter), do: value

  @impl true
  def render(assigns) do
    ~H"""
    <.header>
      Runtime Stats
      <:subtitle>
        Session stats for the AtomVM runtime in this page, plus a few browser-side metrics when the
        browser exposes them.
      </:subtitle>
    </.header>

    <.controls auto_refresh={@auto_refresh} />
    <.stress_test />
    <.key_metrics
      erlang_stats={@erlang_stats}
      js_stats={@js_stats}
      session_uptime_ms={@session_uptime_ms}
    />
    <.system_info_card erlang_stats={@erlang_stats} />
    <.erlang_memory_card :if={has_erlang_memory?(@erlang_stats)} erlang_stats={@erlang_stats} />
    <.js_memory_card :if={@js_stats != %{}} js_stats={@js_stats} />
    <.browser_info_card :if={@js_stats != %{}} js_stats={@js_stats} />
    <.failed_probes_card :if={@failed_probes != []} failed_probes={@failed_probes} />
    <.process_snapshot_card
      :if={@process_snapshot != []}
      process_snapshot={@process_snapshot}
      process_delta={@process_delta}
    />
    """
  end

  defp controls(assigns) do
    ~H"""
    <div class="flex flex-wrap gap-2 my-4">
      <label class="label cursor-pointer gap-2">
        <span class="label-text">Auto-refresh</span>
        <input
          type="checkbox"
          class="toggle toggle-sm toggle-primary"
          checked={@auto_refresh}
          phx-click="toggle-auto-refresh"
        />
      </label>
      <span class="text-xs text-base-content/70 self-center">
        Manual refresh is disabled for now because click-triggered samples appear to leak runtime
        processes.
      </span>
    </div>
    """
  end

  defp stress_test(assigns) do
    assigns = assign(assigns, :actions, @stress_actions)

    ~H"""
    <div class="card bg-base-200 shadow mb-6">
      <div class="card-body p-4">
        <h3 class="card-title text-sm">Stress Test</h3>
        <div class="flex flex-wrap gap-2">
          <.button
            :for={action <- @actions}
            phx-click={action.event}
            class="btn btn-sm btn-warning btn-outline"
          >
            {action.label}
          </.button>
        </div>
      </div>
    </div>
    """
  end

  defp key_metrics(assigns) do
    ~H"""
    <div class="stats stats-vertical sm:stats-horizontal shadow w-full mb-6">
      <div class="stat">
        <div class="stat-title">{primary_memory_label(@erlang_stats, @js_stats)}</div>
        <div class="stat-value text-lg">{primary_memory_value(@erlang_stats, @js_stats)}</div>
        <div class="stat-desc">{primary_memory_note(@erlang_stats, @js_stats)}</div>
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
        <div class="stat-title">Session Uptime</div>
        <div class="stat-value text-lg">{format_uptime(@session_uptime_ms)}</div>
        <div class="stat-desc">Time since this LiveView mounted</div>
      </div>
    </div>
    """
  end

  defp system_info_card(assigns) do
    assigns = assign(assigns, :rows, system_info_rows(assigns.erlang_stats))

    ~H"""
    <div class="card bg-base-200 shadow mb-6">
      <div class="card-body p-4">
        <h3 class="card-title text-sm">System Info</h3>
        <div class="grid grid-cols-2 sm:grid-cols-3 gap-2 text-sm">
          <div :for={row <- @rows}>
            <span class="text-base-content/60">{row.label}:</span>
            <span class="font-mono">{row.value}</span>
          </div>
        </div>
      </div>
    </div>
    """
  end

  defp erlang_memory_card(assigns) do
    assigns = assign(assigns, :categories, @memory_categories)

    ~H"""
    <div class="card bg-base-200 shadow mb-6">
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
              <tr :for={{key, label} <- @categories}>
                <td>{label}</td>
                <td class="text-right font-mono">{memory_value(@erlang_stats, key)}</td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    </div>
    """
  end

  defp js_memory_card(assigns) do
    assigns = assign(assigns, :rows, js_memory_rows(assigns.js_stats))

    ~H"""
    <div class="card bg-base-200 shadow mb-6">
      <div class="card-body p-4">
        <h3 class="card-title text-sm">WASM / Browser Memory</h3>
        <p class="text-xs text-base-content/70">
          WASM memory is read from the module's linear memory. JS heap numbers come from
          <code class="font-mono">performance.memory</code> and are usually only available in Chromium-based
          browsers.
        </p>
        <div class="overflow-x-auto">
          <table class="table table-sm table-zebra">
            <thead>
              <tr>
                <th>Category</th>
                <th class="text-right">Size</th>
              </tr>
            </thead>
            <tbody>
              <tr :for={row <- @rows}>
                <td>{row.label}</td>
                <td class={["text-right font-mono", row.class]}>{row.value}</td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    </div>
    """
  end

  defp browser_info_card(assigns) do
    assigns = assign(assigns, :rows, browser_rows(assigns.js_stats))

    ~H"""
    <div class="card bg-base-200 shadow mb-6">
      <div class="card-body p-4">
        <h3 class="card-title text-sm">Browser</h3>
        <div class="grid grid-cols-2 sm:grid-cols-3 gap-2 text-sm">
          <div :for={row <- @rows} class={if row.full_width, do: "col-span-full", else: nil}>
            <span class="text-base-content/60">{row.label}:</span>
            <div class={if row.full_width, do: "font-mono text-xs break-all mt-1", else: "font-mono"}>
              {row.value}
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end

  defp failed_probes_card(assigns) do
    ~H"""
    <div class="card bg-base-200 shadow mb-6">
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

  defp process_snapshot_card(assigns) do
    ~H"""
    <div class="card bg-base-200 shadow mb-6">
      <div class="card-body p-4">
        <h3 class="card-title text-sm">Process Snapshot</h3>
        <p class="text-xs text-base-content/70">
          Current processes from <code class="font-mono">erlang:processes/0</code>. This helps spot
          which pids remain after each refresh.
        </p>
        <div class="text-xs text-base-content/70">
          Total: {@process_delta.total} |
          Added since last refresh: {length(@process_delta.added)} |
          Removed: {@process_delta.removed_count}
        </div>
        <div :if={@process_delta.added != []} class="mt-2">
          <div class="text-xs font-semibold">Recently added pids</div>
          <div class="font-mono text-[11px] break-all">{Enum.join(@process_delta.added, ", ")}</div>
        </div>
        <div class="overflow-x-auto mt-3">
          <table class="table table-sm table-zebra">
            <thead>
              <tr>
                <th>Pid</th>
                <th class="text-right">Memory</th>
                <th class="text-right">Msgs</th>
                <th class="text-right">Heap</th>
                <th class="text-right">Stack</th>
              </tr>
            </thead>
            <tbody>
              <tr :for={proc <- @process_snapshot}>
                <td class="font-mono text-xs">{proc.pid}</td>
                <td class="text-right font-mono">{format_bytes(proc.memory)}</td>
                <td class="text-right font-mono">{proc.message_queue_len}</td>
                <td class="text-right font-mono">{proc.heap_size}</td>
                <td class="text-right font-mono">{proc.stack_size}</td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    </div>
    """
  end

  defp word_size_label(4), do: "32-bit"
  defp word_size_label(8), do: "64-bit"
  defp word_size_label(value), do: to_string(value)
end
