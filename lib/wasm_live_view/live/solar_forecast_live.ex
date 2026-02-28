defmodule WasmLiveView.SolarForecastLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents

  @storage_key "solar_forecast_cache"
  @settings_key "solar_forecast_settings"
  # forecast.solar free tier: 10 requests per 30 minutes
  @cache_ttl_s 30 * 60

  @impl true
  def mount(_params, _session, socket) do
    {panel_size_kw, direction, tilt} = load_settings()

    socket =
      assign(socket,
        current_route: :solar_forecast,
        loading: false,
        error: nil,
        location: nil,
        solar_data: nil,
        forecast_data: nil,
        combined_chart_ops: [],
        fetch_timer: nil,
        cache: load_cache(),
        panel_size_kw: panel_size_kw,
        direction: direction,
        tilt: tilt
      )

    socket =
      if connected?(socket),
        do: push_event(socket, "check-permission", %{}),
        else: socket

    {:ok, socket}
  end

  @impl true
  def handle_event("permission-result", %{"granted" => true}, socket) do
    {:noreply,
     socket
     |> assign(loading: true, error: nil, solar_data: nil, location: nil)
     |> push_event("request-location", %{})}
  end

  def handle_event("permission-result", %{"granted" => false}, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("get-location", _params, socket) do
    {:noreply,
     socket
     |> assign(loading: true, error: nil, solar_data: nil, location: nil)
     |> push_event("request-location", %{})}
  end

  @impl true
  def handle_event("location-found", %{"lat" => lat, "lon" => lon}, socket) do
    socket = assign(socket, location: %{lat: to_float(lat), lon: to_float(lon)})
    do_fetch_or_cache(socket)
  end

  @impl true
  def handle_event("location-error", %{"error" => error}, socket) do
    {:noreply, assign(socket, loading: false, error: "Location error: #{error}")}
  end

  @impl true
  def handle_event("panel-watts-changed", %{"value" => value}, socket) do
    {:noreply, socket |> assign(panel_size_kw: to_float(value) / 1000.0) |> save_and_schedule()}
  end

  @impl true
  def handle_event("direction-changed", %{"direction" => value}, socket) do
    {:noreply, socket |> assign(direction: to_float(value)) |> save_and_schedule()}
  end

  @impl true
  def handle_event("tilt-changed", %{"tilt" => value}, socket) do
    {:noreply, socket |> assign(tilt: to_float(value)) |> save_and_schedule()}
  end

  @impl true
  def handle_info({:solar_result, {:ok, data}, key}, socket) do
    cache = cache_put(socket.assigns.cache, key, data)
    save_cache(cache)

    forecast = format_forecast_data(data)

    {:noreply,
     assign(socket,
       loading: false,
       solar_data: data,
       forecast_data: forecast,
       combined_chart_ops: build_combined_chart(forecast.today_period, forecast.tomorrow_period, socket.assigns.panel_size_kw * 1000),
       cache: cache
     )}
  end

  def handle_info({:solar_result, {:error, reason}, _key}, socket) do
    {:noreply, assign(socket, loading: false, error: reason)}
  end

  @impl true
  def handle_info(:scheduled_fetch, socket) do
    do_fetch_or_cache(assign(socket, fetch_timer: nil))
  end

  # --- cache ---

  defp cache_key(lat, lon, tilt, direction, panel_size_kw) do
    # Float.round/2 is not available in AtomVM — round via integer arithmetic
    lat_r = round(lat * 100) / 100.0
    lon_r = round(lon * 100) / 100.0
    {lat_r, lon_r, round(tilt), round(direction), panel_size_kw}
  end

  defp cache_get(cache, key) do
    now = :erlang.system_time(:second)

    case Map.get(cache, key) do
      {data, ts} when now - ts < @cache_ttl_s -> {:hit, data}
      _ -> :miss
    end
  end

  defp cache_put(cache, key, data) do
    Map.put(cache, key, {data, :erlang.system_time(:second)})
  end

  defp load_cache do
    case Popcorn.Wasm.run_js(
           "({ args }) => { return [localStorage.getItem(args.key)]; }",
           %{key: @storage_key},
           return: :value
         ) do
      {:ok, nil} ->
        %{}

      {:ok, json} ->
        now = :erlang.system_time(:second)

        json
        |> Jason.decode!()
        |> Enum.reduce(%{}, fn entry, acc ->
          [lat, lon, tilt, dir, kwp] = entry["key"]
          ts = entry["ts"]

          if now - ts < @cache_ttl_s do
            Map.put(acc, {lat, lon, tilt, dir, kwp}, {entry["data"], ts})
          else
            acc
          end
        end)

      _ ->
        %{}
    end
  end

  defp save_cache(cache) do
    entries =
      Enum.map(cache, fn {{lat, lon, tilt, dir, kwp}, {data, ts}} ->
        %{key: [lat, lon, tilt, dir, kwp], data: data, ts: ts}
      end)

    json = Jason.encode!(entries)

    Popcorn.Wasm.run_js(
      "({ args }) => { localStorage.setItem(args.key, args.json); return []; }",
      %{key: @storage_key, json: json}
    )
  end

  # --- settings ---

  defp load_settings do
    case Popcorn.Wasm.run_js(
           "({ args }) => { return [localStorage.getItem(args.key)]; }",
           %{key: @settings_key},
           return: :value
         ) do
      {:ok, nil} ->
        {5.0, 0, 30}

      {:ok, json} ->
        %{"panel_size_kw" => p, "direction" => d, "tilt" => t} = Jason.decode!(json)
        {to_float(p), to_float(d), to_float(t)}

      _ ->
        {5.0, 0, 30}
    end
  end

  defp save_settings(panel_size_kw, direction, tilt) do
    json = Jason.encode!(%{panel_size_kw: panel_size_kw, direction: direction, tilt: tilt})

    Popcorn.Wasm.run_js(
      "({ args }) => { localStorage.setItem(args.key, args.json); return []; }",
      %{key: @settings_key, json: json}
    )
  end

  # --- fetch logic ---

  defp save_and_schedule(socket) do
    %{panel_size_kw: p, direction: d, tilt: t} = socket.assigns
    save_settings(p, d, t)
    schedule_fetch(socket)
  end

  defp schedule_fetch(socket) do
    if socket.assigns.fetch_timer, do: Process.cancel_timer(socket.assigns.fetch_timer)
    ref = Process.send_after(self(), :scheduled_fetch, 500)
    assign(socket, fetch_timer: ref)
  end

  defp do_fetch_or_cache(socket) do
    case socket.assigns.location do
      nil ->
        {:noreply, socket}

      location ->
        %{panel_size_kw: panel_size_kw, tilt: tilt, direction: direction} = socket.assigns
        key = cache_key(location.lat, location.lon, tilt, direction, panel_size_kw)

        case cache_get(socket.assigns.cache, key) do
          {:hit, data} ->
            forecast = format_forecast_data(data)

            {:noreply,
             assign(socket,
               loading: false,
               solar_data: data,
               forecast_data: forecast,
               combined_chart_ops: build_combined_chart(forecast.today_period, forecast.tomorrow_period, socket.assigns.panel_size_kw * 1000)
             )}

          :miss ->
            lv = self()

            spawn(fn ->
              result =
                fetch_solar_forecast(location.lat, location.lon, panel_size_kw, tilt, direction)

              send(lv, {:solar_result, result, key})
            end)

            {:noreply, assign(socket, loading: true, error: nil)}
        end
    end
  end

  # --- private ---

  defp to_float(val) when is_float(val), do: val
  defp to_float(val) when is_integer(val), do: val * 1.0

  defp to_float(val) when is_binary(val) do
    if String.contains?(val, "."),
      do: String.to_float(val),
      else: String.to_integer(val) * 1.0
  end

  defp fetch_solar_forecast(lat, lon, panel_size_kw, tilt, direction) do
    try do
      # forecast.solar doesn't support CORS, use cors-anywhere proxy
      base_url = "https://corsproxy.io/?url=https://api.forecast.solar"
      url = "#{base_url}/estimate/#{lat}/#{lon}/#{tilt}/#{direction}/#{panel_size_kw}"

      resp =
        Req.get!(url,
          adapter: &WasmLiveView.WasmFetchAdapter.run/1,
          decode_body: false
        )

      case resp.status do
        200 ->
          data = Jason.decode!(resp.body)
          {:ok, data}

        _ ->
          {:error, "Solar API returned status #{resp.status}"}
      end
    catch
      _, reason -> {:error, inspect(reason)}
    end
  end

  defp format_forecast_data(data) do
    result = data["result"]

    # Peak instantaneous power in W — already scaled for kWp by the API
    {peak_dt, peak_w} = result["watts"] |> Enum.max_by(fn {_dt, w} -> w end)
    peak_time = peak_dt |> String.split(" ") |> List.last() |> String.slice(0, 5)

    # Daily totals in kWh, sorted by date
    day_entries =
      result["watt_hours_day"]
      |> Map.to_list()
      |> Enum.sort_by(fn {date, _} -> date end)
      |> Enum.map(fn {date, wh} -> {date, wh / 1000.0} end)

    {today, today_kwh} = List.first(day_entries)
    tomorrow = Enum.at(day_entries, 1)

    period_entries =
      (result["watt_hours_period"] || %{})
      |> Map.to_list()
      |> Enum.sort_by(fn {dt, _} -> dt end)

    parse_period = fn date ->
      period_entries
      |> Enum.filter(fn {dt, _} -> String.starts_with?(dt, date) end)
      |> Enum.map(fn {dt, wh} ->
        time = dt |> String.split(" ") |> List.last() |> String.slice(0, 5)
        {time, wh}
      end)
    end

    today_period = parse_period.(today)
    tomorrow_period = if tomorrow, do: parse_period.(elem(tomorrow, 0)), else: []

    %{peak_w: peak_w, peak_time: peak_time, today: today, today_kwh: today_kwh, tomorrow: tomorrow, days: day_entries, today_period: today_period, tomorrow_period: tomorrow_period}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <.header>
      Solar Forecast
      <:subtitle>Plan your solar panel installation with real solar data from forecast.solar</:subtitle>
    </.header>

    <div class="mt-6">
      <.button phx-click="get-location" disabled={@loading}>
        {if @loading, do: "Getting forecast...", else: "Get Solar Forecast"}
      </.button>
      <div id="geolocation" phx-hook="Geolocation"></div>
    </div>

    <div :if={@error} class="alert alert-error mt-4">
      {@error}
    </div>

    <div :if={@location && @loading && is_nil(@solar_data)} class="mt-8">
      <p>Fetching solar forecast for {@location.lat}, {@location.lon}...</p>
    </div>

    <div :if={@solar_data} class="mt-8 space-y-6">
      <!-- Solar Panel Configuration -->
      <div class="card bg-base-200 shadow-xl">
        <div class="card-body">
          <h2 class="card-title">Solar Panel Configuration</h2>

          <form>
            <!-- Panel Size -->
            <div class="form-control w-full">
              <div class="flex justify-between items-center mb-1">
                <span class="label-text text-sm">Panel Size</span>
                <span class="flex items-center gap-1">
                  <input
                    type="number"
                    name="panel_watts"
                    value={round(@panel_size_kw * 1000)}
                    phx-blur="panel-watts-changed"
                    class="input input-xs input-bordered w-20 text-right font-mono"
                    min="1"
                    step="1"
                  />
                  <span class="text-xs text-base-content/40">W</span>
                </span>
              </div>
            </div>

            <!-- Direction -->
            <div class="form-control w-full mt-2">
              <div class="flex justify-between items-center mb-1">
                <span class="label-text text-sm">Direction (Azimuth)</span>
                <span class="badge badge-neutral font-mono text-xs">{@direction}°</span>
              </div>
              <input
                type="range"
                name="direction"
                min="-180"
                max="180"
                step="5"
                value={@direction}
                phx-change="direction-changed"
                class="range range-xs range-primary w-full"
              />
              <div class="flex w-full justify-between px-1 mt-1 text-xs text-base-content/40">
                <span>N</span>
                <span>E</span>
                <span>S</span>
                <span>W</span>
                <span>N</span>
              </div>
            </div>

            <!-- Tilt -->
            <div class="form-control w-full mt-2">
              <div class="flex justify-between items-center mb-1">
                <span class="label-text text-sm">Tilt</span>
                <span class="badge badge-neutral font-mono text-xs">{@tilt}°</span>
              </div>
              <input
                type="range"
                name="tilt"
                min="0"
                max="90"
                step="5"
                value={@tilt}
                phx-change="tilt-changed"
                class="range range-xs range-primary w-full"
              />
              <div class="flex w-full justify-between px-1 mt-1 text-xs text-base-content/40">
                <span>Flat</span>
                <span>45°</span>
                <span>Vertical</span>
              </div>
            </div>
          </form>
        </div>
      </div>

      <!-- Forecast Summary -->
      <div class="card bg-base-200 shadow-xl">
        <div class="card-body">
          <h2 class="card-title">Today's Forecast</h2>
          <div class="grid grid-cols-2 gap-4">
            <div class="stat">
              <div class="stat-title">Peak Power</div>
              <div class="stat-value text-2xl">{@forecast_data.peak_w}</div>
              <div class="stat-desc">Watts at {@forecast_data.peak_time}</div>
            </div>

            <div class="stat">
              <div class="stat-title">Daily Output</div>
              <div class="stat-value text-2xl">{format_energy(@forecast_data.today_kwh)}</div>
              <div class="stat-desc">{energy_unit(@forecast_data.today_kwh)}</div>
            </div>
          </div>
          <div :if={@forecast_data.tomorrow} class="mt-3 pt-2 border-t border-base-300 text-xs text-base-content/50">
            Tomorrow ({elem(@forecast_data.tomorrow, 0)}): <span class="font-medium">{format_energy(elem(@forecast_data.tomorrow, 1))} {energy_unit(elem(@forecast_data.tomorrow, 1))}</span>
          </div>
        </div>
      </div>

      <!-- Combined Hourly Chart -->
      <div class="card bg-base-200 shadow-xl">
        <div class="card-body">
          <h2 class="card-title">Hourly Output</h2>
          <canvas
            id="combined-chart"
            phx-hook="EaselCanvas"
            width="560"
            height="280"
            data-ops={Jason.encode!(@combined_chart_ops)}
            class="w-full max-w-[560px]"
          />
        </div>
      </div>

      <!-- Forecast by Day -->
      <div class="card bg-base-200 shadow-xl">
        <div class="card-body">
          <h2 class="card-title">Forecast by Day</h2>
          <div class="overflow-x-auto">
            <table class="table table-sm">
              <thead>
                <tr>
                  <th>Date</th>
                  <th>Est. Output</th>
                </tr>
              </thead>
              <tbody>
                <tr :for={{date, kwh} <- @forecast_data.days}>
                  <td>{date}</td>
                  <td>{format_energy(kwh)} {energy_unit(kwh)}</td>
                </tr>
              </tbody>
            </table>
          </div>
        </div>
      </div>
    </div>
    """
  end

  defp build_combined_chart(today_period, tomorrow_period, max_wh) do
    today_map = Map.new(today_period)
    tomorrow_map = Map.new(tomorrow_period)

    today_times = Map.keys(today_map) |> Enum.sort()
    tomorrow_times = Map.keys(tomorrow_map) |> Enum.sort()

    start_time =
      cond do
        today_times == [] or tomorrow_times == [] ->
          List.first(today_times ++ tomorrow_times) || "00:00"
        String.slice(hd(today_times), 0, 2) == String.slice(hd(tomorrow_times), 0, 2) ->
          if hd(today_times) > hd(tomorrow_times), do: hd(today_times), else: hd(tomorrow_times)
        true ->
          if hd(today_times) < hd(tomorrow_times), do: hd(today_times), else: hd(tomorrow_times)
      end

    end_time =
      cond do
        today_times == [] or tomorrow_times == [] ->
          List.last(today_times ++ tomorrow_times) || "23:59"
        String.slice(List.last(today_times), 0, 2) == String.slice(List.last(tomorrow_times), 0, 2) ->
          if List.last(today_times) < List.last(tomorrow_times), do: List.last(today_times), else: List.last(tomorrow_times)
        true ->
          if List.last(today_times) > List.last(tomorrow_times), do: List.last(today_times), else: List.last(tomorrow_times)
      end

    times =
      (today_times ++ tomorrow_times)
      |> Enum.uniq()
      |> Enum.sort()
      |> Enum.filter(fn t -> t >= start_time and t <= end_time end)

    bar_count = length(times)

    if bar_count == 0 do
      []
    else
      width = 560
      height = 280
      pad_l = 60
      pad_r = 15
      pad_t = 28
      pad_b = 50
      chart_w = width - pad_l - pad_r
      chart_h = height - pad_t - pad_b

      slot = chart_w / bar_count
      bar_w = max(slot * 0.44, 1.0)
      label_every = max(1, div(bar_count, 8))

      canvas =
        Easel.new(width, height)
        |> Easel.set_fill_style("#1d232a")
        |> Easel.fill_rect(0, 0, width, height)
        |> Easel.set_stroke_style("#4b5563")
        |> Easel.set_line_width(1)
        |> Easel.begin_path()
        |> Easel.move_to(pad_l, pad_t)
        |> Easel.line_to(pad_l, height - pad_b)
        |> Easel.line_to(width - pad_r, height - pad_b)
        |> Easel.stroke()

      grid_lines = 4

      canvas =
        Enum.reduce(0..grid_lines, canvas, fn i, acc ->
          y = pad_t + chart_h * (1 - i / grid_lines)
          val = max_wh * i / grid_lines
          label = "#{round(val)} Wh"

          acc
          |> Easel.set_stroke_style("#374151")
          |> Easel.set_line_width(0.5)
          |> Easel.begin_path()
          |> Easel.move_to(pad_l, y)
          |> Easel.line_to(width - pad_r, y)
          |> Easel.stroke()
          |> Easel.set_fill_style("#9ca3af")
          |> Easel.set_font("11px sans-serif")
          |> Easel.set_text_align("right")
          |> Easel.fill_text(label, pad_l - 6, y + 4)
        end)

      canvas =
        times
        |> Enum.with_index()
        |> Enum.reduce(canvas, fn {time, i}, acc ->
          today_wh = Map.get(today_map, time, 0)
          tomorrow_wh = Map.get(tomorrow_map, time, 0)
          slot_x = pad_l + i * slot + slot * 0.06

          # Today bar — amber gradient
          pct_t = min(today_wh / max_wh, 1.0)
          rt = round(42 + (245 - 42) * pct_t)
          gt = round(21 + (158 - 21) * pct_t)
          bt = round(0 + (11 - 0) * pct_t)
          bh_t = today_wh / max_wh * chart_h

          # Tomorrow bar — blue gradient
          pct_tm = min(tomorrow_wh / max_wh, 1.0)
          rtm = round(10 + (59 - 10) * pct_tm)
          gtm = round(20 + (130 - 20) * pct_tm)
          btm = round(50 + (246 - 50) * pct_tm)
          bh_tm = tomorrow_wh / max_wh * chart_h

          acc =
            acc
            |> Easel.set_fill_style("rgb(#{rt},#{gt},#{bt})")
            |> Easel.fill_rect(slot_x, height - pad_b - bh_t, bar_w, bh_t)
            |> Easel.set_fill_style("rgb(#{rtm},#{gtm},#{btm})")
            |> Easel.fill_rect(slot_x + bar_w + 1, height - pad_b - bh_tm, bar_w, bh_tm)

          if rem(i, label_every) == 0 do
            acc
            |> Easel.set_fill_style("#9ca3af")
            |> Easel.set_font("10px sans-serif")
            |> Easel.set_text_align("center")
            |> Easel.fill_text(time, slot_x + bar_w, height - pad_b + 14)
          else
            acc
          end
        end)

      # Legend
      canvas =
        canvas
        |> Easel.set_fill_style("rgb(245,158,11)")
        |> Easel.fill_rect(pad_l, 8, 10, 8)
        |> Easel.set_fill_style("#9ca3af")
        |> Easel.set_font("10px sans-serif")
        |> Easel.set_text_align("left")
        |> Easel.fill_text("Today", pad_l + 13, 16)
        |> Easel.set_fill_style("rgb(59,130,246)")
        |> Easel.fill_rect(pad_l + 55, 8, 10, 8)
        |> Easel.set_fill_style("#9ca3af")
        |> Easel.fill_text("Tomorrow", pad_l + 68, 16)

      canvas |> Easel.render() |> Map.get(:ops)
    end
  end

  defp format_energy(kwh) when kwh < 1.0, do: Integer.to_string(round(kwh * 1000))

  defp format_energy(kwh) do
    hundredths = round(kwh * 100)
    int_part = div(hundredths, 100)
    frac = rem(hundredths, 100)
    frac_str = if frac < 10, do: "0#{frac}", else: "#{frac}"
    "#{int_part}.#{frac_str}"
  end

  defp energy_unit(kwh) when kwh < 1.0, do: "Wh"
  defp energy_unit(_), do: "kWh"
end
