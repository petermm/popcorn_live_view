defmodule WasmLiveView.WeatherLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents

  @impl true
  def mount(_params, _session, socket) do
    socket =
      assign(socket,
        current_route: :weather,
        loading: false,
        error: nil,
        weather: nil,
        location: nil
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
     |> assign(loading: true, error: nil, weather: nil, location: nil)
     |> push_event("request-location", %{})}
  end

  def handle_event("permission-result", %{"granted" => false}, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("get-location", _params, socket) do
    {:noreply,
     socket
     |> assign(loading: true, error: nil, weather: nil, location: nil)
     |> push_event("request-location", %{})}
  end

  @impl true
  def handle_event("location-found", %{"lat" => lat, "lon" => lon}, socket) do
    lat_f = to_float(lat)
    lon_f = to_float(lon)
    location = %{lat: lat_f, lon: lon_f}
    lv = self()

    spawn(fn ->
      result =
        try do
          url = "https://api.open-meteo.com/v1/forecast?latitude=#{lat_f}&longitude=#{lon_f}&current=temperature_2m,relative_humidity_2m,weather_code,wind_speed_10m"
          uri = URI.new!(url)
          resp = Req.get!(uri, adapter: &WasmLiveView.WasmFetchAdapter.run/1, decode_body: false)

          IO.inspect(resp)
          case resp.status do
            200 ->
              data = Jason.decode!(resp.body)
              IO.inspect(data)
              weather = parse_weather(data, lat_f, lon_f)
              IO.inspect(weather)
              {:ok, weather}

            _ ->
              {:error, "Weather API returned status #{resp.status}"}
          end
        catch
          _, reason -> {:error, inspect(reason)}
        end

      send(lv, {:weather_result, result, location})
    end)

    {:noreply, assign(socket, location: location)}
  end

  defp to_float(val) when is_float(val), do: val
  defp to_float(val) when is_binary(val), do: String.to_float(val)
  defp to_float(val) when is_integer(val), do: val * 1.0

  def handle_event("location-error", %{"error" => error}, socket) do
    {:noreply, assign(socket, loading: false, error: "Location error: #{error}")}
  end

  @impl true
  def handle_info({:weather_result, {:ok, weather}, location}, socket) do
    {:noreply, assign(socket, loading: false, weather: weather, location: location)}
  end

  def handle_info({:weather_result, {:error, reason}, _location}, socket) do
    {:noreply, assign(socket, loading: false, error: reason)}
  end

  defp parse_weather(data, lat, lon) do

    #data is:
    # %{"current" => %{"interval" => 900, "relative_humidity_2m" => 99, "temperature_2m" => 6.8,
    # "time" => "2026-02-27T15:00", "weather_code" => 51, "wind_speed_10m" => 14.0},
    # "current_units" => %{"interval" => "seconds", "relative_humidity_2m" => "%", "temperature_2m" => "°C", "time" => "iso8601", "weather_code" => "wmo code", "wind_speed_10m" => "km/h"},
    #  "elevation" => 12.0, "generationtime_ms" => 0.0672340393, "latitude" => 55.111111, "longitude" => 11.111111,
    # "timezone" => "GMT", "timezone_abbreviation" => "GMT", "utc_offset_seconds" => 0}

    current = data["current"]

    temp = to_number(current["temperature_2m"])
    wind = to_number(current["wind_speed_10m"])
    humidity = to_number(current["relative_humidity_2m"])
    code = to_number(current["weather_code"])

    %{
      temp: round(temp),
      temp_f: round(temp * 9 / 5 + 32),
      condition: weather_code_to_condition(code),
      humidity: humidity,
      wind: round(wind),
      location: "#{format_coord(lat)}, #{format_coord(lon)}",
      time: current["time"]
    }
  end

  defp format_coord(f) do
    int_part = trunc(f)
    frac = round(abs(f - int_part) * 100)
    frac_str = if frac < 10, do: "0#{frac}", else: "#{frac}"
    "#{int_part}.#{frac_str}"
  end

  defp to_number(val) when is_number(val), do: val
  defp to_number(val) when is_binary(val), do: String.to_float(val)

  defp weather_code_to_condition(code) when is_number(code) do
    case round(code) do
      0 -> "Clear sky"
      1 -> "Mainly clear"
      2 -> "Partly cloudy"
      3 -> "Overcast"
      45 -> "Foggy"
      48 -> "Depositing rime fog"
      51 -> "Light drizzle"
      53 -> "Moderate drizzle"
      55 -> "Dense drizzle"
      61 -> "Slight rain"
      63 -> "Moderate rain"
      65 -> "Heavy rain"
      71 -> "Slight snow"
      73 -> "Moderate snow"
      75 -> "Heavy snow"
      80 -> "Slight rain showers"
      81 -> "Moderate rain showers"
      82 -> "Violent rain showers"
      95 -> "Thunderstorm"
      96 -> "Thunderstorm with hail"
      99 -> "Thunderstorm with heavy hail"
      _ -> "Unknown"
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <.header>
      LocalWeather
      <:subtitle>Get your local weather using browser geolocation and the Open-Meteo API.</:subtitle>
    </.header>

    <div class="mt-6">
      <.button phx-click="get-location" disabled={@loading}>
        {if @loading, do: "Getting location...", else: "Get My Weather"}
      </.button>
      <div id="geolocation" phx-hook="Geolocation"></div>
    </div>

    <div :if={@error} class="alert alert-error mt-4">
      {@error}
    </div>

    <div :if={@weather} class="mt-8">
      <div class="card bg-base-200 shadow-xl">
        <div class="card-body">
          <h2 class="card-title text-2xl">{@weather.location}</h2>
          <p class="text-sm text-base-content/60">{@weather.time}</p>

          <div class="grid grid-cols-2 gap-4 mt-4">
            <div class="stat">
              <div class="stat-title">Temperature</div>
              <div class="stat-value text-4xl">{@weather.temp}°C</div>
              <div class="stat-desc">{@weather.temp_f}°F</div>
            </div>

            <div class="stat">
              <div class="stat-title">Condition</div>
              <div class="stat-value text-lg">{@weather.condition}</div>
            </div>

            <div class="stat">
              <div class="stat-title">Humidity</div>
              <div class="stat-value">{@weather.humidity}%</div>
            </div>

            <div class="stat">
              <div class="stat-title">Wind</div>
              <div class="stat-value">{@weather.wind} km/h</div>
            </div>
          </div>
        </div>
      </div>
    </div>

    <div :if={@location && @loading && is_nil(@weather)} class="mt-8">
      <p>Fetching weather for {@location.lat}, {@location.lon}...</p>
    </div>
    """
  end
end
