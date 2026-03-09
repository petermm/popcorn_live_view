defmodule WasmLiveView.Components.Geolocation do
  use Phoenix.Component

  @doc """
  Renders a hidden geolocation hook element.

  Responds to `check-permission` and `request-location` push_events,
  and pushes `permission-result`, `location-found`, or `location-error` back.
  """
  def geolocation(assigns) do
    ~H"""
    <div id="geolocation" phx-hook=".Geolocation"></div>

    <script :type={Phoenix.LiveView.ColocatedHook} name=".Geolocation">
    export default {
      mounted() {
        this._checkPermission = () => {
          if (!navigator.permissions) return;
          navigator.permissions
            .query({ name: "geolocation" })
            .then((result) => {
              if (result.state !== "denied") {
                this.pushEvent("permission-result", { granted: true });
              }
            })
            .catch(() => {});
        };

        this._requestLocation = () => {
          if (!navigator.geolocation) {
            this.pushEvent("location-error", {
              error: "Geolocation not supported",
            });
            return;
          }

          navigator.geolocation.getCurrentPosition(
            (position) => {
              this.pushEvent("location-found", {
                lat: position.coords.latitude,
                lon: position.coords.longitude,
              });
            },
            (err) => {
              this.pushEvent("location-error", { error: err.message });
            },
            { timeout: 15000, maximumAge: 300000 },
          );
        };

        this.handleEvent("check-permission", this._checkPermission);
        this.handleEvent("request-location", this._requestLocation);
      },
    };
    </script>
    """
  end
end
