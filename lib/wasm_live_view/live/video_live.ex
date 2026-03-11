defmodule WasmLiveView.VideoLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents

  @player_src "https://stream.mux.com/BV3YZtogl89mg9VcNBhhnHm02Y34zI1nlMuMQfAbl3dM/highest.mp4"
  @video_actions ~w(toggle-play toggle-mute seek-back seek-forward restart)

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       current_route: :video,
       player_ready: false,
       player_playing: false,
       player_muted: false,
       current_time_s: 0,
       duration_s: 0,
       play_blocked_message: nil
     )}
  end

  @impl true
  def handle_event("video-command", %{"action" => action}, socket) when action in @video_actions do
    {:noreply,
     socket
     |> assign(:play_blocked_message, nil)
     |> push_event("video-command", %{action: action})}
  end

  def handle_event("video-state", params, socket) do
    player_playing = truthy?(params["playing"])

    {:noreply,
     assign(socket,
       player_ready: truthy?(params["ready"]),
       player_playing: player_playing,
       player_muted: truthy?(params["muted"]),
       current_time_s: whole_seconds(params["current_time"]),
       duration_s: whole_seconds(params["duration"]),
       play_blocked_message: if(player_playing, do: nil, else: socket.assigns.play_blocked_message)
     )}
  end

  def handle_event("video-play-blocked", %{"message" => message}, socket) do
    {:noreply, assign(socket, :play_blocked_message, message)}
  end

  @impl true
  def render(assigns) do
    player_src = @player_src
    player_state = player_state_label(assigns.player_ready, assigns.player_playing)
    play_label = if assigns.player_playing, do: "Pause", else: "Play"
    mute_label = if assigns.player_muted, do: "Unmute", else: "Mute"
    progress = progress_percent(assigns.current_time_s, assigns.duration_s)

    ~H"""
    <.header>
      Video
      <:subtitle>
        Video.js v10 HTML player loaded from jsDelivr with a colocated LiveView hook.
      </:subtitle>
    </.header>

    <section
      id="video-player-demo"
      class="space-y-4 rounded-2xl border border-base-300 bg-base-200/50 p-4 sm:p-6"
    >
      <div class="space-y-1">
        <h2 class="text-base font-semibold">Video.js v10 player</h2>
        <p class="text-sm text-base-content/70">
          The page loads the HTML renderer at mount time, then upgrades the custom elements in place.
        </p>
      </div>

      <div
        id="video-player-shell"
        data-role="player-shell"
        phx-hook=".VideoPlayer"
        phx-update="ignore"
        class="relative overflow-hidden rounded-2xl opacity-0 transition-opacity duration-300"
      >
        <div
          data-role="status"
          class="badge badge-neutral absolute right-3 top-3 z-10 border-0 bg-black/70 text-white shadow-sm"
        >
          Loading player...
        </div>
        <video-player class="block aspect-video w-full">
          <video-skin>
            <video
              slot="media"
              src={player_src}
              crossorigin="anonymous"
              playsinline
            >
            </video>
          </video-skin>
        </video-player>
      </div>

      <section class="space-y-4 rounded-2xl border border-base-300 bg-base-100/80 p-4">
        <div class="flex flex-col gap-2 sm:flex-row sm:items-center sm:justify-between">
          <div>
            <h3 class="text-sm font-semibold">LiveView controls</h3>
            <p class="text-sm text-base-content/70">
              These buttons are handled in Elixir and forwarded to the player with
              <code>push_event/3</code>.
            </p>
          </div>
          <div class="badge badge-outline">{player_state}</div>
        </div>

        <div class="flex flex-wrap gap-2">
          <.button
            phx-click="video-command"
            phx-value-action="toggle-play"
            disabled={!@player_ready}
            class="btn btn-sm btn-primary"
          >
            {play_label}
          </.button>
          <.button
            phx-click="video-command"
            phx-value-action="toggle-mute"
            disabled={!@player_ready}
            class="btn btn-sm"
          >
            {mute_label}
          </.button>
          <.button
            phx-click="video-command"
            phx-value-action="seek-back"
            disabled={!@player_ready}
            class="btn btn-sm"
          >
            -10s
          </.button>
          <.button
            phx-click="video-command"
            phx-value-action="seek-forward"
            disabled={!@player_ready}
            class="btn btn-sm"
          >
            +10s
          </.button>
          <.button
            phx-click="video-command"
            phx-value-action="restart"
            disabled={!@player_ready}
            class="btn btn-sm"
          >
            Restart
          </.button>
        </div>

        <div class="space-y-2">
          <progress class="progress progress-primary w-full" value={progress} max="100"></progress>
          <div class="flex items-center justify-between text-sm text-base-content/70">
            <span>{format_clock(@current_time_s)}</span>
            <span>{format_clock(@duration_s)}</span>
          </div>
        </div>

        <div class="grid gap-3 sm:grid-cols-3">
          <div class="rounded-xl border border-base-300 bg-base-200/60 p-3">
            <div class="text-xs uppercase tracking-wide text-base-content/60">State</div>
            <div class="mt-1 text-sm font-semibold">{player_state}</div>
          </div>
          <div class="rounded-xl border border-base-300 bg-base-200/60 p-3">
            <div class="text-xs uppercase tracking-wide text-base-content/60">Audio</div>
            <div class="mt-1 text-sm font-semibold">
              {if @player_muted, do: "Muted", else: "Sound on"}
            </div>
          </div>
          <div class="rounded-xl border border-base-300 bg-base-200/60 p-3">
            <div class="text-xs uppercase tracking-wide text-base-content/60">Position</div>
            <div class="mt-1 text-sm font-semibold">
              {format_clock(@current_time_s)} / {format_clock(@duration_s)}
            </div>
          </div>
        </div>

        <p :if={@play_blocked_message} class="text-sm text-warning">
          {@play_blocked_message}
        </p>
      </section>

      <p class="text-xs text-base-content/60">
        Source: Mux demo stream. Runtime assets:
        <code>@videojs/html/cdn/video.js</code>
        and
        <code>@videojs/html/cdn/video.css</code>.
      </p>

      <script :type={Phoenix.LiveView.ColocatedHook} name=".VideoPlayer">
      const VIDEO_JS_HTML_CDN =
        "https://cdn.jsdelivr.net/npm/@videojs/html/cdn/video.js";
      const VIDEO_JS_CSS_CDN =
        "https://cdn.jsdelivr.net/npm/@videojs/html/cdn/video.css";
      const VIDEO_JS_CSS_ID = "videojs-html-cdn-css";

      function ensureVideoJsStylesheet() {
        let link = document.getElementById(VIDEO_JS_CSS_ID);
        if (link) return link;

        link = document.createElement("link");
        link.id = VIDEO_JS_CSS_ID;
        link.rel = "stylesheet";
        link.href = VIDEO_JS_CSS_CDN;
        link.crossOrigin = "anonymous";
        document.head.appendChild(link);
        return link;
      }

      function loadVideoJsHtml() {
        if (!window.__videoJsHtmlLoadPromise) {
          window.__videoJsHtmlLoadPromise = import(
            /* @vite-ignore */ VIDEO_JS_HTML_CDN
          ).catch((err) => {
            delete window.__videoJsHtmlLoadPromise;
            throw err;
          });
        }

        return window.__videoJsHtmlLoadPromise;
      }

      export default {
        async mounted() {
          this.statusEl = this.el.querySelector('[data-role="status"]');
          this.videoEl = null;
          this.videoListeners = [];
          this.lastStateKey = null;
          this.setStatus("Loading Video.js v10...", [
            "badge-neutral",
            "bg-black/70",
            "text-white",
          ]);

          try {
            ensureVideoJsStylesheet();
            await loadVideoJsHtml();
            await customElements.whenDefined("video-player");
            this.videoEl = this.el.querySelector("video");

            if (!this.videoEl) {
              throw new Error("video element not found");
            }

            this.bindVideoEvents();

            this.el.classList.remove("opacity-0");
            this.setStatus("Elixir controls ready", [
              "badge-success",
              "bg-success/90",
              "text-success-content",
            ]);
            this.pushPlayerState({ force: true });
          } catch (err) {
            console.error("[VideoPlayer] failed to load Video.js:", err);
            this.el.classList.remove("opacity-0");
            this.setStatus("Load failed", [
              "badge-error",
              "bg-error/90",
              "text-error-content",
            ]);
          }

          this.handleEvent("video-command", ({ action }) => this.runCommand(action));
        },

        bindVideoEvents() {
          const sync = ({ force = false } = {}) => this.pushPlayerState({ force });

          this.videoListeners = [
            ["loadedmetadata", () => sync({ force: true })],
            ["durationchange", () => sync({ force: true })],
            ["play", () => sync({ force: true })],
            ["pause", () => sync({ force: true })],
            ["ended", () => sync({ force: true })],
            ["volumechange", () => sync({ force: true })],
            ["timeupdate", () => sync()],
          ];

          for (const [eventName, listener] of this.videoListeners) {
            this.videoEl.addEventListener(eventName, listener);
          }
        },

        async runCommand(action) {
          if (!this.videoEl) return;

          try {
            switch (action) {
              case "toggle-play":
                if (this.videoEl.paused || this.videoEl.ended) {
                  await this.videoEl.play();
                } else {
                  this.videoEl.pause();
                }
                break;

              case "toggle-mute":
                this.videoEl.muted = !this.videoEl.muted;
                break;

              case "seek-back":
                this.videoEl.currentTime = Math.max(0, this.videoEl.currentTime - 10);
                break;

              case "seek-forward": {
                const nextTime = this.videoEl.currentTime + 10;
                const maxTime = Number.isFinite(this.videoEl.duration)
                  ? this.videoEl.duration
                  : nextTime;
                this.videoEl.currentTime = Math.min(maxTime, nextTime);
                break;
              }

              case "restart":
                this.videoEl.currentTime = 0;
                break;

              default:
                return;
            }
          } catch (err) {
            if (action === "toggle-play") {
              const message =
                err?.name === "NotAllowedError"
                  ? "Browser blocked play() after the LiveView round-trip. Click the player once, then try again."
                  : `Play failed: ${String(err)}`;

              this.setStatus("Play blocked", [
                "badge-warning",
                "bg-warning/90",
                "text-warning-content",
              ]);
              this.pushEvent("video-play-blocked", { message }, () => {});
            } else {
              console.error("[VideoPlayer] command failed:", err);
            }
          }

          this.pushPlayerState({ force: true });
        },

        pushPlayerState({ force = false } = {}) {
          if (!this.videoEl) return;

          const snapshot = {
            ready: true,
            playing: !this.videoEl.paused && !this.videoEl.ended,
            muted: Boolean(this.videoEl.muted),
            current_time: Math.max(0, Math.floor(this.videoEl.currentTime || 0)),
            duration: Number.isFinite(this.videoEl.duration)
              ? Math.max(0, Math.floor(this.videoEl.duration))
              : 0,
          };
          const nextStateKey = JSON.stringify(snapshot);

          if (!force && nextStateKey === this.lastStateKey) return;

          this.lastStateKey = nextStateKey;
          this.pushEvent("video-state", snapshot, () => {});
        },

        setStatus(text, classes) {
          if (!this.statusEl) return;

          this.statusEl.textContent = text;
          this.statusEl.classList.remove(
            "badge-info",
            "badge-success",
            "badge-error",
            "badge-neutral",
            "badge-warning",
            "bg-black/70",
            "text-white",
            "bg-success",
            "bg-success/90",
            "text-success-content",
            "bg-error",
            "bg-error/90",
            "text-error-content",
            "bg-warning/90",
            "text-warning-content",
          );
          if (classes) this.statusEl.classList.add(...classes);
        },

        destroyed() {
          if (!this.videoEl) return;

          for (const [eventName, listener] of this.videoListeners) {
            this.videoEl.removeEventListener(eventName, listener);
          }

          this.videoListeners = [];
        },
      };
      </script>
    </section>
    """
  end

  defp player_state_label(false, _playing), do: "Loading"
  defp player_state_label(true, true), do: "Playing"
  defp player_state_label(true, false), do: "Paused"

  defp progress_percent(_current_time_s, duration_s) when duration_s <= 0, do: 0

  defp progress_percent(current_time_s, duration_s) do
    round(min(current_time_s * 100 / duration_s, 100))
  end

  defp format_clock(seconds) do
    seconds = max(seconds, 0)
    minutes = div(seconds, 60)
    seconds_remainder = rem(seconds, 60)
    IO.iodata_to_binary(:io_lib.format("~B:~2..0B", [minutes, seconds_remainder]))
  end

  defp truthy?(value) when is_boolean(value), do: value
  defp truthy?(value) when is_integer(value), do: value != 0
  defp truthy?(value) when is_binary(value), do: value in ["true", "1", "yes", "on"]
  defp truthy?(_value), do: false

  defp whole_seconds(value) when is_integer(value), do: max(value, 0)
  defp whole_seconds(value) when is_float(value), do: max(trunc(value), 0)

  defp whole_seconds(value) when is_binary(value) do
    case Float.parse(value) do
      {seconds, _rest} -> whole_seconds(seconds)
      :error -> 0
    end
  end

  defp whole_seconds(_value), do: 0
end
