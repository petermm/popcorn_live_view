defmodule WasmLiveView.BackgroundVideoLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents

  @player_src "https://stream.mux.com/BV3YZtogl89mg9VcNBhhnHm02Y34zI1nlMuMQfAbl3dM/highest.mp4"
  @video_actions ~w(toggle-play toggle-mute seek-back seek-forward restart)

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       current_route: :background_video,
       player_ready: false,
       player_playing: false,
       player_muted: true,
       current_time_s: 0,
       duration_s: 0,
       play_blocked_message: nil
     )}
  end

  @impl true
  def handle_event("video-command", %{"action" => action}, socket)
      when action in @video_actions do
    {:noreply,
     socket
     |> assign(:play_blocked_message, nil)
     |> push_event("background-video-command", %{action: action})}
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
       play_blocked_message:
         if(player_playing, do: nil, else: socket.assigns.play_blocked_message)
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
    status_label = if assigns.player_ready, do: "Background player ready", else: "Loading background player..."

    ~H"""
    <.header>
      Background Video
      <:subtitle>
        Background video hero with LiveView controls and overlay content.
      </:subtitle>
    </.header>

    <section class="space-y-4 rounded-2xl border border-base-300 bg-base-200/50 p-4 sm:p-6">
      <div class="space-y-1">
        <h2 class="text-base font-semibold">Background video hero</h2>
        <p class="text-sm text-base-content/70">
          This uses a native video background fallback, with the copy and status cards layered in front.
        </p>
      </div>

      <section class="relative isolate min-h-[28rem] overflow-hidden rounded-[2rem] border border-base-300 bg-slate-950 text-white shadow-2xl">
        <div
          id="background-video-shell"
          data-role="player-shell"
          phx-hook=".BackgroundVideoPlayer"
          phx-update="ignore"
          class="absolute inset-0 opacity-0 transition-opacity duration-500"
        >
          <video
            class="size-full object-cover"
            src={player_src}
            muted
            autoplay
            loop
            playsinline
            preload="auto"
            crossorigin="anonymous"
          >
          </video>
        </div>

        <div class="pointer-events-none absolute inset-0 z-10 bg-gradient-to-br from-slate-950/90 via-slate-900/40 to-slate-950/80"></div>
        <div class="pointer-events-none absolute inset-x-0 bottom-0 z-10 h-40 bg-gradient-to-t from-slate-950/80 to-transparent"></div>

        <div class="relative z-20 flex min-h-[28rem] flex-col justify-between p-6 sm:p-8 lg:p-10">
          <div class="flex items-start justify-between gap-4">
            <div class="max-w-2xl space-y-4">
              <div class="badge border-white/20 bg-white/10 text-white backdrop-blur">
                Background video demo
              </div>
              <div class="space-y-3">
                <h3 class="max-w-xl text-3xl font-black tracking-tight sm:text-4xl">
                  Full-bleed video with content layered directly over the player.
                </h3>
                <p class="max-w-xl text-sm leading-6 text-white/80 sm:text-base">
                  The media runs behind the copy while LiveView still owns the controls, state,
                  and progress readout.
                </p>
              </div>
              <div class="flex flex-wrap gap-3">
                <button
                  type="button"
                  phx-click="video-command"
                  phx-value-action="toggle-play"
                  disabled={!@player_ready}
                  class="btn btn-primary btn-sm pointer-events-auto"
                >
                  {play_label}
                </button>
                <button
                  type="button"
                  data-background-video-action="toggle-mute"
                  data-background-video-target="background-video-shell"
                  disabled={!@player_ready}
                  class="btn btn-sm border-white/20 bg-white/10 text-white backdrop-blur hover:bg-white/20 pointer-events-auto"
                >
                  {mute_label}
                </button>
              </div>
            </div>

            <div
              data-role="status"
              class="badge pointer-events-auto border-white/20 bg-black/50 text-white backdrop-blur"
            >
              {status_label}
            </div>
          </div>

          <div class="grid gap-3 sm:grid-cols-3">
            <div class="rounded-2xl border border-white/10 bg-white/10 p-4 backdrop-blur-sm">
              <div class="text-[0.7rem] uppercase tracking-[0.24em] text-white/60">State</div>
              <div class="mt-2 text-lg font-semibold text-white">{player_state}</div>
            </div>
            <div class="rounded-2xl border border-white/10 bg-white/10 p-4 backdrop-blur-sm">
              <div class="text-[0.7rem] uppercase tracking-[0.24em] text-white/60">Audio</div>
              <div class="mt-2 text-lg font-semibold text-white">
                {if @player_muted, do: "Muted", else: "Sound on"}
              </div>
            </div>
            <div class="rounded-2xl border border-white/10 bg-white/10 p-4 backdrop-blur-sm">
              <div class="text-[0.7rem] uppercase tracking-[0.24em] text-white/60">Position</div>
              <div class="mt-2 text-lg font-semibold text-white">
                {format_clock(@current_time_s)} / {format_clock(@duration_s)}
              </div>
            </div>
          </div>
        </div>
      </section>

      <section class="space-y-4 rounded-2xl border border-base-300 bg-base-100/80 p-4">
        <div class="flex flex-col gap-2 sm:flex-row sm:items-center sm:justify-between">
          <div>
            <h3 class="text-sm font-semibold">LiveView controls</h3>
            <p class="text-sm text-base-content/70">
              Same Elixir-side control loop as the regular video page, but driving the background layer.
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
            data-background-video-action="toggle-mute"
            data-background-video-target="background-video-shell"
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

        <p :if={@play_blocked_message} class="text-sm text-warning">
          {@play_blocked_message}
        </p>
      </section>

      <p class="text-xs text-base-content/60">
        Source: Mux demo stream. The requested
        <code>@videojs/html/cdn/background/*</code>
        assets returned 404 in the browser, so this page falls back to a native
        <code>&lt;video&gt;</code>
        background.
      </p>

      <script :type={Phoenix.LiveView.ColocatedHook} name=".BackgroundVideoPlayer">
      export default {
        async mounted() {
          this.statusEl = this.el.parentElement?.querySelector('[data-role="status"]');
          this.videoEl = null;
          this.videoListeners = [];
          this.lastStateKey = null;
          this.volumeRampFrame = null;
          this.savedVolume = 0.85;
          this.isSafari =
            navigator.vendor?.includes("Apple") &&
            !/CriOS|FxiOS|EdgiOS|Chrome|Chromium/.test(navigator.userAgent);
          this.setStatus("Loading background player...", [
            "bg-black/50",
            "text-white",
          ]);

          try {
            this.videoEl = this.el.querySelector("video");

            if (!this.videoEl) {
              throw new Error("video element not found");
            }

            this.savedVolume =
              typeof this.videoEl.volume === "number" && this.videoEl.volume > 0
                ? this.videoEl.volume
                : 0.85;
            this.videoEl.volume =
              this.videoEl.muted && !this.isSafari ? 0 : this.savedVolume;
            this.bindVideoEvents();
            this.handleDocumentClick = (event) => {
              const button = event.target.closest("[data-background-video-action]");
              if (!button || button.disabled) return;
              if (button.dataset.backgroundVideoTarget !== this.el.id) return;

              if (button.dataset.backgroundVideoAction === "toggle-mute") {
                event.preventDefault();
                void this.runCommand("toggle-mute");
              }
            };
            document.addEventListener("click", this.handleDocumentClick, true);

            this.el.classList.remove("opacity-0");
            this.setStatus("Background player ready", [
              "bg-emerald-500",
              "text-white",
            ]);
            this.videoEl.play?.().catch(() => {});
            this.pushPlayerState({ force: true });
          } catch (err) {
            console.error("[BackgroundVideoPlayer] failed to load background video:", err);
            this.el.classList.remove("opacity-0");
            this.setStatus("Load failed", [
              "bg-rose-500",
              "text-white",
            ]);
          }

          this.handleEvent("background-video-command", ({ action }) => this.runCommand(action));
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
                if (this.videoEl.muted) {
                  await this.fadeInAudio();
                } else {
                  await this.fadeOutAudio();
                }
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
                  ? "Browser blocked play() after the LiveView round-trip. Click the hero once, then try again."
                  : `Play failed: ${String(err)}`;

              this.setStatus("Play blocked", [
                "bg-amber-500",
                "text-white",
              ]);
              this.pushEvent("video-play-blocked", { message }, () => {});
            } else {
              console.error("[BackgroundVideoPlayer] command failed:", err);
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

        clearVolumeRamp() {
          if (this.volumeRampFrame != null) {
            cancelAnimationFrame(this.volumeRampFrame);
            this.volumeRampFrame = null;
          }
        },

        rampVolume(toVolume, duration = 360) {
          if (!this.videoEl) return Promise.resolve();

          this.clearVolumeRamp();

          const fromVolume = Number.isFinite(this.videoEl.volume) ? this.videoEl.volume : 0;
          const startAt = performance.now();

          return new Promise((resolve) => {
            const tick = (now) => {
              const progress = Math.min((now - startAt) / duration, 1);
              const eased = 1 - Math.pow(1 - progress, 3);
              this.videoEl.volume = fromVolume + (toVolume - fromVolume) * eased;

              if (progress < 1) {
                this.volumeRampFrame = requestAnimationFrame(tick);
                return;
              }

              this.videoEl.volume = toVolume;
              this.volumeRampFrame = null;
              resolve();
            };

            this.volumeRampFrame = requestAnimationFrame(tick);
          });
        },

        async fadeInAudio() {
          if (!this.videoEl) return;

          const targetVolume =
            Number.isFinite(this.savedVolume) && this.savedVolume > 0
              ? this.savedVolume
              : 0.85;

          if (this.isSafari) {
            this.clearVolumeRamp();
            this.setMutedState(false);
            this.videoEl.volume = targetVolume;
            this.videoEl.play?.().catch(() => {});
            return;
          }

          this.setMutedState(false);
          this.videoEl.volume = 0;
          this.videoEl.play?.().catch(() => {});
          await this.rampVolume(targetVolume, 420);
        },

        async fadeOutAudio() {
          if (!this.videoEl) return;

          if (this.isSafari) {
            this.clearVolumeRamp();
            this.setMutedState(true);
            return;
          }

          if (this.videoEl.volume > 0.05) {
            this.savedVolume = this.videoEl.volume;
          }

          await this.rampVolume(0, 280);
          this.setMutedState(true);
        },

        setMutedState(muted) {
          if (!this.videoEl) return;

          this.videoEl.muted = muted;
          this.videoEl.defaultMuted = muted;

          if (muted) {
            this.videoEl.setAttribute("muted", "");
          } else {
            this.videoEl.removeAttribute("muted");
          }
        },

        setStatus(text, classes) {
          if (!this.statusEl) return;

          this.statusEl.textContent = text;
          this.statusEl.classList.remove(
            "bg-black/50",
            "bg-emerald-500",
            "bg-rose-500",
            "bg-amber-500",
            "text-white",
          );
          if (classes) this.statusEl.classList.add(...classes);
        },

        destroyed() {
          this.clearVolumeRamp();

          if (this.handleDocumentClick) {
            document.removeEventListener("click", this.handleDocumentClick, true);
            this.handleDocumentClick = null;
          }

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
