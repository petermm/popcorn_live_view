defmodule WasmLiveView.VideoLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents

  @player_src "https://stream.mux.com/BV3YZtogl89mg9VcNBhhnHm02Y34zI1nlMuMQfAbl3dM/highest.mp4"

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, current_route: :video)}
  end

  @impl true
  def render(assigns) do
    player_src = @player_src

    ~H"""
    <.header>
      Video
      <:subtitle>
        Video.js v10 HTML player loaded from jsDelivr with a colocated LiveView hook.
      </:subtitle>
    </.header>

    <section
      id="video-player-demo"
      phx-hook=".VideoPlayer"
      phx-update="ignore"
      class="space-y-4 rounded-2xl border border-base-300 bg-base-200/50 p-4 sm:p-6"
    >
      <div class="space-y-1">
        <h2 class="text-base font-semibold">Video.js v10 player</h2>
        <p class="text-sm text-base-content/70">
          The page loads the HTML renderer at mount time, then upgrades the custom elements in place.
        </p>
      </div>

      <div
        data-role="player-shell"
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
          this.playerShell = this.el.querySelector('[data-role="player-shell"]');
          this.setStatus("Loading Video.js v10...", [
            "badge-neutral",
            "bg-black/70",
            "text-white",
          ]);

          try {
            ensureVideoJsStylesheet();
            await loadVideoJsHtml();
            await customElements.whenDefined("video-player");

            this.playerShell?.classList.remove("opacity-0");
            this.setStatus("Player ready", [
              "badge-success",
              "bg-success/90",
              "text-success-content",
            ]);
          } catch (err) {
            console.error("[VideoPlayer] failed to load Video.js:", err);
            this.playerShell?.classList.remove("opacity-0");
            this.setStatus("Load failed", [
              "badge-error",
              "bg-error/90",
              "text-error-content",
            ]);
          }
        },

        setStatus(text, classes) {
          if (!this.statusEl) return;

          this.statusEl.textContent = text;
          this.statusEl.classList.remove(
            "badge-info",
            "badge-success",
            "badge-error",
            "badge-neutral",
            "bg-black/70",
            "text-white",
            "bg-success",
            "bg-success/90",
            "text-success-content",
            "bg-error",
            "bg-error/90",
            "text-error-content",
          );
          if (classes) this.statusEl.classList.add(...classes);
        },
      };
      </script>
    </section>
    """
  end
end
