defmodule WasmLiveView.ChromeAILive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, current_route: :chrome_ai)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <.header>
      chromeAI
      <:subtitle>
        Basic demos for Chrome's experimental on-device AI APIs (prompt + writer style calls).
      </:subtitle>
    </.header>

    <div id="chrome-ai-demo" phx-hook=".ChromeAIDemo" class="space-y-8">
      <section class="rounded-xl border border-base-300 bg-base-200/40 p-4 space-y-3">
        <h2 class="text-base font-semibold">API status</h2>
        <p class="text-sm text-base-content/70">
          This checks Chrome's documented globals: <code>LanguageModel</code>,
          <code>Writer</code>, and <code>Rewriter</code>.
        </p>

        <div data-role="status" class="text-sm text-base-content/80">
          Checking capabilities...
        </div>

        <button type="button" data-role="refresh" class="btn btn-sm btn-soft">
          Refresh capability check
        </button>
      </section>

      <section class="rounded-xl border border-base-300 bg-base-200/40 p-4 space-y-3">
        <h2 class="text-base font-semibold">Prompt demo</h2>
        <p class="text-sm text-base-content/70">
          Sends your prompt to the Prompt API via <code>LanguageModel.create()</code>.
        </p>

        <label class="fieldset">
          <span class="label">Prompt</span>
          <textarea
            data-role="prompt-input"
            class="textarea textarea-bordered w-full"
            rows="4"
          >Give me 3 ideas for an Elixir LiveView demo that uses browser AI APIs.</textarea>
        </label>

        <div class="flex items-center gap-2">
          <button type="button" data-role="prompt-run" class="btn btn-primary btn-sm">
            Run prompt
          </button>
          <span data-role="prompt-busy" class="text-sm text-base-content/70 hidden">Running...</span>
        </div>

        <pre
          data-role="prompt-output"
          class="min-h-24 whitespace-pre-wrap rounded-lg bg-base-100 p-3 text-sm"
        >No output yet.</pre>
      </section>

      <section class="rounded-xl border border-base-300 bg-base-200/40 p-4 space-y-3">
        <h2 class="text-base font-semibold">Writer demo</h2>
        <p class="text-sm text-base-content/70">
          Tries Chrome's experimental writer/rewriter style APIs. Exact methods can differ by build.
        </p>

        <label class="fieldset">
          <span class="label">Instruction</span>
          <input
            data-role="writer-instruction"
            class="input input-bordered w-full"
            type="text"
            value="Rewrite this in a concise, friendly tone."
          />
        </label>

        <label class="fieldset">
          <span class="label">Text</span>
          <textarea
            data-role="writer-input"
            class="textarea textarea-bordered w-full"
            rows="5"
          >This project demonstrates multiple LiveView pages that run inside a browser-based runtime.</textarea>
        </label>

        <div class="flex items-center gap-2">
          <button type="button" data-role="writer-run" class="btn btn-primary btn-sm">
            Run writer
          </button>
          <span data-role="writer-busy" class="text-sm text-base-content/70 hidden">Running...</span>
        </div>

        <pre
          data-role="writer-output"
          class="min-h-24 whitespace-pre-wrap rounded-lg bg-base-100 p-3 text-sm"
        >No output yet.</pre>
      </section>
    </div>

    <script :type={Phoenix.LiveView.ColocatedHook} name=".ChromeAIDemo">
    export default {
      mounted() {
        this.statusEl = this.el.querySelector('[data-role="status"]');
        this.refreshBtn = this.el.querySelector('[data-role="refresh"]');
        this.promptInputEl = this.el.querySelector('[data-role="prompt-input"]');
        this.promptRunBtn = this.el.querySelector('[data-role="prompt-run"]');
        this.promptBusyEl = this.el.querySelector('[data-role="prompt-busy"]');
        this.promptOutputEl = this.el.querySelector('[data-role="prompt-output"]');
        this.writerInstructionEl = this.el.querySelector(
          '[data-role="writer-instruction"]',
        );
        this.writerInputEl = this.el.querySelector('[data-role="writer-input"]');
        this.writerRunBtn = this.el.querySelector('[data-role="writer-run"]');
        this.writerBusyEl = this.el.querySelector('[data-role="writer-busy"]');
        this.writerOutputEl = this.el.querySelector('[data-role="writer-output"]');

        this.onRefresh = () => {
          this.renderStatus();
        };
        this.onPromptRun = () => this.runPromptDemo();
        this.onWriterRun = () => this.runWriterDemo();

        this.refreshBtn?.addEventListener("click", this.onRefresh);
        this.promptRunBtn?.addEventListener("click", this.onPromptRun);
        this.writerRunBtn?.addEventListener("click", this.onWriterRun);

        this.renderStatus();
      },

      destroyed() {
        this.refreshBtn?.removeEventListener("click", this.onRefresh);
        this.promptRunBtn?.removeEventListener("click", this.onPromptRun);
        this.writerRunBtn?.removeEventListener("click", this.onWriterRun);
      },

      getPromptAPI() {
        return window.LanguageModel || null;
      },

      getWriterAPI() {
        return window.Writer || null;
      },

      getRewriterAPI() {
        return window.Rewriter || null;
      },

      async capabilitySummary() {
        const promptAPI = this.getPromptAPI();
        const writerAPI = this.getWriterAPI();
        const rewriterAPI = this.getRewriterAPI();
        const promptAvailability = promptAPI?.availability
          ? await promptAPI.availability().catch(() => "error")
          : "n/a";
        const writerAvailability = writerAPI?.availability
          ? await writerAPI.availability().catch(() => "error")
          : "n/a";
        const rewriterAvailability = rewriterAPI?.availability
          ? await rewriterAPI.availability().catch(() => "error")
          : "n/a";

        return [
          `LanguageModel: ${promptAPI ? "available" : "not found"}`,
          `LanguageModel.availability(): ${promptAvailability}`,
          `Writer: ${writerAPI ? "available" : "not found"}`,
          `Writer.availability(): ${writerAvailability}`,
          `Rewriter: ${rewriterAPI ? "available" : "not found"}`,
          `Rewriter.availability(): ${rewriterAvailability}`,
        ];
      },

      async renderStatus() {
        if (!this.statusEl) return;
        this.statusEl.textContent = "Checking capabilities...";
        const lines = await this.capabilitySummary();
        this.statusEl.textContent = lines.join("\n");
      },

      setBusy(kind, busy) {
        const runBtn = kind === "prompt" ? this.promptRunBtn : this.writerRunBtn;
        const busyEl = kind === "prompt" ? this.promptBusyEl : this.writerBusyEl;
        if (runBtn) runBtn.disabled = busy;
        if (busyEl) busyEl.classList.toggle("hidden", !busy);
      },

      async runPromptRaw(promptText) {
        const promptAPI = this.getPromptAPI();
        if (promptAPI?.create) {
          // Prompt API creation can require transient user activation.
          // Do not await other async work before create().
          let session;
          try {
            session = await promptAPI.create({
              monitor: (m) => {
                if (!m?.addEventListener) return;
                m.addEventListener("downloadprogress", (e) => {
                  const pct =
                    typeof e.total === "number" && e.total > 0
                      ? Math.round((e.loaded / e.total) * 100)
                      : Math.round((e.loaded || 0) * 100);
                  if (this.promptBusyEl) {
                    this.promptBusyEl.textContent = `Downloading model: ${pct}%`;
                  }
                });
              },
            });
          } catch (_err) {
            session = await promptAPI.create();
          }
          try {
            if (typeof session.prompt === "function") {
              return await session.prompt(promptText);
            }
            if (typeof session.promptStreaming === "function") {
              const stream = session.promptStreaming(promptText);
              let acc = "";
              for await (const chunk of stream) acc += chunk;
              return acc;
            }
            throw new Error("Session has no prompt() or promptStreaming()");
          } finally {
            await session.destroy?.();
            await session.close?.();
          }
        }

        throw new Error("No Prompt API found (LanguageModel.create)");
      },

      async runWriterRaw(instruction, text) {
        const writerAPI = this.getWriterAPI();
        if (writerAPI?.create) {
          if (writerAPI.availability) {
            const availability = await writerAPI.availability();
            if (availability === "unavailable") {
              throw new Error("Writer is unavailable on this browser/device");
            }
          }

          const writer = await writerAPI.create();
          try {
            if (typeof writer.write === "function") {
              try {
                return await writer.write(text, { context: instruction });
              } catch {
                return await writer.write(`${instruction}\n\n${text}`);
              }
            }
            if (typeof writer.rewrite === "function") {
              return await writer.rewrite(text, { instruction });
            }
            if (typeof writer.prompt === "function") {
              return await writer.prompt(`${instruction}\n\n${text}`);
            }
          } finally {
            await writer.destroy?.();
            await writer.close?.();
          }
        }

        const rewriterAPI = this.getRewriterAPI();
        if (rewriterAPI?.create) {
          if (rewriterAPI.availability) {
            const availability = await rewriterAPI.availability();
            if (availability === "unavailable") {
              throw new Error("Rewriter is unavailable on this browser/device");
            }
          }

          const rewriter = await rewriterAPI.create();
          try {
            if (typeof rewriter.rewrite === "function") {
              return await rewriter.rewrite(text, { instruction });
            }
          } finally {
            await rewriter.destroy?.();
            await rewriter.close?.();
          }
        }

        throw new Error("No writer/rewriter API found");
      },

      async runPromptDemo() {
        const promptText = this.promptInputEl?.value?.trim();
        if (!promptText) return;

        this.setBusy("prompt", true);
        if (this.promptOutputEl) this.promptOutputEl.textContent = "Running...";

        try {
          const output = await this.runPromptRaw(promptText);
          if (this.promptOutputEl) this.promptOutputEl.textContent = String(output);
        } catch (err) {
          if (this.promptOutputEl) {
            this.promptOutputEl.textContent = `Prompt demo failed:\n${
              err?.name ? `${err.name}: ` : ""
            }${err?.message || String(err)}`;
          }
        } finally {
          this.setBusy("prompt", false);
          if (this.promptBusyEl) this.promptBusyEl.textContent = "Running...";
          this.renderStatus();
        }
      },

      async runWriterDemo() {
        const instruction = this.writerInstructionEl?.value?.trim() || "";
        const text = this.writerInputEl?.value?.trim();
        if (!text) return;

        this.setBusy("writer", true);
        if (this.writerOutputEl) this.writerOutputEl.textContent = "Running...";

        try {
          const output = await this.runWriterRaw(instruction, text);
          if (this.writerOutputEl) this.writerOutputEl.textContent = String(output);
        } catch (err) {
          if (this.writerOutputEl) {
            this.writerOutputEl.textContent = `Writer demo failed:\n${String(err)}`;
          }
        } finally {
          this.setBusy("writer", false);
          this.renderStatus();
        }
      },
    };
    </script>
    """
  end
end
