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

    <div id="chrome-ai-demo" phx-hook="ChromeAIDemo" class="space-y-8">
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
    """
  end
end
