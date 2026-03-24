const CODEMIRROR_BASE_CDN = "https://cdn.jsdelivr.net/npm/codemirror@5.65.20";
const CODEMIRROR_JS_CDN = `${CODEMIRROR_BASE_CDN}/lib/codemirror.js`;
const CODEMIRROR_MODE_CDN = `${CODEMIRROR_BASE_CDN}/mode/erlang/erlang.js`;
const CODEMIRROR_CSS_CDN = `${CODEMIRROR_BASE_CDN}/lib/codemirror.css`;

let codeMirrorPromise = null;

function loadScript(url) {
  const existing = document.querySelector(`script[data-codemirror-url="${url}"]`);

  if (existing) {
    if (existing.dataset.loaded === "true") return Promise.resolve();

    return new Promise((resolve, reject) => {
      existing.addEventListener("load", () => resolve(), { once: true });
      existing.addEventListener("error", () => reject(new Error(`Failed to load ${url}`)), {
        once: true,
      });
    });
  }

  return new Promise((resolve, reject) => {
    const script = document.createElement("script");
    script.src = url;
    script.async = true;
    script.dataset.codemirrorUrl = url;

    script.addEventListener(
      "load",
      () => {
        script.dataset.loaded = "true";
        resolve();
      },
      { once: true },
    );

    script.addEventListener("error", () => reject(new Error(`Failed to load ${url}`)), {
      once: true,
    });

    document.head.appendChild(script);
  });
}

function loadStylesheet(url) {
  const existing = document.querySelector(`link[data-codemirror-url="${url}"]`);

  if (existing) {
    if (existing.dataset.loaded === "true") return Promise.resolve();

    return new Promise((resolve, reject) => {
      existing.addEventListener("load", () => resolve(), { once: true });
      existing.addEventListener("error", () => reject(new Error(`Failed to load ${url}`)), {
        once: true,
      });
    });
  }

  return new Promise((resolve, reject) => {
    const link = document.createElement("link");
    link.rel = "stylesheet";
    link.href = url;
    link.dataset.codemirrorUrl = url;

    link.addEventListener(
      "load",
      () => {
        link.dataset.loaded = "true";
        resolve();
      },
      { once: true },
    );

    link.addEventListener("error", () => reject(new Error(`Failed to load ${url}`)), {
      once: true,
    });

    document.head.appendChild(link);
  });
}

async function loadCodeMirror() {
  if (!codeMirrorPromise) {
    codeMirrorPromise = (async () => {
      await loadStylesheet(CODEMIRROR_CSS_CDN);
      await loadScript(CODEMIRROR_JS_CDN);
      await loadScript(CODEMIRROR_MODE_CDN);

      if (!window.CodeMirror) {
        throw new Error("CodeMirror did not initialize");
      }

      return window.CodeMirror;
    })().catch((error) => {
      codeMirrorPromise = null;
      throw error;
    });
  }

  return await codeMirrorPromise;
}

export const ErlangCodeEditor = {
  async mounted() {
    this.textarea = this.el.querySelector('textarea[name="code"]');
    this.editorRoot = this.el.querySelector('[data-role="editor-root"]');
    this.editor = null;
    this._applyingRemoteUpdate = false;

    if (!this.textarea || !this.editorRoot) return;

    try {
      const CodeMirror = await loadCodeMirror();

      this.editor = CodeMirror(this.editorRoot, {
        value: this._readTextareaValue(),
        mode: "erlang",
        theme: "atomvm-dark",
        lineNumbers: true,
        lineWrapping: true,
        indentUnit: 4,
        tabSize: 4,
      });

      this.editor.on("change", () => {
        if (this._applyingRemoteUpdate) return;

        this._syncTextarea(this.editor.getValue());
      });

      this._syncVisibility();
      this.editor.refresh();
    } catch (err) {
      console.error("[ErlangCodeEditor] failed to load CodeMirror:", err);
    }
  },

  updated() {
    this._syncVisibility();

    if (!this.editor || !this.textarea) return;

    const nextValue = this._readTextareaValue();
    const currentValue = this.editor.getValue();

    if (nextValue !== currentValue) {
      this._applyingRemoteUpdate = true;
      this.editor.setValue(nextValue);
      this._applyingRemoteUpdate = false;
    }

    this.editor.refresh();
  },

  destroyed() {
    if (this.editor) {
      this.editor.getWrapperElement().remove();
      this.editor = null;
    }

    if (this.textarea) {
      this.textarea.classList.remove("erlang-editor-hidden");
    }

    if (this.editorRoot) {
      this.editorRoot.classList.add("hidden");
      this.editorRoot.replaceChildren();
    }
  },

  _readTextareaValue() {
    const value = this.textarea.value || "";
    const textContent = this.textarea.textContent || "";

    if (textContent && textContent !== value) {
      return textContent;
    }

    return value || textContent;
  },

  _syncTextarea(value) {
    this.textarea.value = value;
    this.textarea.textContent = value;
    this.textarea.dispatchEvent(new Event("input", { bubbles: true }));
  },

  _syncVisibility() {
    if (this.textarea) {
      this.textarea.classList.add("erlang-editor-hidden");
    }

    if (this.editorRoot) {
      this.editorRoot.classList.remove("hidden");
    }
  },
};
