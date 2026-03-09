defmodule WasmLiveView.NotesSqliteLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents
  alias WasmLiveView.Note

  @impl true
  def mount(_params, _session, socket) do
    notes = db_list()

    {:ok,
     socket
     |> assign(:current_route, :notes_sqlite)
     |> assign(:search, "")
     |> assign(:notes, notes)
     |> assign(:editing, nil)
     |> assign_form(Note.changeset(%Note{}, %{}))}
  end

  @impl true
  def handle_event("validate", %{"note" => params}, socket) do
    source = socket.assigns.editing || %Note{}
    changeset = Note.changeset(source, params) |> Map.put(:action, :validate)
    {:noreply, assign_form(socket, changeset)}
  end

  def handle_event("save", %{"note" => params}, socket) do
    case socket.assigns.editing do
      nil -> create_note(socket, params)
      note -> update_note(socket, note, params)
    end
  end

  def handle_event("edit", %{"id" => id}, socket) do
    note = Enum.find(socket.assigns.notes, &(&1.id == String.to_integer(id)))
    {:noreply, socket |> assign(:editing, note) |> assign_form(Note.changeset(note, %{}))}
  end

  def handle_event("cancel", _params, socket) do
    {:noreply, socket |> assign(:editing, nil) |> assign_form(Note.changeset(%Note{}, %{}))}
  end

  def handle_event("delete", %{"id" => id}, socket) do
    db_delete(String.to_integer(id))
    {:noreply, assign(socket, :notes, db_search(socket.assigns.search, db_list()))}
  end

  def handle_event("do-search", %{"value" => value}, socket) do
    value = String.trim(value)
    notes = db_search(value, socket.assigns.notes)
    {:noreply, socket |> assign(:search, value) |> assign(:notes, notes)}
  end

  defp create_note(socket, params) do
    ts = :erlang.system_time(:second)
    note = %Note{id: nil, inserted_at: ts, updated_at: ts}

    case Note.changeset(note, params) |> Ecto.Changeset.apply_action(:insert) do
      {:ok, new_note} ->
        db_insert(new_note)
        notes = db_search(socket.assigns.search, db_list())

        {:noreply,
         socket
         |> assign(:notes, notes)
         |> assign(:editing, nil)
         |> assign_form(Note.changeset(%Note{}, %{}))}

      {:error, changeset} ->
        {:noreply, assign_form(socket, changeset)}
    end
  end

  defp update_note(socket, note, params) do
    ts = :erlang.system_time(:second)

    case Note.changeset(%{note | updated_at: ts}, params)
         |> Ecto.Changeset.apply_action(:update) do
      {:ok, updated} ->
        db_update(updated)
        notes = db_search(socket.assigns.search, db_list())

        {:noreply,
         socket
         |> assign(:notes, notes)
         |> assign(:editing, nil)
         |> assign_form(Note.changeset(%Note{}, %{}))}

      {:error, changeset} ->
        {:noreply, assign_form(socket, changeset)}
    end
  end

  # --- SQLite via Popcorn.Wasm.run_js ---

  defp db_list do
    case Popcorn.Wasm.run_js(
           """
           ({ }) => {
             const db = window.__sqliteDB;
             if (!db) return [JSON.stringify([])];
             return [JSON.stringify(
               db.selectObjects(
                 "SELECT id, title, body, inserted_at, updated_at FROM notes ORDER BY inserted_at DESC"
               )
             )];
           }
           """,
           %{},
           return: :value
         ) do
      {:ok, json} when is_binary(json) ->
        json
        |> Jason.decode!()
        |> Enum.map(fn r ->
          %Note{
            id: r["id"],
            title: r["title"],
            body: r["body"],
            inserted_at: r["inserted_at"],
            updated_at: r["updated_at"]
          }
        end)

      _ ->
        []
    end
  end

  defp db_search("", _default), do: db_list()

  defp db_search(query, default) do
    case Popcorn.Wasm.run_js(
           """
           ({ args }) => {
             const db = window.__sqliteDB;
             const searchMode = window.__notesSearchMode || "like";
             const selectLikeRows = (query) => {
               const needle = `%${query}%`;
               return db.selectObjects(
                 `SELECT id, title, body, inserted_at, updated_at
                  FROM notes
                  WHERE lower(title) LIKE lower(?)
                     OR lower(coalesce(body, '')) LIKE lower(?)
                  ORDER BY inserted_at DESC`,
                 [needle, needle]
               );
             };
             let rows;

             if (searchMode === "fts5") {
               try {
                 rows = db.selectObjects(
                   `SELECT n.id, n.title, n.body, n.inserted_at, n.updated_at
                    FROM notes n
                    JOIN notes_fts ON notes_fts.rowid = n.id
                    WHERE notes_fts MATCH ?
                    ORDER BY bm25(notes_fts), n.inserted_at DESC`,
                   [args.query]
                 );
               } catch (_err) {
                 rows = selectLikeRows(args.query);
               }
             } else {
               rows = selectLikeRows(args.query);
             }

             return [JSON.stringify(rows)];
           }
           """,
           %{query: query},
           return: :value
         ) do
      {:ok, json} when is_binary(json) ->
        json
        |> Jason.decode!()
        |> Enum.map(fn r ->
          %Note{
            id: r["id"],
            title: r["title"],
            body: r["body"],
            inserted_at: r["inserted_at"],
            updated_at: r["updated_at"]
          }
        end)

      _ ->
        default
    end
  end

  defp db_insert(%Note{title: title, body: body, inserted_at: ts, updated_at: uts}) do
    Popcorn.Wasm.run_js(
      """
      ({ args }) => {
        window.__sqliteDB.exec({
          sql: "INSERT INTO notes (title, body, inserted_at, updated_at) VALUES (?, ?, ?, ?)",
          bind: [args.title, args.body, args.inserted_at, args.updated_at]
        });
        window.__sqliteSave();
        return [];
      }
      """,
      %{title: title, body: body || "", inserted_at: ts, updated_at: uts}
    )
  end

  defp db_update(%Note{id: id, title: title, body: body, updated_at: uts}) do
    Popcorn.Wasm.run_js(
      """
      ({ args }) => {
        window.__sqliteDB.exec({
          sql: "UPDATE notes SET title = ?, body = ?, updated_at = ? WHERE id = ?",
          bind: [args.title, args.body, args.updated_at, args.id]
        });
        window.__sqliteSave();
        return [];
      }
      """,
      %{id: id, title: title, body: body || "", updated_at: uts}
    )
  end

  defp db_delete(id) do
    Popcorn.Wasm.run_js(
      """
      ({ args }) => {
        window.__sqliteDB.exec({
          sql: "DELETE FROM notes WHERE id = ?",
          bind: [args.id]
        });
        window.__sqliteSave();
        return [];
      }
      """,
      %{id: id}
    )
  end

  defp assign_form(socket, changeset) do
    assign(socket, :form, to_form(changeset, as: :note))
  end

  @impl true
  def render(assigns) do
    ~H"""
    <.header>
      Notes (SQLite+Search)
      <:subtitle>
        SQLite via <code>@sqlite.org/sqlite-wasm</code>, persisted to OPFS and queried
        directly from the WASM process via <code>Popcorn.Wasm.run_js/3</code>.
      </:subtitle>
      <:actions>
        <span :if={@editing} class="text-sm text-base-content/50">Editing</span>
      </:actions>
    </.header>

    <.form for={@form} phx-change="validate" phx-submit="save" class="mb-8">
      <h2 class="text-lg font-semibold mb-4">
        <%= if @editing, do: "Edit Note", else: "New Note" %>
      </h2>
      <.input field={@form[:title]} label="Title" />
      <.input field={@form[:body]} type="textarea" label="Body" rows="4" />
      <div class="flex gap-2 mt-4">
        <.button type="submit" variant="primary">
          <%= if @editing, do: "Update", else: "Create" %>
        </.button>
        <.button :if={@editing} type="button" phx-click="cancel" class="btn">
          Cancel
        </.button>
      </div>
    </.form>

    <div class="mb-4">
      <label for="notes-search" class="text-sm font-medium block mb-1">Live search</label>
      <input
        id="notes-search"
        name="value"
        type="text"
        value={@search}
        placeholder="Try: title AND body, title OR body, quoted phrases..."
        phx-keyup="do-search"
        phx-debounce="250"
        class="input input-bordered w-full"
      />
    </div>

    <div :if={@notes == []} class="text-base-content/50 italic">
      <%= if @search == "", do: "No notes yet. Create one above!", else: "No notes match this search." %>
    </div>

    <div :for={note <- @notes} class="card card-border bg-base-100 mb-3">
      <div class="card-body p-4">
        <div class="flex justify-between items-start">
          <h3 class="card-title text-base">{note.title}</h3>
          <div class="flex gap-2">
            <.button phx-click="edit" phx-value-id={note.id} class="btn btn-sm btn-warning">
              Edit
            </.button>
            <.button
              phx-click="delete"
              phx-value-id={note.id}
              data-confirm="Are you sure?"
              class="btn btn-sm btn-error"
            >
              Delete
            </.button>
          </div>
        </div>
        <p :if={note.body && note.body != ""} class="whitespace-pre-wrap">{note.body}</p>
      </div>
    </div>
    """
  end

  # Colocated JS: SQLite + OPFS setup, extracted at compile time into the JS bundle.
  # This runs once at page load (side-effect import) and sets window.__sqliteDB,
  # window.__sqliteSave, and window.__notesSearchMode for run_js calls above.
  def sqlite_setup(assigns) do
    ~H"""
    <script :type={Phoenix.LiveView.ColocatedJS}>
    const BASE_URL = new URL("..", document.currentScript?.src || import.meta.url).href
      || new URL(".", window.location.href).href;
    const SQLITE_MODULE_URL = BASE_URL + "sqlite3.mjs";
    const DB_FILENAME = "/popcorn_notes.sqlite";

    async function opfsLoad() {
      try {
        const root = await navigator.storage.getDirectory();
        const fh = await root.getFileHandle("popcorn_notes.sqlite");
        const file = await fh.getFile();
        return new Uint8Array(await file.arrayBuffer());
      } catch {
        return null;
      }
    }

    async function opfsSave(db) {
      const data = window.__sqlite3.capi.sqlite3_js_db_export(db.pointer);
      const root = await navigator.storage.getDirectory();
      const fh = await root.getFileHandle("popcorn_notes.sqlite", { create: true });
      const writable = await fh.createWritable();
      await writable.write(data);
      await writable.close();
    }

    async function setupSQLite() {
      const { default: sqlite3InitModule } = await import(/* @vite-ignore */ SQLITE_MODULE_URL);
      const sqlite3 = await sqlite3InitModule();

      const existing = await opfsLoad();
      if (existing?.byteLength) {
        sqlite3.capi.sqlite3_js_posix_create_file(DB_FILENAME, existing);
      }

      const db = new sqlite3.oo1.DB(DB_FILENAME, existing?.byteLength ? "w" : "c");

      db.exec(`CREATE TABLE IF NOT EXISTS notes (
        id INTEGER PRIMARY KEY,
        title TEXT NOT NULL,
        body TEXT,
        inserted_at INTEGER NOT NULL,
        updated_at INTEGER NOT NULL
      )`);

      let notesSearchMode = "like";
      try {
        db.exec(`CREATE VIRTUAL TABLE IF NOT EXISTS notes_fts
          USING fts5(title, body, content='notes', content_rowid='id')`);

        db.exec(`CREATE TRIGGER IF NOT EXISTS notes_ai AFTER INSERT ON notes BEGIN
          INSERT INTO notes_fts(rowid, title, body) VALUES (new.id, new.title, coalesce(new.body, ''));
        END`);

        db.exec(`CREATE TRIGGER IF NOT EXISTS notes_ad AFTER DELETE ON notes BEGIN
          INSERT INTO notes_fts(notes_fts, rowid, title, body) VALUES ('delete', old.id, old.title, coalesce(old.body, ''));
        END`);

        db.exec(`CREATE TRIGGER IF NOT EXISTS notes_au AFTER UPDATE ON notes BEGIN
          INSERT INTO notes_fts(notes_fts, rowid, title, body) VALUES ('delete', old.id, old.title, coalesce(old.body, ''));
          INSERT INTO notes_fts(rowid, title, body) VALUES (new.id, new.title, coalesce(new.body, ''));
        END`);

        db.exec(`INSERT INTO notes_fts(notes_fts) VALUES ('rebuild')`);
        notesSearchMode = "fts5";
      } catch (err) {
        console.warn("[WasmLiveView] FTS5 unavailable, using LIKE search fallback", err);
      }

      window.__sqlite3 = sqlite3;
      window.__sqliteDB = db;
      window.__notesSearchMode = notesSearchMode;
      window.__sqliteSave = () => opfsSave(db).catch(console.error);
      console.log(
        `[WasmLiveView] SQLite ${sqlite3.version.libVersion} ready, persisted via OPFS (search: ${notesSearchMode})`
      );
    }

    window.__sqliteReady = setupSQLite();
    </script>
    """
  end
end
