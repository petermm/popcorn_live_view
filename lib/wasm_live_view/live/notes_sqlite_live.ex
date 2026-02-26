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
    {:noreply, assign(socket, :notes, db_list())}
  end

  defp create_note(socket, params) do
    ts = :erlang.system_time(:second)
    note = %Note{id: nil, inserted_at: ts, updated_at: ts}

    case Note.changeset(note, params) |> Ecto.Changeset.apply_action(:insert) do
      {:ok, new_note} ->
        db_insert(new_note)
        {:noreply,
         socket
         |> assign(:notes, db_list())
         |> assign(:editing, nil)
         |> assign_form(Note.changeset(%Note{}, %{}))}

      {:error, changeset} ->
        {:noreply, assign_form(socket, changeset)}
    end
  end

  defp update_note(socket, note, params) do
    ts = :erlang.system_time(:second)

    case Note.changeset(%{note | updated_at: ts}, params) |> Ecto.Changeset.apply_action(:update) do
      {:ok, updated} ->
        db_update(updated)
        {:noreply,
         socket
         |> assign(:notes, db_list())
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
             const stmt = db.prepare("SELECT id, title, body, inserted_at, updated_at FROM notes ORDER BY inserted_at DESC");
             const rows = [];
             while (stmt.step()) {
               const r = stmt.getAsObject();
               rows.push(r);
             }
             stmt.free();
             return [JSON.stringify(rows)];
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

  defp db_insert(%Note{title: title, body: body, inserted_at: ts, updated_at: uts}) do
    Popcorn.Wasm.run_js(
      """
      ({ args }) => {
        window.__sqliteDB.run(
          "INSERT INTO notes (title, body, inserted_at, updated_at) VALUES (?, ?, ?, ?)",
          [args.title, args.body, args.inserted_at, args.updated_at]
        );
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
        window.__sqliteDB.run(
          "UPDATE notes SET title = ?, body = ?, updated_at = ? WHERE id = ?",
          [args.title, args.body, args.updated_at, args.id]
        );
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
        window.__sqliteDB.run('DELETE FROM notes WHERE id = ?', [args.id]);
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
      Notes (SQLite)
      <:subtitle>
        SQLite via <code>sql.js</code>, persisted to OPFS — survives page reloads.
        The WASM process issues SQL directly via <code>Popcorn.Wasm.run_js/3</code>.
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

    <div :if={@notes == []} class="text-base-content/50 italic">
      No notes yet. Create one above!
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
end
