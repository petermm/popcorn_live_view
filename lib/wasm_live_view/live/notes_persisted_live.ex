defmodule WasmLiveView.NotesPersistedLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents
  alias WasmLiveView.Note

  @storage_key "popcorn_notes"

  @impl true
  def mount(_params, _session, socket) do
    notes = load_from_storage()

    {:ok,
     socket
     |> assign(:current_route, :notes_persisted)
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
    id = parse_id(id)
    note = Enum.find(socket.assigns.notes, &(&1.id == id))
    {:noreply, socket |> assign(:editing, note) |> assign_form(Note.changeset(note, %{}))}
  end

  def handle_event("cancel", _params, socket) do
    {:noreply, socket |> assign(:editing, nil) |> assign_form(Note.changeset(%Note{}, %{}))}
  end

  def handle_event("delete", %{"id" => id}, socket) do
    id = parse_id(id)
    notes = Enum.reject(socket.assigns.notes, &(&1.id == id))
    save_to_storage(notes)
    {:noreply, assign(socket, :notes, notes)}
  end

  defp create_note(socket, params) do
    id = :erlang.unique_integer([:positive, :monotonic])
    ts = :erlang.system_time(:second)
    note = %Note{id: id, inserted_at: ts, updated_at: ts}

    case Note.changeset(note, params) |> Ecto.Changeset.apply_action(:insert) do
      {:ok, new_note} ->
        notes = [new_note | socket.assigns.notes]
        save_to_storage(notes)

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

    case Note.changeset(%{note | updated_at: ts}, params) |> Ecto.Changeset.apply_action(:update) do
      {:ok, updated} ->
        notes =
          Enum.map(socket.assigns.notes, fn n -> if n.id == updated.id, do: updated, else: n end)

        save_to_storage(notes)

        {:noreply,
         socket
         |> assign(:notes, notes)
         |> assign(:editing, nil)
         |> assign_form(Note.changeset(%Note{}, %{}))}

      {:error, changeset} ->
        {:noreply, assign_form(socket, changeset)}
    end
  end

  # WASM directly reads from localStorage — no JS hook needed
  defp load_from_storage do
    case Popcorn.Wasm.run_js(
           "({ args }) => { return [localStorage.getItem(args.key)]; }",
           %{key: @storage_key},
           return: :value
         ) do
      {:ok, nil} ->
        []

      {:ok, json} ->
        json
        |> Jason.decode!()
        |> Enum.map(fn n ->
          %Note{
            id: n["id"],
            title: n["title"],
            body: n["body"],
            inserted_at: n["inserted_at"],
            updated_at: n["updated_at"]
          }
        end)
        |> Enum.sort_by(& &1.inserted_at, :desc)

      _ ->
        []
    end
  end

  # WASM directly writes to localStorage — no JS hook needed
  defp save_to_storage(notes) do
    json = Jason.encode!(Enum.map(notes, &Map.from_struct/1))

    Popcorn.Wasm.run_js(
      "({ args }) => { localStorage.setItem(args.key, args.json); return []; }",
      %{key: @storage_key, json: json}
    )
  end

  # phx-value-id is always a string; note ids are integers
  defp parse_id(id) when is_binary(id), do: String.to_integer(id)
  defp parse_id(id), do: id

  defp assign_form(socket, changeset) do
    assign(socket, :form, to_form(changeset, as: :note))
  end

  @impl true
  def render(assigns) do
    ~H"""
    <.header>
      Notes (Persisted)
      <:subtitle>
        Stored in <code>localStorage</code> — survives page reloads.
        The WASM process calls <code>localStorage</code> directly via <code>Popcorn.Wasm.run_js/3</code>.
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
