defmodule WasmLiveView.NotesLive do
  use Phoenix.LiveView, layout: {WasmLiveView.Layouts, :app}

  import WasmLiveViewWeb.CoreComponents

  alias WasmLiveView.{Note, Notes}

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:current_route, :home)
     |> assign(:notes, Notes.list_notes())
     |> assign(:editing, nil)
     |> assign_form(Notes.change_note(%Note{}))}
  end

  @impl true
  def handle_event("validate", %{"note" => _params} = all_params, socket) do
    source = socket.assigns.editing || %Note{}
    changeset = Notes.change_note(source, all_params["note"]) |> Map.put(:action, :validate)
    {:noreply, assign_form(socket, changeset)}
  end

  def handle_event("save", %{"note" => params}, socket) do
    case socket.assigns.editing do
      nil -> create_note(socket, params)
      note -> update_note(socket, note, params)
    end
  end

  def handle_event("edit", %{"id" => id}, socket) do
    note = Notes.get_note!(String.to_integer(id))
    changeset = Notes.change_note(note)
    {:noreply, socket |> assign(:editing, note) |> assign_form(changeset)}
  end

  def handle_event("cancel", _params, socket) do
    {:noreply,
     socket
     |> assign(:editing, nil)
     |> assign_form(Notes.change_note(%Note{}))}
  end

  def handle_event("delete", %{"id" => id}, socket) do
    note = Notes.get_note!(String.to_integer(id))
    Notes.delete_note(note)
    {:noreply, assign(socket, :notes, Notes.list_notes())}
  end

  defp create_note(socket, params) do
    case Notes.create_note(params) do
      {:ok, _note} ->
        {:noreply,
         socket
         |> assign(:notes, Notes.list_notes())
         |> assign(:editing, nil)
         |> assign_form(Notes.change_note(%Note{}))}

      {:error, changeset} ->
        {:noreply, assign_form(socket, changeset)}
    end
  end

  defp update_note(socket, note, params) do
    case Notes.update_note(note, params) do
      {:ok, _note} ->
        {:noreply,
         socket
         |> assign(:notes, Notes.list_notes())
         |> assign(:editing, nil)
         |> assign_form(Notes.change_note(%Note{}))}

      {:error, changeset} ->
        {:noreply, assign_form(socket, changeset)}
    end
  end

  defp assign_form(socket, changeset) do
    assign(socket, :form, to_form(changeset, as: :note))
  end

  @impl true
  def render(assigns) do
    ~H"""
    <.header>
      Notes
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

    <div
      :for={note <- @notes}
      class="card card-border bg-base-100 mb-3"
    >
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
