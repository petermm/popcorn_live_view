defmodule WasmLiveView.Note do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :integer, autogenerate: false}
  embedded_schema do
    field :title, :string
    field :body, :string
    field :inserted_at, :integer
    field :updated_at, :integer
  end

  def changeset(note, attrs) do
    note
    |> cast(attrs, [:title, :body])
    |> validate_required([:title])
  end
end

defmodule WasmLiveView.Notes do
  use Agent

  alias WasmLiveView.Note

  defp now, do: :erlang.system_time(:second)

  def start_link(_opts) do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  def list_notes do
    Agent.get(__MODULE__, fn notes ->
      notes
      |> Map.values()
      |> Enum.sort_by(& &1.inserted_at, :desc)
    end)
  end

  def get_note!(id) do
    Agent.get(__MODULE__, fn notes -> Map.fetch!(notes, id) end)
  end

  def change_note(%Note{} = note, attrs \\ %{}) do
    Note.changeset(note, attrs)
  end

  def create_note(attrs) do
    id = :erlang.unique_integer([:positive])
    ts = now()

    %Note{id: id, inserted_at: ts, updated_at: ts}
    |> Note.changeset(attrs)
    |> Ecto.Changeset.apply_action(:insert)
    |> case do
      {:ok, note} ->
        Agent.update(__MODULE__, &Map.put(&1, note.id, note))
        {:ok, note}

      {:error, changeset} ->
        {:error, changeset}
    end
  end

  def update_note(%Note{} = note, attrs) do
    note
    |> Note.changeset(attrs)
    |> Ecto.Changeset.put_change(:updated_at, now())
    |> Ecto.Changeset.apply_action(:update)
    |> case do
      {:ok, updated} ->
        Agent.update(__MODULE__, &Map.put(&1, updated.id, updated))
        {:ok, updated}

      {:error, changeset} ->
        {:error, changeset}
    end
  end

  def delete_note(%Note{id: id}) do
    Agent.update(__MODULE__, &Map.delete(&1, id))
    :ok
  end
end
