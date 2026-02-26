defmodule WasmLiveView.EvalInWasm do
  use GenServer

  @process_name :eval_server

  def start_link(_) do
    GenServer.start_link(__MODULE__, nil, name: @process_name)
  end

  def eval_elixir(code) do
    GenServer.call(@process_name, {:eval_elixir, code}, 15_000)
  end

  @impl GenServer
  def init(_args) do
    {:ok, nil}
  end

  @impl GenServer
  def handle_call({:eval_elixir, code}, _from, state) do
    try do
      {value, output} = do_eval(code, :elixir)
      result = format_result(output, value)
      {:reply, {:ok, result}, state}
    rescue
      error -> {:reply, {:error, Exception.message(error)}, state}
    end
  end

  defp do_eval(code, :elixir) do
    {:ok, string_io} = StringIO.open("")
    original_gl = :erlang.group_leader()
    :erlang.group_leader(string_io, self())

    try do
      {value, _bindings} = Code.eval_string(code, [], __ENV__)
      {_input, output} = StringIO.contents(string_io)
      {value, output}
    after
      :erlang.group_leader(original_gl, self())
      StringIO.close(string_io)
    end
  end

  defp format_result("", value), do: inspect(value)
  defp format_result(output, value), do: output <> "=> " <> inspect(value)

  defp do_eval(code, {:module, :erlang}) do
    compile_opts = [
      :deterministic,
      :return_errors,
      :compressed,
      :no_spawn_compiler_process,
      :no_docs
    ]

    parse_form = fn form_tok ->
      {:ok, form} = :erl_parse.parse_form(form_tok)
      form
    end

    code = :erlang.binary_to_list(code)

    with {:ok, tokens, _end_location} <- :erl_scan.string(code),
         {:ok, module, module_bin} <-
           tokens
           |> split_forms()
           |> Enum.map(parse_form)
           |> :compile.noenv_forms(compile_opts),
         {:module, _module} <- :code.load_binary(module, ~c"nofile", module_bin) do
      module
    end
  end

  defp do_eval(code, :erlang) do
    code = :erlang.binary_to_list(code)

    with {:ok, tokens, _end_location} <- :erl_scan.string(code),
         {:ok, exprs} <- :erl_parse.parse_exprs(tokens),
         {:value, value, _bindings} <- :erl_eval.exprs(exprs, []) do
      value
    end
  end

  defp split_forms(forms) do
    split_on_dots = fn
      {:dot, _} = f, current -> {:cont, Enum.reverse([f | current]), []}
      f, current -> {:cont, [f | current]}
    end

    ensure_empty_acc = fn [] -> {:cont, []} end

    Enum.chunk_while(forms, [], split_on_dots, ensure_empty_acc)
  end
end
