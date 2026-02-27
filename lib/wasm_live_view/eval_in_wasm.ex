defmodule WasmLiveView.EvalInWasm do
  use GenServer

  @process_name :eval_server

  def start_link(_) do
    GenServer.start_link(__MODULE__, nil, name: @process_name)
  end

  def eval_elixir(code) do
    GenServer.call(@process_name, {:eval_elixir, code}, 15_000)
  end

  def compile_erlang(code) when is_binary(code) do
    GenServer.call(@process_name, {:compile_erlang, code}, 15_000)
  end

  def compile_erlang(code) when is_list(code) do
    compile_erlang(List.to_string(code))
  end

  def debug_pack do
    GenServer.call(@process_name, :debug_pack, 15_000)
  end

  def compile_and_pack(src) when is_binary(src) do
    GenServer.call(@process_name, {:compile_and_pack, src}, 30_000)
  end

  def run_api_test(name) when is_atom(name) do
    GenServer.call(@process_name, {:api_test, name}, 60_000)
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

  def handle_call(:debug_pack, _from, state) do
    result =
      try do
        src =
          "-module(hello_world).\n-export([start/0]).\nstart() -> io:format(\"Hello world!~n\").\n"

        [{filename, beam}] = do_compile_erlang(src)

        IO.inspect("made it so far")

        case :packbeam_api.create_from_binaries([{filename, beam}]) do
          {:ok, avm} -> {:ok, "OK, AVM size: #{byte_size(avm)} bytes"}
          {:error, r} -> {:error, inspect(r)}
        end
      catch
        kind, reason ->
          st = __STACKTRACE__
          {:error, "#{kind}: #{inspect(reason)}\n#{Exception.format_stacktrace(st)}"}
      end

    {:reply, result, state}
  end

  def handle_call({:compile_and_pack, src}, _from, state) do
    result =
      try do
        [{filename, beam}] = do_compile_erlang(src)

        case :packbeam_api.create_from_binaries([{filename, beam}]) do
          {:ok, avm} -> {:ok, "AVM #{byte_size(avm)}B"}
          {:error, r} -> {:error, inspect(r)}
        end
      catch
        kind, reason ->
          st = __STACKTRACE__
          {:error, "#{kind}: #{inspect(reason)}\n#{Exception.format_stacktrace(st)}"}
      end

    {:reply, result, state}
  end

  def handle_call({:api_test, name}, _from, state) do
    result =
      try do
        do_api_test(name)
      catch
        kind, reason ->
          st = __STACKTRACE__
          {:error, "#{kind}: #{inspect(reason)}\n#{Exception.format_stacktrace(st)}"}
      end

    {:reply, result, state}
  end

  def handle_call({:compile_erlang, code}, _from, state) do
    try do
      result = do_compile_erlang(code)
      {:reply, {:ok, result}, state}
    rescue
      e in RuntimeError ->
        {:reply, {:error, e.message}, state}

      e ->
        {:reply, {:error, inspect(e)}, state}
    catch
      kind, reason ->
        {:reply, {:error, "#{kind}: #{inspect(reason)}"}, state}
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

  # --- API tests ---

  defp do_api_test(:multi_module) do
    inputs =
      do_compile_erlang("-module(a).\n-export([start/0]).\nstart() -> b:start().\n") ++
        do_compile_erlang("-module(b).\n-export([start/0]).\nstart() -> ok.\n") ++
        do_compile_erlang("-module(c).\n-export([test/0]).\ntest() -> ok.\n")

    case :packbeam_api.create_from_binaries(inputs) do
      {:ok, avm} ->
        n = count_avm_elements(avm)
        if n == 3, do: {:ok, "3 modules, #{byte_size(avm)}B"}, else: {:error, "expected 3, got #{n}"}

      {:error, r} ->
        {:error, inspect(r)}
    end
  end

  defp do_api_test(:io_format_regression) do
    src = "-module(hello_world).\n-export([start/0]).\nstart() -> io:format(\"Hello world!~n\").\n"
    inputs = do_compile_erlang(src)

    case :packbeam_api.create_from_binaries(inputs) do
      {:ok, avm} ->
        n = count_avm_elements(avm)
        if n == 1, do: {:ok, "1 module, #{byte_size(avm)}B"}, else: {:error, "expected 1, got #{n}"}

      {:error, r} ->
        {:error, inspect(r)}
    end
  end

  defp do_api_test(:source_text) do
    greet =
      "-module(greet).\n-export([start/0,hello/1]).\nstart() -> hello(world).\nhello(N) -> {hello,N}.\n"

    util = "-module(util).\n-export([id/1]).\nid(X) -> X.\n"
    inputs = do_compile_erlang(greet) ++ do_compile_erlang(util)

    case :packbeam_api.create_from_binaries(inputs) do
      {:ok, avm} ->
        n = count_avm_elements(avm)

        if n == 2,
          do: {:ok, "2 modules (greet+util), #{byte_size(avm)}B"},
          else: {:error, "expected 2, got #{n}"}

      {:error, r} ->
        {:error, inspect(r)}
    end
  end

  defp do_api_test(:lib_option) do
    inputs =
      do_compile_erlang("-module(a).\n-export([start/0]).\nstart() -> b:start().\n") ++
        do_compile_erlang("-module(b).\n-export([start/0]).\nstart() -> ok.\n")

    case :packbeam_api.create_from_binaries(inputs, %{lib: true}) do
      {:ok, avm} ->
        n = count_avm_elements(avm)
        {:ok, "#{n} modules as lib (no entrypoint), #{byte_size(avm)}B"}

      {:error, r} ->
        {:error, inspect(r)}
    end
  end

  defp do_api_test(:prune_drops_d) do
    # a→b, b→c+e, c→f; d is unreachable — prune should drop it
    srcs = [
      "-module(a).\n-export([start/0]).\nstart() -> b:start().\n",
      "-module(b).\n-export([start/0]).\nstart() -> e:b_calls_me(), c:test().\n",
      "-module(c).\n-export([test/0]).\ntest() -> f:c_calls_me().\n",
      "-module(d).\n-export([no_one_calls_me/0]).\nno_one_calls_me() -> sorry.\n",
      "-module(e).\n-export([b_calls_me/0]).\nb_calls_me() -> ok.\n",
      "-module(f).\n-export([c_calls_me/0]).\nc_calls_me() -> ok.\n"
    ]

    inputs = Enum.flat_map(srcs, &do_compile_erlang/1)

    case :packbeam_api.create_from_binaries(inputs, %{prune: true}) do
      {:ok, avm} ->
        names = avm_module_names(avm)

        if Enum.member?(names, "d.beam"),
          do: {:error, "d.beam not pruned — modules: #{inspect(names)}"},
          else: {:ok, "d dropped, #{length(names)} kept: #{Enum.join(names, ", ")}, #{byte_size(avm)}B"}

      {:error, r} ->
        {:error, inspect(r)}
    end
  end

  defp do_api_test(:prune_sup_callback) do
    # start_mod calls my_sup via atom; my_sup references my_worker only in a literal child spec map
    # prune must scan literal tables to keep my_worker; d is unreachable and should be dropped
    start_src =
      "-module(start_mod).\n-export([start/0]).\nstart() -> Sup = my_sup, Sup:start_link().\n"

    sup_src =
      "-module(my_sup).\n-export([start_link/0,init/1]).\n" <>
        "start_link() -> init([]).\n" <>
        "init(_) ->\n" <>
        "    ChildSpecs = [\#{id => my_worker, start => {my_worker, start_link, []}}],\n" <>
        "    {ok, {\#{}, ChildSpecs}}.\n"

    worker_src =
      "-module(my_worker).\n-export([start_link/0]).\nstart_link() -> ok.\n"

    d_src = "-module(d).\n-export([no_one_calls_me/0]).\nno_one_calls_me() -> sorry.\n"

    inputs =
      Enum.flat_map([start_src, sup_src, worker_src, d_src], &do_compile_erlang/1)

    case :packbeam_api.create_from_binaries(inputs, %{prune: true}) do
      {:ok, avm} ->
        names = avm_module_names(avm)
        has_worker = Enum.member?(names, "my_worker.beam")
        has_d = Enum.member?(names, "d.beam")

        cond do
          !has_worker ->
            {:error, "my_worker pruned (wrong) — modules: #{inspect(names)}"}

          has_d ->
            {:error, "d not pruned (wrong) — modules: #{inspect(names)}"}

          true ->
            {:ok, "my_worker kept, d pruned — #{length(names)} modules, #{byte_size(avm)}B"}
        end

      {:error, r} ->
        {:error, inspect(r)}
    end
  end

  # --- AVM binary helpers ---
  # Format: 24-byte header, then elements <<Size:32, Flags:32, Reserved:32, Name\0, padding, data>>
  # Size includes itself (4 bytes), so payload = Size-4 bytes follow the Size field.
  # Terminated by <<0:32, ...>>.

  defp count_avm_elements(<<_::binary-size(24), rest::binary>>), do: do_count_elements(rest, 0)
  defp count_avm_elements(_), do: 0

  defp do_count_elements(<<0::32, _::binary>>, n), do: n
  defp do_count_elements(<<>>, n), do: n

  defp do_count_elements(<<size::32, rest::binary>>, n) when size >= 4 do
    skip = size - 4
    <<_::binary-size(skip), next::binary>> = rest
    do_count_elements(next, n + 1)
  end

  defp avm_module_names(<<_::binary-size(24), rest::binary>>), do: do_avm_names(rest, [])
  defp avm_module_names(_), do: []

  defp do_avm_names(<<0::32, _::binary>>, acc), do: Enum.reverse(acc)
  defp do_avm_names(<<>>, acc), do: Enum.reverse(acc)

  defp do_avm_names(<<size::32, rest::binary>>, acc) when size >= 4 do
    skip = size - 4
    <<element::binary-size(skip), next::binary>> = rest
    # element: <<Flags:32, Reserved:32, Name_bytes...\0, padding, data>>
    name = avm_element_name(element)
    do_avm_names(next, [name | acc])
  end

  defp avm_element_name(<<_::64, rest::binary>>), do: read_until_null(rest, [])
  defp avm_element_name(_), do: ""

  defp read_until_null(<<0, _::binary>>, acc),
    do: :erlang.list_to_binary(:lists.reverse(acc))

  defp read_until_null(<<c, rest::binary>>, acc), do: read_until_null(rest, [c | acc])
  defp read_until_null(<<>>, acc), do: :erlang.list_to_binary(:lists.reverse(acc))

  # --- Erlang compiler ---

  defp do_compile_erlang(code) do
    code_list = :unicode.characters_to_list(code, :utf8) ++ ~c"\n"
    forms = scan_and_parse_forms(code_list, {1, 1})
    {:ok, module, module_bin} = :compile.forms(forms, [])
    [{Atom.to_charlist(module) ++ ~c".beam", module_bin}]
  end

  defp scan_and_parse_forms(remaining, loc) do
    case :erl_scan.tokens([], remaining, loc) do
      {:done, {:ok, tokens, end_loc}, rest} ->
        {:ok, form} = :erl_parse.parse_form(tokens)
        [form | scan_and_parse_forms(rest, end_loc)]

      {:done, {:eof, _}, _} ->
        []

      {:more, _} ->
        []
    end
  end

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
