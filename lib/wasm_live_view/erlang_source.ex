defmodule WasmLiveView.ErlangSource do
  def compile_binary!(code) do
    forms = preprocessed_forms!(code)

    case :compile.forms(forms, compile_opts()) do
      {:ok, module, module_bin} ->
        {module, module_bin}

      {:ok, module, module_bin, _warnings} ->
        {module, module_bin}

      {:error, errors, _warnings} ->
        raise format_compiler_messages(errors)
    end
  end

  def load_module!(code) do
    forms = preprocessed_forms!(code)

    case :compile.noenv_forms(forms, compile_opts()) do
      {:ok, module, module_bin} ->
        {:module, _module} = :code.load_binary(module, ~c"nofile", module_bin)
        module

      {:ok, module, module_bin, _warnings} ->
        {:module, _module} = :code.load_binary(module, ~c"nofile", module_bin)
        module

      {:error, errors, _warnings} ->
        raise format_compiler_messages(errors)
    end
  end

  def format!(code) do
    forms = scan_forms!(code)

    if Enum.any?(forms, &form_uses_macros?/1) do
      format_with_macros(code)
    else
      forms
      |> Enum.map_join("", fn tokens ->
        tokens
        |> parse_form!()
        |> :erl_pp.form()
        |> IO.iodata_to_binary()
      end)
      |> String.trim_trailing()
      |> Kernel.<>("\n")
    end
  end

  def preprocessed_forms!(code) do
    code
    |> preprocess_forms!()
    |> Enum.map(&parse_form!/1)
  end

  def format_compiler_messages(errors) do
    errors
    |> Enum.flat_map(fn
      {_file, file_errors} -> file_errors
      error_info -> [error_info]
    end)
    |> Enum.map_join("\n", &format_error_info/1)
  end

  defp compile_opts do
    [
      :binary,
      :deterministic,
      :return_errors,
      :return_warnings,
      :compressed,
      :no_spawn_compiler_process,
      :no_docs
    ]
  end

  defp format_with_macros(code) do
    case try_format_with_syntax_tools(code) do
      {:ok, formatted} ->
        formatted

      {:error, _reason} ->
        forms = preprocess_forms!(code)
        Enum.each(forms, &parse_form!/1)
        String.trim_trailing(code) <> "\n"
    end
  end

  defp try_format_with_syntax_tools(code) do
    try do
      formatted =
        code
        |> parse_syntax_forms!()
        |> apply_erlang_unary(:erl_syntax, :form_list)
        |> apply_erlang_unary(:erl_prettypr, :format)
        |> IO.iodata_to_binary()
        |> String.trim_trailing()
        |> Kernel.<>("\n")

      {:ok, formatted}
    rescue
      error ->
        {:error, error}
    catch
      kind, reason ->
        {:error, {kind, reason}}
    end
  end

  defp parse_syntax_forms!(code) do
    with_string_io(code, fn string_io ->
      case apply_erlang(module: :epp_dodger, function: :parse, args: [string_io, 1, [clever: true]]) do
        {:ok, forms} -> forms
        {:error, error_info} -> raise format_error_info(error_info)
      end
    end)
  end

  defp parse_form!(tokens) do
    case :erl_parse.parse_form(tokens) do
      {:ok, form} -> form
      {:error, error_info} -> raise format_error_info(error_info)
    end
  end

  defp preprocess_forms!(code) do
    code
    |> scan_forms!()
    |> expand_preprocessor_forms!()
  end

  defp scan_forms!(code) do
    case scan_forms(code) do
      {:ok, forms} -> forms
      {:error, error_info} -> raise format_error_info(error_info)
    end
  end

  defp scan_forms(code) do
    code
    |> :unicode.characters_to_list(:utf8)
    |> Kernel.++(~c"\n")
    |> do_scan_forms({1, 1}, [])
  end

  defp do_scan_forms(remaining, loc, acc) do
    case :erl_scan.tokens([], remaining, loc) do
      {:done, {:ok, tokens, end_loc}, rest} ->
        do_scan_forms(rest, end_loc, [tokens | acc])

      {:done, {:eof, _}, _rest} ->
        {:ok, Enum.reverse(acc)}

      {:done, {:error, error_info, _end_loc}, _rest} ->
        {:error, error_info}

      {:more, _continuation} ->
        {:ok, Enum.reverse(acc)}
    end
  end

  defp expand_preprocessor_forms!(forms) do
    {expanded, _state} =
      Enum.reduce(forms, {[], %{macros: %{}, module_name: nil}}, fn form, {acc, state} ->
        case classify_preprocessor_form(form) do
          {:define, name, replacement} ->
            {acc, %{state | macros: Map.put(state.macros, name, replacement)}}

          {:undef, name} ->
            {acc, %{state | macros: Map.delete(state.macros, name)}}

          {:unsupported, directive} ->
            raise "unsupported preprocessor directive -#{directive}"

          :regular ->
            expanded_form = expand_macro_tokens!(form, state)
            next_state = maybe_track_module_name(expanded_form, state)
            {[expanded_form | acc], next_state}
        end
      end)

    Enum.reverse(expanded)
  end

  defp classify_preprocessor_form([
         {:-, _},
         {:atom, _, :define},
         {:"(", _} | rest
       ]) do
    with {:ok, inside, [{:dot, _}]} <- take_until_matching_closer(rest, [:")"], []),
         {:ok, head, replacement} <- split_on_top_level_comma(inside),
         {:ok, name} <- parse_macro_name(head) do
      {:define, name, replacement}
    else
      {:unsupported, reason} -> raise reason
      _ -> raise "invalid -define directive"
    end
  end

  defp classify_preprocessor_form([
         {:-, _},
         {:atom, _, :undef},
         {:"(", _} | rest
       ]) do
    with {:ok, inside, [{:dot, _}]} <- take_until_matching_closer(rest, [:")"], []),
         {:ok, name} <- parse_undef_name(inside) do
      {:undef, name}
    else
      _ -> raise "invalid -undef directive"
    end
  end

  defp classify_preprocessor_form([{:-, _}, {:atom, _, directive} | _rest])
       when directive in [:include, :include_lib, :ifdef, :ifndef, :if, :elif, :else, :endif, :error, :warning, :feature] do
    {:unsupported, directive}
  end

  defp classify_preprocessor_form(_form), do: :regular

  defp parse_macro_name([{:var, _, name}]), do: {:ok, name}
  defp parse_macro_name([{:atom, _, name}]), do: {:ok, name}

  defp parse_macro_name([{:var, _, _name}, {:"(", _} | _rest]),
    do: {:unsupported, "macro definitions with arguments"}

  defp parse_macro_name([{:atom, _, _name}, {:"(", _} | _rest]),
    do: {:unsupported, "macro definitions with arguments"}

  defp parse_macro_name(_head), do: :error

  defp parse_undef_name([{:var, _, name}]), do: {:ok, name}
  defp parse_undef_name([{:atom, _, name}]), do: {:ok, name}
  defp parse_undef_name(_inside), do: :error

  defp maybe_track_module_name(
         [
           {:-, _},
           {:atom, _, :module},
           {:"(", _},
           {:atom, _, module_name},
           {:")", _},
           {:dot, _}
         ],
         state
       ) do
    %{state | module_name: module_name}
  end

  defp maybe_track_module_name(_form, state), do: state

  defp form_uses_macros?(form) do
    Enum.any?(form, fn
      {:"?", _} -> true
      {:-, _} ->
        case classify_preprocessor_form(form) do
          {:define, _, _} -> true
          {:undef, _} -> true
          {:unsupported, _} -> true
          :regular -> false
        end

      _ ->
        false
    end)
  end

  defp expand_macro_tokens!(tokens, state) do
    do_expand_macro_tokens(tokens, state)
  end

  defp do_expand_macro_tokens([{:"?", line}, {:var, _, _name}, {:"(", _} | _rest], _state) do
    raise "line #{line}: macro calls with arguments are not supported in AtomVM"
  end

  defp do_expand_macro_tokens([{:"?", line}, {:atom, _, _name}, {:"(", _} | _rest], _state) do
    raise "line #{line}: macro calls with arguments are not supported in AtomVM"
  end

  defp do_expand_macro_tokens([{:"?", line}, {:var, _, name} | rest], state) do
    expand_macro_use(name, line, state) ++ do_expand_macro_tokens(rest, state)
  end

  defp do_expand_macro_tokens([{:"?", line}, {:atom, _, name} | rest], state) do
    expand_macro_use(name, line, state) ++ do_expand_macro_tokens(rest, state)
  end

  defp do_expand_macro_tokens([token | rest], state) do
    [token | do_expand_macro_tokens(rest, state)]
  end

  defp do_expand_macro_tokens([], _state), do: []

  defp expand_macro_use(name, line, %{macros: macros, module_name: module_name}) do
    case Map.fetch(macros, name) do
      {:ok, replacement} ->
        do_expand_macro_tokens(replacement, %{macros: macros, module_name: module_name})

      :error ->
        case builtin_macro_tokens(name, line, module_name) do
          nil -> raise "line #{line}: undefined macro '#{name}'"
          tokens -> tokens
        end
    end
  end

  defp builtin_macro_tokens(:MODULE, line, module_name) when is_atom(module_name),
    do: [{:atom, line, module_name}]

  defp builtin_macro_tokens(:MODULE_STRING, line, module_name) when is_atom(module_name),
    do: [{:string, line, Atom.to_charlist(module_name)}]

  defp builtin_macro_tokens(:FILE, line, _module_name), do: [{:string, line, ~c"nofile"}]
  defp builtin_macro_tokens(:LINE, line, _module_name), do: [{:integer, line, line}]

  defp builtin_macro_tokens(:MACHINE, line, _module_name) do
    machine = :erlang.system_info(:machine) |> List.to_string() |> String.to_atom()
    [{:atom, line, machine}]
  end

  defp builtin_macro_tokens(:OTP_RELEASE, line, _module_name) do
    otp_release = :erlang.system_info(:otp_release) |> List.to_string() |> String.to_integer()
    [{:integer, line, otp_release}]
  end

  defp builtin_macro_tokens(_name, _line, _module_name), do: nil

  defp split_on_top_level_comma(tokens) do
    do_split_on_top_level_comma(tokens, [], [])
  end

  defp do_split_on_top_level_comma([{:",", _} | rest], [], acc) do
    {:ok, Enum.reverse(acc), rest}
  end

  defp do_split_on_top_level_comma([token | rest], stack, acc) do
    do_split_on_top_level_comma(rest, update_nesting_stack(token, stack), [token | acc])
  end

  defp do_split_on_top_level_comma([], _stack, _acc), do: :error

  defp take_until_matching_closer([token | rest], [expected], acc) do
    if token_type(token) == expected do
      {:ok, Enum.reverse(acc), rest}
    else
      take_until_matching_closer(rest, update_nesting_stack(token, [expected]), [token | acc])
    end
  end

  defp take_until_matching_closer([token | rest], stack, acc) do
    take_until_matching_closer(rest, update_nesting_stack(token, stack), [token | acc])
  end

  defp take_until_matching_closer([], _stack, _acc), do: :error

  defp update_nesting_stack(token, stack) do
    case opening_closer(token) do
      nil ->
        pop_matching_closer(token, stack)

      closer ->
        [closer | stack]
    end
  end

  defp opening_closer({:"(", _}), do: :")"
  defp opening_closer({:"{", _}), do: :"}"
  defp opening_closer({:"[", _}), do: :"]"
  defp opening_closer({:"<<", _}), do: :">>"
  defp opening_closer({:begin, _}), do: :end
  defp opening_closer({:case, _}), do: :end
  defp opening_closer({:if, _}), do: :end
  defp opening_closer({:receive, _}), do: :end
  defp opening_closer({:try, _}), do: :end
  defp opening_closer(_token), do: nil

  defp pop_matching_closer(token, [expected | rest]) do
    if token_type(token) == expected, do: rest, else: [expected | rest]
  end

  defp pop_matching_closer(_token, stack), do: stack

  defp token_type({type, _, _}), do: type
  defp token_type({type, _}), do: type

  defp format_error_info({line, module, description}) when is_integer(line) do
    message =
      if function_exported?(module, :format_error, 1) do
        description |> module.format_error() |> IO.iodata_to_binary()
      else
        inspect(description)
      end

    "line #{line}: #{message}"
  end

  defp format_error_info({{line, _column}, module, description}) do
    format_error_info({line, module, description})
  end

  defp format_error_info(other), do: inspect(other)

  defp with_string_io(code, fun) do
    {:ok, string_io} = StringIO.open(code)

    try do
      fun.(string_io)
    after
      StringIO.close(string_io)
    end
  end

  defp apply_erlang(module: module, function: function, args: args) do
    :erlang.apply(module, function, args)
  end

  defp apply_erlang_unary(value, module, function) do
    :erlang.apply(module, function, [value])
  end
end
