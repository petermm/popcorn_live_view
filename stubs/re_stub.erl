-module(re).
-export([compile/1, compile/2, run/2, run/3, version/0, inspect/2]).

%% ===================================================================
%% :re stub for AtomVM/WASM
%%
%% Delegates regex operations to JavaScript's RegExp engine via
%% Popcorn.Wasm.run_js!. Provides the same API as Erlang's :re so
%% Elixir's Regex module works unmodified.
%%
%% Limitations:
%%   - PCRE-only syntax not supported by JS RegExp will fail
%%     (possessive quantifiers, atomic groups, etc.)
%%   - (?P<name>...) is auto-converted to (?<name>...)
%%   - Requires RegExp 'd' flag (Chrome 90+, Firefox 88+, Safari 15+)
%%   - Each run/3 call does a JS round-trip (JSON ser/deser overhead)
%% ===================================================================

%% -------------------------------------------------------------------
%% compile/1,2
%% -------------------------------------------------------------------
compile(Pattern) ->
    compile(Pattern, []).

compile(Pattern, Options) ->
    Bin = to_bin(Pattern),
    Flags = opts_to_flags(Options),
    JsPat = pcre_to_js(Bin),
    Args = #{pattern => JsPat, flags => Flags},
    case 'Elixir.Popcorn.Wasm':'run_js!'(compile_js(), Args, [{return, value}]) of
        true ->
            {ok, {re_stub, Bin, Flags}};
        ErrMsg when is_binary(ErrMsg) ->
            {error, {binary_to_list(ErrMsg), 0}}
    end.

%% -------------------------------------------------------------------
%% version/0
%% -------------------------------------------------------------------
version() ->
    <<"js_regexp_stub/1.0">>.

%% -------------------------------------------------------------------
%% inspect/2
%% -------------------------------------------------------------------
inspect({re_stub, Source, _Flags}, namelist) ->
    {namelist, extract_names(Source)}.

%% -------------------------------------------------------------------
%% run/2,3
%% -------------------------------------------------------------------
run(Subject, RE) ->
    run(Subject, RE, []).

run(Subject, RE, Options) ->
    {Pattern, Flags0} = case RE of
        {re_stub, P, F} -> {P, F};
        P               -> {to_bin(P), opts_to_flags(Options)}
    end,
    SubjBin = to_bin(Subject),
    Global  = lists:member(global, Options),
    {CapSpec, CapType} = parse_capture(Options),
    Offset  = proplists:get_value(offset, Options, 0),

    Flags = case Global of
        true  -> <<Flags0/binary, "g">>;
        false -> Flags0
    end,

    Args = #{
        pattern => pcre_to_js(Pattern),
        flags   => Flags,
        subject => SubjBin,
        offset  => Offset,
        global  => Global
    },

    Raw = 'Elixir.Popcorn.Wasm':'run_js!'(run_js(), Args, [{return, value}]),

    %% Build name→group-index map for named capture specs
    Names = extract_names(Pattern),
    NameMap = maps:from_list(
        [{N, Idx} || {N, Idx} <- lists:zip(Names, lists:seq(1, length(Names)))]
    ),

    case Raw of
        nil ->
            nomatch;
        Matches when is_list(Matches) ->
            case CapSpec of
                none -> match;
                _    -> {match, format(Matches, CapSpec, CapType, Global, NameMap)}
            end
    end.

%% ===================================================================
%% Internal helpers
%% ===================================================================

%% -------------------------------------------------------------------
%% Options → JS RegExp flags
%% -------------------------------------------------------------------
opts_to_flags(Opts) ->
    opts_to_flags(Opts, <<>>).

opts_to_flags([], Acc)              -> Acc;
opts_to_flags([caseless  | T], Acc) -> opts_to_flags(T, <<Acc/binary, "i">>);
opts_to_flags([multiline | T], Acc) -> opts_to_flags(T, <<Acc/binary, "m">>);
opts_to_flags([dotall    | T], Acc) -> opts_to_flags(T, <<Acc/binary, "s">>);
opts_to_flags([unicode   | T], Acc) -> opts_to_flags(T, <<Acc/binary, "u">>);
opts_to_flags([_         | T], Acc) -> opts_to_flags(T, Acc).

%% -------------------------------------------------------------------
%% Parse {capture, Spec, Type} from option list
%% -------------------------------------------------------------------
parse_capture(Opts) -> parse_capture(Opts, all, index).

parse_capture([], S, T)                        -> {S, T};
parse_capture([{capture, none}     | _], _, _) -> {none, index};
parse_capture([{capture, S, T}     | _], _, _) -> {S, T};
parse_capture([{capture, S}        | R], _, T) -> parse_capture(R, S, T);
parse_capture([_                   | R], S, T) -> parse_capture(R, S, T).

%% -------------------------------------------------------------------
%% PCRE → JS: convert (?P<name>...) → (?<name>...)
%% -------------------------------------------------------------------
pcre_to_js(<<>>)                 -> <<>>;
pcre_to_js(<<"(?P<", R/binary>>) -> <<"(?<", (pcre_to_js(R))/binary>>;
pcre_to_js(<<C, R/binary>>)     -> <<C, (pcre_to_js(R))/binary>>.

%% -------------------------------------------------------------------
%% Extract named capture group names in order from pattern source
%% -------------------------------------------------------------------
extract_names(Src) -> extract_names(Src, []).

extract_names(<<>>, Acc) ->
    lists:reverse(Acc);
extract_names(<<"(?P<", R/binary>>, Acc) ->
    {Name, R2} = take_until($>, R, <<>>),
    extract_names(R2, [Name | Acc]);
extract_names(<<"(?<", R/binary>>, Acc) ->
    case R of
        <<"=", _/binary>> -> extract_names(R, Acc);   %% lookbehind (?<=...)
        <<"!", _/binary>> -> extract_names(R, Acc);   %% neg lookbehind (?<!...)
        _ ->
            {Name, R2} = take_until($>, R, <<>>),
            extract_names(R2, [Name | Acc])
    end;
extract_names(<<_, R/binary>>, Acc) ->
    extract_names(R, Acc).

take_until(C, <<C, R/binary>>, Acc) -> {Acc, R};
take_until(C, <<X, R/binary>>, Acc) -> take_until(C, R, <<Acc/binary, X>>);
take_until(_, <<>>, Acc)            -> {Acc, <<>>}.

%% -------------------------------------------------------------------
%% JS: compile-time validation
%% -------------------------------------------------------------------
compile_js() ->
    <<"({ args }) => {\n"
      "  try { new RegExp(args.pattern, args.flags); return [true]; }\n"
      "  catch(e) { return [e.message]; }\n"
      "}">>.

%% -------------------------------------------------------------------
%% JS: regex execution — returns match data with byte-accurate offsets
%%
%% Returns null (no match) or an array of match objects:
%%   { indices: [[byteStart, byteLen], ...], texts: [string|null, ...] }
%%
%% Uses the 'd' flag (RegExp indices) for accurate group positions.
%% Falls back to indexOf-based estimation when 'd' is unavailable.
%% Byte positions are computed via TextEncoder (UTF-8).
%% -------------------------------------------------------------------
run_js() ->
    <<"({ args }) => {\n"
      "  const { pattern, flags, subject, offset, global: isGlobal } = args;\n"
      "  const enc = new TextEncoder();\n"
      "  const bytePos = (s, ci) => enc.encode(s.substring(0, ci)).length;\n"
      "  const charPos = (s, bo) => {\n"
      "    let b = 0, c = 0;\n"
      "    while (b < bo && c < s.length) { b += enc.encode(s.charAt(c)).length; c++; }\n"
      "    return c;\n"
      "  };\n"
      "  let useFlags = flags;\n"
      "  if (offset > 0 && !flags.includes('g')) useFlags += 'g';\n"
      "  let re;\n"
      "  try { re = new RegExp(pattern, useFlags + 'd'); }\n"
      "  catch(e) { re = new RegExp(pattern, useFlags); }\n"
      "  if (offset > 0) re.lastIndex = charPos(subject, offset);\n"
      "  const results = [];\n"
      "  let m;\n"
      "  do {\n"
      "    m = re.exec(subject);\n"
      "    if (m) {\n"
      "      const hi = !!m.indices;\n"
      "      const indices = [];\n"
      "      const texts = [];\n"
      "      for (let i = 0; i < m.length; i++) {\n"
      "        if (m[i] == null) {\n"
      "          indices.push([-1, 0]);\n"
      "          texts.push(null);\n"
      "        } else if (hi && m.indices[i]) {\n"
      "          const s = bytePos(subject, m.indices[i][0]);\n"
      "          const e = bytePos(subject, m.indices[i][1]);\n"
      "          indices.push([s, e - s]);\n"
      "          texts.push(m[i]);\n"
      "        } else {\n"
      "          const ci = subject.indexOf(m[i], m.index);\n"
      "          const s = bytePos(subject, ci >= 0 ? ci : m.index);\n"
      "          const l = enc.encode(m[i]).length;\n"
      "          indices.push([s, l]);\n"
      "          texts.push(m[i]);\n"
      "        }\n"
      "      }\n"
      "      results.push({ indices, texts });\n"
      "    }\n"
      "  } while (m && isGlobal);\n"
      "  return [results.length === 0 ? null : results];\n"
      "}">>.

%% -------------------------------------------------------------------
%% Format JS match results → Erlang :re.run return format
%% -------------------------------------------------------------------
format(Matches, CapSpec, CapType, true = _Global, NameMap) ->
    [format_one(M, CapSpec, CapType, NameMap) || M <- Matches];
format(Matches, CapSpec, CapType, false, NameMap) ->
    format_one(hd(Matches), CapSpec, CapType, NameMap).

format_one(Match, CapSpec, CapType, NameMap) ->
    Indices = maps:get(<<"indices">>, Match),
    Texts   = maps:get(<<"texts">>, Match),
    {SI, ST} = select_groups(CapSpec, Indices, Texts, NameMap),
    emit(CapType, SI, ST).

%% Select groups based on capture spec
select_groups(all, I, T, _NM)                       -> {I, T};
select_groups(all_but_first, [_ | I], [_ | T], _NM) -> {I, T};
select_groups(all_but_first, [], [], _NM)            -> {[], []};
select_groups(first, [I | _], [T | _], _NM)         -> {[I], [T]};
select_groups(first, [], [], _NM)                    -> {[], []};
select_groups(Ns, I, T, NameMap) when is_list(Ns) ->
    Len = length(I),
    Pick = fun
               (N) when is_integer(N), N < Len ->
                   {lists:nth(N + 1, I), lists:nth(N + 1, T)};
               (N) when is_binary(N) ->
                   case maps:find(N, NameMap) of
                       {ok, Idx} when Idx < Len ->
                           {lists:nth(Idx + 1, I), lists:nth(Idx + 1, T)};
                       _ ->
                           {[-1, 0], nil}
                   end;
               (_) ->
                   {[-1, 0], nil}
           end,
    Pairs = [Pick(N) || N <- Ns],
    {[element(1, P) || P <- Pairs],
     [element(2, P) || P <- Pairs]}.

%% Emit results in the requested type
emit(index, Indices, _Texts) ->
    [idx_tuple(I) || I <- Indices];
emit(binary, _Indices, Texts) ->
    [case T of nil -> <<>>; _ -> T end || T <- Texts];
emit(list, _Indices, Texts) ->
    [case T of nil -> []; _ -> binary_to_list(T) end || T <- Texts].

idx_tuple([S, L]) -> {S, L};
idx_tuple(T) when is_tuple(T) -> T.

%% -------------------------------------------------------------------
%% iolist → binary
%% -------------------------------------------------------------------
to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L)   -> iolist_to_binary(L).
