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
    JsPat = pcre_to_js(Bin, Flags),
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

    Raw = case split_keep_operator(Pattern) of
        {ok, LeftPat, RightPat} ->
            Args = #{
                left    => pcre_to_js(LeftPat, Flags),
                right   => pcre_to_js(RightPat, Flags),
                pattern => pcre_to_js(Pattern, Flags),
                flags   => Flags,
                subject => SubjBin,
                offset  => Offset,
                global  => Global
            },
            'Elixir.Popcorn.Wasm':'run_js!'(run_keep_js(), Args, [{return, value}]);
        none ->
            Args = #{
                pattern => pcre_to_js(Pattern, Flags),
                flags   => Flags,
                subject => SubjBin,
                offset  => Offset,
                global  => Global
            },
            'Elixir.Popcorn.Wasm':'run_js!'(run_js(), Args, [{return, value}])
    end,

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
%% PCRE → JS compatibility rewrites
%% -------------------------------------------------------------------
pcre_to_js(Bin, Flags) ->
    %% Compatibility rewrites for JS RegExp:
    %%   (?P<name>...) -> (?<name>...)
    %%   (?P=name)     -> \k<name>     (named backref)
    %%   (?1)          -> \1           (simple numeric subroutine fallback)
    %%   (?>...)       -> (?:...)      (atomic group fallback)
    %%   \A, \z        -> ^, $
    %%   \Z            -> (?=(?:\r?\n)?$)
    %%   \R            -> (?:\r\n|[\n\r\u2028\u2029])
    %%   \h,\H,\v,\V   -> Unicode-ish horizontal/vertical whitespace classes
    %%   (?|...)       -> (?:...)      (branch-reset fallback, partial semantics)
    %%   \K            -> (?<=left)right (single-\K fallback, fixed-prefix friendly)
    %%   (?C...), (*SKIP)(*FAIL)       compile fallbacks (partial semantics)
    %%   [[:lower:]]   -> [\p{Ll}]     with /u, [a-z] otherwise
    %%   \w, \W        -> Unicode-aware equivalents with /u
    B0 = rewrite_pcre_named_backrefs(Bin),
    B0a = rewrite_simple_subroutines(B0),
    B1 = replace_all(B0a, <<"(?P<">>, <<"(?<">>),
    B2 = replace_all(B1, <<"(?>">>, <<"(?:">>),
    B2a = replace_all(B2, <<"\\A">>, <<"^">>),
    B2b = replace_all(B2a, <<"\\z">>, <<"$">>),
    B2b1 = replace_all(B2b, <<"\\Z">>, <<"(?=(?:\\r?\\n)?$)">>),
    B2b2 = replace_all(B2b1, <<"\\R">>, <<"(?:\\r\\n|[\\n\\r\\u2028\\u2029])">>),
    B2b3 = replace_all(B2b2, <<"\\h">>, <<"[\\t\\x20\\u00A0\\u1680\\u2000-\\u200A\\u202F\\u205F\\u3000]">>),
    B2b4 = replace_all(B2b3, <<"\\H">>, <<"[^\\t\\x20\\u00A0\\u1680\\u2000-\\u200A\\u202F\\u205F\\u3000]">>),
    B2b5 = replace_all(B2b4, <<"\\v">>, <<"[\\n\\v\\f\\r\\x85\\u2028\\u2029]">>),
    B2b6 = replace_all(B2b5, <<"\\V">>, <<"[^\\n\\v\\f\\r\\x85\\u2028\\u2029]">>),
    B2c = replace_all(B2b6, <<"(?|">>, <<"(?:" >>),
    B2d = rewrite_keep_operator(B2c),
    B2d1 = replace_all(B2d, <<"\\K">>, <<>>),
    B2e = strip_callouts(B2d1),
    B2f = replace_all(B2e, <<"(*SKIP)(*FAIL)">>, <<"(?!)">>),
    B2g = replace_all(B2f, <<"(*FAIL)">>, <<"(?!)">>),
    B3 =
        case has_flag(Flags, $u) of
            true  -> replace_all(B2g, <<"[[:lower:]]">>, <<"[\\p{Ll}]">>);
            false -> replace_all(B2g, <<"[[:lower:]]">>, <<"[a-z]">>)
        end,
    case has_flag(Flags, $u) of
        true ->
            B4 = replace_all(B3, <<"\\w">>, <<"[\\p{L}\\p{N}_]">>),
            replace_all(B4, <<"\\W">>, <<"[^\\p{L}\\p{N}_]">>);
        false ->
            B3
    end.

has_flag(<<>>, _Flag) -> false;
has_flag(<<Flag, _/binary>>, Flag) -> true;
has_flag(<<_, Rest/binary>>, Flag) -> has_flag(Rest, Flag).

%% Convert PCRE named backrefs: (?P=name) -> \k<name>
rewrite_pcre_named_backrefs(Bin) ->
    rewrite_pcre_named_backrefs(Bin, <<>>).

rewrite_pcre_named_backrefs(<<>>, Acc) ->
    Acc;
rewrite_pcre_named_backrefs(<<"(?P=", Rest/binary>>, Acc) ->
    case take_until($), Rest, <<>>) of
        {<<>>, _} ->
            <<Acc/binary, "(?P=", Rest/binary>>;
        {Name, Tail} ->
            rewrite_pcre_named_backrefs(Tail, <<Acc/binary, "\\k<", Name/binary, ">">>)
    end;
rewrite_pcre_named_backrefs(<<C, Rest/binary>>, Acc) ->
    rewrite_pcre_named_backrefs(Rest, <<Acc/binary, C>>).

%% Convert simple numeric subroutines: (?1) .. (?9) -> \1 .. \9
rewrite_simple_subroutines(Bin) ->
    rewrite_simple_subroutines(Bin, 1).

rewrite_simple_subroutines(Bin, 10) ->
    Bin;
rewrite_simple_subroutines(Bin, N) ->
    D = integer_to_binary(N),
    Pat = <<"(?", D/binary, ")">>,
    Repl = <<"\\", D/binary>>,
    rewrite_simple_subroutines(replace_all(Bin, Pat, Repl), N + 1).

%% Single \K fallback: left\Kright -> (?<=left)right
%% This preserves common "drop-prefix from overall match" behavior for fixed-width left parts.
rewrite_keep_operator(Bin) ->
    case binary:split(Bin, <<"\\K">>) of
        [Left, Right] ->
            <<"(?<=", Left/binary, ")", Right/binary>>;
        _ ->
            Bin
    end.

%% Split first \K operator occurrence (best-effort; does not fully parse escapes).
split_keep_operator(Bin) ->
    case binary:split(Bin, <<"\\K">>) of
        [Left, Right] -> {ok, Left, Right};
        _ -> none
    end.

%% Remove PCRE callouts: (?C) and (?C123)
strip_callouts(Bin) ->
    strip_callouts(Bin, <<>>).

strip_callouts(<<>>, Acc) ->
    Acc;
strip_callouts(<<"(?C)", Rest/binary>>, Acc) ->
    strip_callouts(Rest, Acc);
strip_callouts(<<"(?C", Rest/binary>>, Acc) ->
    case take_digits_then_close(Rest, <<>>) of
        {ok, Tail} ->
            strip_callouts(Tail, Acc);
        error ->
            strip_callouts(Rest, <<Acc/binary, "(?C">>)
    end;
strip_callouts(<<C, Rest/binary>>, Acc) ->
    strip_callouts(Rest, <<Acc/binary, C>>).

take_digits_then_close(<<$), Tail/binary>>, _Acc) ->
    {ok, Tail};
take_digits_then_close(<<D, Tail/binary>>, Acc) when D >= $0, D =< $9 ->
    take_digits_then_close(Tail, <<Acc/binary, D>>);
take_digits_then_close(_, _) ->
    error.

replace_all(Bin, Search, Repl) ->
    replace_all(Bin, Search, Repl, <<>>).

replace_all(<<>>, _Search, _Repl, Acc) ->
    Acc;
replace_all(Bin, Search, Repl, Acc) ->
    case binary:match(Bin, Search) of
        nomatch ->
            <<Acc/binary, Bin/binary>>;
        {Pos, Len} ->
            <<Head:Pos/binary, _Skip:Len/binary, Tail/binary>> = Bin,
            replace_all(Tail, Search, Repl, <<Acc/binary, Head/binary, Repl/binary>>)
    end.

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
      "  const MAX_STEPS = 10000;\n"
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
      "  let steps = 0;\n"
      "  let prevLastIndex = re.lastIndex;\n"
      "  do {\n"
      "    if (++steps > MAX_STEPS) break;\n"
      "    try { m = re.exec(subject); }\n"
      "    catch (_) { return [null]; }\n"
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
      "      if (isGlobal && m[0] === '') {\n"
      "        if (re.lastIndex <= prevLastIndex) re.lastIndex = prevLastIndex + 1;\n"
      "      }\n"
      "      prevLastIndex = re.lastIndex;\n"
      "    }\n"
      "  } while (m && isGlobal);\n"
      "  return [results.length === 0 ? null : results];\n"
      "}">>.

%% -------------------------------------------------------------------
%% JS: execution path for patterns containing \K
%% Emulates "keep out" by matching left+right, then recalculating group 0 start.
%% -------------------------------------------------------------------
run_keep_js() ->
    <<"({ args }) => {\n"
      "  const { left, right, flags, subject, offset, global: isGlobal } = args;\n"
      "  const enc = new TextEncoder();\n"
      "  const MAX_STEPS = 10000;\n"
      "  const bytePos = (s, ci) => enc.encode(s.substring(0, ci)).length;\n"
      "  const charPos = (s, bo) => {\n"
      "    let b = 0, c = 0;\n"
      "    while (b < bo && c < s.length) { b += enc.encode(s.charAt(c)).length; c++; }\n"
      "    return c;\n"
      "  };\n"
      "  const stripFlag = (f, ch) => f.split('').filter(x => x !== ch).join('');\n"
      "  let useFlags = flags;\n"
      "  if (offset > 0 && !flags.includes('g')) useFlags += 'g';\n"
      "  let re;\n"
      "  try { re = new RegExp('(?:' + left + ')(?:' + right + ')', useFlags + 'd'); }\n"
      "  catch (e) {\n"
      "    try { re = new RegExp('(?:' + left + ')(?:' + right + ')', useFlags); }\n"
      "    catch (_) { return [null]; }\n"
      "  }\n"
      "  let leftWhole, rightWhole;\n"
      "  try {\n"
      "    const testFlags = stripFlag(stripFlag(useFlags, 'g'), 'd');\n"
      "    leftWhole = new RegExp('^(?:' + left + ')$', testFlags);\n"
      "    rightWhole = new RegExp('^(?:' + right + ')$', testFlags);\n"
      "  } catch (_) {\n"
      "    return [null];\n"
      "  }\n"
      "  if (offset > 0) re.lastIndex = charPos(subject, offset);\n"
      "  const results = [];\n"
      "  let steps = 0;\n"
      "  let m;\n"
      "  let prevLastIndex = re.lastIndex;\n"
      "  do {\n"
      "    if (++steps > MAX_STEPS) break;\n"
      "    try { m = re.exec(subject); }\n"
      "    catch (_) { return [null]; }\n"
      "    if (m) {\n"
      "      const start = m.index;\n"
      "      const end = start + m[0].length;\n"
      "      let keepPos = -1;\n"
      "      for (let ci = start; ci <= end; ci++) {\n"
      "        const l = subject.substring(start, ci);\n"
      "        const r = subject.substring(ci, end);\n"
      "        if (leftWhole.test(l) && rightWhole.test(r)) { keepPos = ci; break; }\n"
      "      }\n"
      "      if (keepPos < 0) keepPos = start;\n"
      "      const hi = !!m.indices;\n"
      "      const indices = [];\n"
      "      const texts = [];\n"
      "      for (let i = 0; i < m.length; i++) {\n"
      "        if (i === 0) {\n"
      "          const s = bytePos(subject, keepPos);\n"
      "          const e = bytePos(subject, end);\n"
      "          indices.push([s, e - s]);\n"
      "          texts.push(subject.substring(keepPos, end));\n"
      "        } else if (m[i] == null) {\n"
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
      "      if (isGlobal && m[0] === '') {\n"
      "        if (re.lastIndex <= prevLastIndex) re.lastIndex = prevLastIndex + 1;\n"
      "      }\n"
      "      prevLastIndex = re.lastIndex;\n"
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
select_groups(all_names, I, T, NameMap) ->
    Len = length(I),
    Pairs0 = maps:to_list(NameMap),
    Pairs1 = lists:sort(fun({A, _}, {B, _}) -> A =< B end, Pairs0),
    Picked = [{lists:nth(Idx + 1, I), lists:nth(Idx + 1, T)} || {_N, Idx} <- Pairs1, Idx < Len],
    {[element(1, P) || P <- Picked],
     [element(2, P) || P <- Picked]};
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
               (N) when is_atom(N) ->
                   case maps:find(atom_to_binary(N, utf8), NameMap) of
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
