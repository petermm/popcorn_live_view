-module(zlib).
-export([
    uncompress/1,
    compress/1,
    gunzip/1,
    gzip/1,
    unzip/1,
    zip/1
]).

%% ===================================================================
%% :zlib stub for AtomVM/WASM
%%
%% Delegates inflate/deflate operations to fflate (pure-JS synchronous
%% zlib library) via Popcorn.Wasm.run_js!.
%%
%% Requires fflate to be loaded in the host page:
%%   <script src="assets/fflate.js"></script>
%% fflate sets window.fflate; run_js! JS sees `window` = parent page.
%%
%% Binary data is base64-encoded for safe JSON transport.
%%
%% Supported:
%%   uncompress/1  — zlib inflate  (RFC 1950)
%%   compress/1    — zlib deflate  (RFC 1950)
%%   gunzip/1      — gzip inflate  (RFC 1952)
%%   gzip/1        — gzip deflate  (RFC 1952)
%%   unzip/1       — raw inflate   (RFC 1951)
%%   zip/1         — raw deflate   (RFC 1951)
%% ===================================================================

%% -------------------------------------------------------------------
%% Public API
%% -------------------------------------------------------------------

uncompress(Data) ->
    call_fflate(<<"unzlibSync">>, Data).

compress(Data) ->
    call_fflate(<<"zlibSync">>, Data).

gunzip(Data) ->
    call_fflate(<<"gunzipSync">>, Data).

gzip(Data) ->
    call_fflate(<<"gzipSync">>, Data).

unzip(Data) ->
    call_fflate(<<"inflateSync">>, Data).

zip(Data) ->
    call_fflate(<<"deflateSync">>, Data).

%% ===================================================================
%% Internal
%% ===================================================================

call_fflate(Fn, Data) ->
    B64In = base64:encode(iolist_to_binary(Data)),
    Args = #{fn => Fn, data => B64In},
    case 'Elixir.Popcorn.Wasm':'run_js!'(fflate_js(), Args, [{return, value}]) of
        B64Out when is_binary(B64Out) ->
            base64:decode(B64Out);
        Error ->
            error({zlib_stub_error, Error})
    end.

%% -------------------------------------------------------------------
%% JS: dispatch to fflate.<fn>Sync, transfer data as base64
%%
%% `window` in run_js! scope = parent page window where fflate is
%% loaded as a <script> tag.
%%
%% b64u: base64 string  → Uint8Array
%% ub64: Uint8Array     → base64 string
%% -------------------------------------------------------------------
fflate_js() ->
    <<"({ args }) => {\n"
      "  const b64u = b => {\n"
      "    const s = atob(b);\n"
      "    const u = new Uint8Array(s.length);\n"
      "    for (let i = 0; i < s.length; i++) u[i] = s.charCodeAt(i);\n"
      "    return u;\n"
      "  };\n"
      "  const ub64 = u => {\n"
      "    let s = '';\n"
      "    for (let i = 0; i < u.length; i++) s += String.fromCharCode(u[i]);\n"
      "    return btoa(s);\n"
      "  };\n"
      "  const fn = window.fflate[args.fn];\n"
      "  if (!fn) return ['error:fflate_not_loaded'];\n"
      "  try {\n"
      "    return [ub64(fn(b64u(args.data)))];\n"
      "  } catch(e) {\n"
      "    return ['error:' + e.message];\n"
      "  }\n"
      "}">>.
