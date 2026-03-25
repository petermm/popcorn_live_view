-module(persistent_term).
-export([get/0, get/1, get/2, put/2, erase/1, info/0]).

storage_key(Key) ->
    {?MODULE, Key}.

put(Key, Value) ->
    erlang:put(storage_key(Key), {value, Value}),
    ok.

get() ->
    entries().

get(Key) ->
    case erlang:get(storage_key(Key)) of
        {value, Value} ->
            Value;
        undefined ->
            erlang:error(badarg)
    end.

get(Key, Default) ->
    case erlang:get(storage_key(Key)) of
        {value, Value} ->
            Value;
        undefined ->
            Default
    end.

erase(Key) ->
    case erlang:erase(storage_key(Key)) of
        {value, _Value} ->
            true;
        undefined ->
            false
    end.

info() ->
    #{count => length(entries()), memory => 0}.

entries() ->
    [{Key, Value} || {{persistent_term, Key}, {value, Value}} <- erlang:get()].
