-module(hoblisp).
-export([main/1]).


-spec main([string()]) -> ok.
main(_Args) ->
    {ok, _} = application:ensure_all_started(hoblisp),
    epc:echo().
