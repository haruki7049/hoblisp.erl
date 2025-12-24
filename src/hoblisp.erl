-module(hoblisp).
-export([main/1]).


-spec main([string()]) -> ok.
main(_Args) ->
    {ok, _} = application:load(epc),
    epc:echo().
