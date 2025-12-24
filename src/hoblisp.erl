-module(hoblisp).
-export([main/1]).


-spec main([string()]) -> ok.
main(_Args) ->
    epc:echo().
