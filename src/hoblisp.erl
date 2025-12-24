-module(hoblisp).
-export([main/1]).


-spec main([string()]) -> ok.
main(Args) ->
    io:format("~w~n", [Args]).
