-module(hoblisp).
-export([main/1]).


main(Args) ->
    io:format("~w~n", [Args]).
