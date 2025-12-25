-module(hoblisp).
-export([main/1]).

%% Custom types for Lisp structure
-type hoblisp_atom() :: atom().
-type hoblisp_list() :: [hoblisp_val()].
-type hoblisp_val() :: hoblisp_atom() | hoblisp_list().

%% Types borrowed from epc for local specs
-type parse_result(T) :: {ok, T, string()} | {error, string()}.
-type parser(T) :: fun((string()) -> parse_result(T)).


%% @doc Skip zero or more whitespaces
-spec whitespace() -> parser(string()).
whitespace() ->
    epc:many(epc:choice(epc:char($\s), epc:char($\n))).


%% @doc Parse a symbol or number (at least one character)
-spec atom() -> parser(atom()).
atom() ->
    fun(Input) ->
            %% Use a primitive to ensure at least one character is consumed
            case Input of
                [H | T] when H /= $(, H /= $), H /= $\s, H /= $\n ->
                    %% Consume the rest of the atom
                    {ok, Chars, Rest} = (epc:many(fun([C | R]) when C /= $(, C /= $), C /= $\s, C /= $\n -> {ok, C, R};
                                                     (_) -> {error, "end of atom"}
                                                  end))(T),
                    {ok, list_to_atom([H | Chars]), Rest};
                _ ->
                    {error, "not an atom"}
            end
    end.


%% @doc Parse a list: ( element1 element2 ... )
-spec list_parser() -> parser(hoblisp_list()).
list_parser() ->
    fun(Input) ->
            Parser = epc:map(
                       epc:sequence(
                         epc:char($(),
                         epc:sequence(
                           epc:many(fun(I) -> (element_parser())(I) end),
                           epc:char($)))),
                       fun({_, {Elements, _}}) -> Elements end),
            Parser(Input)
    end.


%% @doc Element parser with whitespace handling
-spec element_parser() -> parser(hoblisp_val()).
element_parser() ->
    fun(Input) ->
            Parser = epc:map(
                       epc:sequence(
                         whitespace(),
                         epc:sequence(
                           epc:choice(list_parser(), atom()),
                           whitespace())),
                       fun({_, {Content, _}}) -> Content end),
            Parser(Input)
    end.


-spec main([string()]) -> ok.
main(_Args) ->
    Input = "(define (add x y) (+ x y))",
    Result = epc:parse(element_parser(), Input),
    io:format("Result: ~p~n", [Result]).
