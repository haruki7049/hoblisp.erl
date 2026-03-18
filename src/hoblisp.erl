-module(hoblisp).
-export([main/1, repl/0]).


%% --- CLI with argparse ---
main(Args) ->
    Cli = #{
            help => "Hoblisp Interpreter using epc",
            commands => #{
                          "repl" => #{
                                      help => "Start the interactive REPL",
                                      handler => fun(_) -> repl() end
                                     },
                          "eval" => #{
                                      help => "Evaluate a given S-expression string",
                                      arguments => [#{name => expr, type => string, help => "Expression to evaluate"}],
                                      handler => fun(#{expr := Expr}) ->
                                                         BinExpr = list_to_binary(Expr),
                                                         try
                                                             io:format("~p~n", [eval_string(BinExpr)])
                                                         catch
                                                             _:Err -> io:format("Error: ~p~n", [Err])
                                                         end
                                                 end
                                     }
                         }
           },
    argparse:run(Args, Cli, #{progname => "hoblisp"}).


%% --- REPL ---
repl() ->
    io:format("Starting Hoblisp REPL. Press Ctrl+C to exit.~n"),
    repl_loop(default_env()).


repl_loop(Env) ->
    case io:get_line("hoblisp> ") of
        eof -> ok;
        Line ->
            BinLine = list_to_binary(Line),
            try
                case parse_input(BinLine) of
                    {ok, Ast, _Rest} ->
                        {Val, Env2} = eval(Ast, Env),
                        io:format("~p~n", [Val]),
                        repl_loop(Env2);
                    {error, Reason} ->
                        io:format("Parse Error: ~p~n", [Reason]),
                        repl_loop(Env)
                end
            catch
                _:Err ->
                    io:format("Eval Error: ~p~n", [Err]),
                    repl_loop(Env)
            end
    end.


eval_string(BinStr) ->
    case parse_input(BinStr) of
        {ok, Ast, _} ->
            {Val, _} = eval(Ast, default_env()),
            Val;
        {error, Reason} ->
            throw({parse_error, Reason})
    end.


%% --- Parser using epc ---
parse_input(Input) ->
    Parser = sexp_parser(),
    epc:parse(Parser, Input).


%% epc_sexp_SUITE.erl に基づいたS式パーサー
sexp_parser() ->
    LParen = epc:token(epc:char($()),
    RParen = epc:token(epc:char($))),

    %% 整数
    MinusP = epc:optional(epc:char($-)),
    DigitsP = epc:many1(epc:digit()),
    NumberP = epc:token(
                epc:map(
                  epc:sequence(MinusP, DigitsP),
                  fun({Minus, Ds}) ->
                          Int = list_to_integer(Ds),
                          case Minus of
                              undefined -> Int;
                              $- -> -Int
                          end
                  end)),

    %% 文字列
    Quote = epc:char($"),
    StringContent = epc:many(epc:none_of("\"")),
    StringP = epc:token(
                epc:map(
                  epc:sequence(Quote, epc:sequence(StringContent, Quote)),
                  fun({_, {Str, _}}) -> list_to_binary(Str) end)),

    %% シンボル
    IsSymChar = fun(C) ->
                        (C >= $a andalso C =< $z) orelse
                        (C >= $A andalso C =< $Z) orelse
                        (C >= $0 andalso C =< $9) orelse
                        lists:member(C, "+-*/_=<>!")
                end,
    SymbolP = epc:token(
                epc:map(
                  epc:many1(epc:satisfy(IsSymChar)),
                  fun(Chars) -> {symbol, list_to_binary(Chars)} end)),

    %% S式全体 (遅延評価による再帰)
    SExpP = fun F() ->
                    epc:choice([NumberP,
                                StringP,
                                SymbolP,
                                %% リスト: ( expr ... )
                                epc:map(
                                  epc:sequence(LParen, epc:sequence(epc:many(epc:lazy(F)), RParen)),
                                  fun({_, {Elements, _}}) -> Elements end)])
            end,

    %% 行頭のスペースを無視
    epc:map(
      epc:sequence(epc:spaces(), SExpP()),
      fun({_, FinalValue}) -> FinalValue end).


%% --- Evaluator ---
default_env() ->
    #{
      ~"+" => fun([A, B]) -> A + B end,
      ~"-" => fun([A, B]) -> A - B end,
      ~"*" => fun([A, B]) -> A * B end,
      ~"/" => fun([A, B]) -> A div B end,
      ~"=" => fun([A, B]) -> A =:= B end,
      ~"<" => fun([A, B]) -> A < B end,
      ~">" => fun([A, B]) -> A > B end
     }.


%% 数値・文字列の評価
eval(Num, Env) when is_integer(Num) -> {Num, Env};
eval(Str, Env) when is_binary(Str) -> {Str, Env};

%% シンボルの評価（環境から変数を取得）
eval({symbol, Sym}, Env) ->
    case maps:find(Sym, Env) of
        {ok, Val} -> {Val, Env};
        error -> throw({unbound_symbol, Sym})
    end;

%% 特殊形式: define (変数定義)
eval([{symbol, ~"define"}, {symbol, Sym}, Expr], Env) ->
    {Val, Env1} = eval(Expr, Env),
    {Val, Env1#{Sym => Val}};

%% 特殊形式: if
eval([{symbol, ~"if"}, Cond, Then, Else], Env) ->
    {CondVal, Env1} = eval(Cond, Env),
    case CondVal of
        false -> eval(Else, Env1);
        _ -> eval(Then, Env1)
    end;

%% 関数適用
eval([Op | Args], Env) ->
    {OpVal, Env1} = eval(Op, Env),
    {EvalArgs, Env2} = eval_list(Args, Env1, []),
    {apply_fn(OpVal, EvalArgs), Env2}.


%% 引数リストの順次評価
eval_list([], Env, Acc) -> {lists:reverse(Acc), Env};
eval_list([H | T], Env, Acc) ->
    {Val, Env1} = eval(H, Env),
    eval_list(T, Env1, [Val | Acc]).


apply_fn(Fn, Args) when is_function(Fn) -> Fn(Args);
apply_fn(_, _) -> throw(not_a_function).
