%% -------------------------------------------------------------------
%% Created: 22 Dec 2011 by etnt@redhoterlang.com
%%
%% @doc Erlang flavored by Some ML
%%
%% -------------------------------------------------------------------
-module(eml).

-export([c/1
         , compiler/1
	 , compile_examples/0
	 , compile_file/1
	 , compile_file/2
         , e/1
         , f/1
         , l/1
         , lexer/1
         , p/1
         , parse/1
         , parser/1
         , typecheck/1
        ]).

-include("eml.hrl").
-include_lib("eunit/include/eunit.hrl").

compile_examples() ->
    Files = string:tokens(os:cmd("ls examples/*.eml"), "\n"),
    F = fun(File) ->
		{ok,_} = compile_file(File),
		io:format("Compiled file ~s~n",[File])
	end,
    [F(File) || File <- Files].

compile_file(FileName) ->
    compile_file(FileName, []).
    
compile_file(FileName, Opts) ->
    ModName = filename:basename(FileName,".eml"),
    DirName = filename:dirname(FileName),
    {ok,EmlForms} = parse(FileName),
    dump(FileName++".parse", EmlForms, Opts),
    CurryForms = eml_compile:curry(EmlForms),
    {ok,ErlForms} = compiler(CurryForms),
    dump(FileName++".compile", ErlForms, Opts),
    String = to_erl(ModName,ErlForms),
    file:write_file(filename:join([DirName,ModName++".erl"]),
		    list_to_binary(String)),
    compile:file(filename:join([DirName,ModName]), Opts ++ [{outdir,DirName}]).

to_erl(Module,Forms) when is_list(Module) ->
    Mx = erl_syntax:attribute(erl_syntax:atom("module"),
			      [erl_syntax:atom(Module)]),

    Es = [erl_syntax:arity_qualifier(
	    erl_syntax:atom(FunName),
	    erl_syntax:integer(Arity))
	  || {function,_,FunName,Arity,_} <- Forms],

    Ex = erl_syntax:attribute(
	   erl_syntax:atom("export"),
	   [erl_syntax:list(Es)]),

    erl_prettypr:format(erl_syntax:form_list([Mx,Ex]++Forms)).


dump(FileName, Data, Opts) ->
    case lists:keyfind(verbose,1,Opts) of
        {verbose,true} ->
            {ok,Fd} = file:open(FileName, write),
            io:format(Fd, "~p~n", [Data]),
            file:close(Fd);
        _ ->
            false
    end.


f(FileName) -> 
    {ok,Forms} = parse(FileName),
    eml_compile:curry(Forms).

c(S) -> e2(compiler(e2(parser(e2(lexer(S)))))).
e(S) -> eml_compile:erl_form(S).
p(S) -> parser(e2(lexer(S))).
l(S) -> lexer(S).


lexer(String) when is_list(String) ->
    eml_lexer:string(String).

parser(Tokens) when is_list(Tokens) ->
    eml_parser:parse(Tokens).

typecheck(ParseTree) ->
    eml_typecheck:run(ParseTree).

compiler(ParseTree) ->
    eml_compile:run(ParseTree).


parse(FileName) ->
    {ok, InFile} = file:open(FileName, [read]),
    Acc = loop(InFile,[]),
    file:close(InFile),
    eml_parser:parse(Acc).

loop(InFile,Acc) ->
    case io:request(InFile,{get_until,prompt,eml_lexer,token,[1]}) of
        {ok,Token,_EndLine} ->
            loop(InFile,Acc ++ [Token]);
        {error,token} ->
            exit(scanning_error);    
        {eof,_} ->
            Acc
    end.



-ifdef(EUNIT).

compiler_test_() ->
    [
     ?_assertEqual([e("add1(X) -> X + 1.")],
                   c("fun add1 X = X + 1;"))

     ,?_assertEqual([e("len([H|T]) -> 1 + len(T);\nlen([]) -> 0.")],
                    c("fun len [H|T] = 1 + len(T)\n| len [] = 0;"))

     ,?_assertEqual([e("add3(Y) -> fun (X) -> X + Y end(3).")],
                    c("fun add3 Y = let val X = 3 in X + Y;"))

     ,?_assertEqual([e("expr(Y) -> ((fun (X) -> fun (Z) -> X*Y+Z end end)(2))(1).")],
                    c("fun expr Y = let val X = 2 val Z = 1 in X*Y+Z;"))

     ,?_assertEqual([e("add(Y) -> (fun ({X,Z}) -> X + Z end)(Y).")],
                    c("fun add Y = let val {X,Z} = Y in X + Z;"))

     ,?_assertEqual([e("add(X,Y) -> X + Y.")],
                    c("fun add X Y = X + Y;"))

     ,?_assertEqual([e("qsort([H | T]) ->  (fun (GrEq) -> fun (Le) -> GrEq ++ [H] ++ Le end   end(qsort([X || X <- T, X >= H])))(qsort([X  || X <- T, X < H])); qsort([]) -> [].")],
                    c("fun qsort [H|T] = let val GrEq = qsort([X || X <- T, X >= H]) val Le  = qsort([X || X <- T, X < H]) in GrEq ++ [H] ++ Le | qsort [] = [];"))

     ,?_assertEqual([eml:e("add(X) -> fun(Y) -> X + Y end.")],
                    eml:c("fun add X = fn Y => X + Y;"))


    ].


parser_test_() ->
    [


     ?_assertMatch({ok,[{function,1,foo,0,
                         [{clause,1,[],[],
                           [{call,1,
                             {atom,1,foldl},
                             [{op_fun,1,'+'},
                              {integer,1,0},
                              {cons,1,
                               {integer,1,1},
                               {cons,1,
                                {integer,1,2},
                                {cons,1,{integer,1,3},{nil,1}}}}]}]}]}]
                   },
                   p("fun foo = foldl(@+, 0, [1,2,3]);"))


%     ?_assertMatch({ok,
%                    {function,1,len,1,
%                     [{clause,1,
%                       [{cons,1,{var,1,'H'},{var,1,'T'}}],
%                       [],
%                       [{op,1,'+',
%                         {integer,1,1},
%                         {call,1,[],len,[{var,1,'T'}]}}]},
%                      {clause,2,[{nil,2}],[],[{integer,2,0}]}]}},
%
%                   p("fun len [H|T] = 1 + len T\n| len [] = 0;"))


%     ,?_assertMatch({ok,{function,1,add,1,
%                         [{clause,2,
%                           [{var,1,'X'}],
%                           [],
%                           [{'let',2,
%                             [{val,2,{var,2,'Y'},{integer,2,2}}],
%                             [{op,2,'+',
%                               {var,2,'X'},
%                               {var,2,'Y'}}]}]}]}},
%
%                    p("fun add X =\n let val Y = 2 in X + Y;"))
     
    ].

lexer_test_() ->
    [

     
     ?_assertMatch({ok,[{atom,1,len},
                        {'[',1},
                        {atom,1,h},
                        {'|',1},
                        {atom,1,t},
                        {']',1},
                        {'=',1},
                        {integer,1,1},
                        {'+',1},
                        {atom,1,len},
                        {atom,1,t},
                        {atom,2,len},
                        {'[',2},
                        {']',2},
                        {'=',2},
                        {integer,2,0}],
                    2},
                   l("len [h|t] = 1 + len t\nlen [] = 0"))

     ,?_assertMatch({ok,[{atom,1,foldl},
                         {'(',1},
                         {'@+',1},
                         {',',1},
                         {integer,1,0},
                         {',',1},
                         {'[',1},
                         {integer,1,1},
                         {',',1},
                         {integer,1,2},
                         {',',1},
                         {integer,1,3},
                         {']',1},
                         {')',1}],
                     1},
                    l("foldl(@+, 0, [1,2,3])"))

    ].

e2(T) when is_tuple(T) -> element(2, T).


-endif.
