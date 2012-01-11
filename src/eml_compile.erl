%% -------------------------------------------------------------------
%% Created: 22 Dec 2011 by etnt@redhoterlang.com
%%
%% @doc The compiler
%%
%% -------------------------------------------------------------------

-module(eml_compile).

-export([curry/1
         , run/1
         , print/1
         , erl_form/1
        ]).

-include("eml.hrl").

-define(e, erl_syntax).



erl_form(ErlStr) ->
    {ok,Form} = erl_parse:parse_form(element(2,erl_scan:string(ErlStr))),
    Form.
    

%%8> erl_parse:parse_form(element(2,erl_scan:string("add1(X) -> X + 1.")))
%%.
%%{ok,{function,1,add1,1,
%%              [{clause,1,
%%                       [{var,1,'X'}],
%%                       [],
%%                       [{op,1,'+',{var,1,'X'},{integer,1,1}}]}]}}
%%
%%9> element(2,eml:p("fun add1 X = X + 1;")).           
%%{function,1,add1,1,
%%          [{clause,1,
%%                   [{var,1,'X'}],
%%                   [],
%%                   [{arith,1,'+',{var,1,'X'},{integer,1,1}}]}]}


print(Forms) ->
    X = erl_prettypr:format(?e:form_list([?e:revert(Forms)]),
                            [{paper,160},{ribbon,80}]),
    io:format("~p~n",[X]).
    


run(ParseTrees) when is_list(ParseTrees) ->
    {ok, [r(ParseTree) || ParseTree <- ParseTrees]}.

r({function,Line,Name,Arity,Clauses}) ->
    put(used_vars, vars(Clauses)),
    {function,Line,Name,Arity,[r(C) || C <- Clauses]};

r({'fun',Line,{clauses,Clauses}}) ->
    {'fun',Line,{clauses,[r(C) || C <- Clauses]}};

r({clause,Line,FormalArgs,Guards,Exprs}) ->
    {clause,Line,FormalArgs,Guards,[r(E) || E <- Exprs]};

r({call,Line,Fun,Args}) ->
    {call,Line,Fun,[r(A) || A <- Args]};

r({'let',Line,[{val,_Line,FormalArg,ActualArg}|E],Exprs}) ->
    %%
    %% We transform a let as described in SPJ:
    %%
    %%  (let v1 = B1, v2 = B2 in E) ==
    %%    (let v1 = B1 
    %%       in let v2 = B2 in E) == 
    %%  (let v1 = B1 in E' == ((\v1.E')B1)  ,  E' == (let v2 = B2 in E)
    %%
    {call,Line,
     {'fun',Line,
      {clauses,
       [{clause,Line,[FormalArg],[],  %% r(FormalArg1) ??
         return(r({'let',Line,E,Exprs}))}]}},
     [r(ActualArg)]};

r({'let',Line,[{rec,_Line,Var,FunExpr}|E],Exprs}) ->
    %%
    %% We transform a 'let rec' to a let + the use of the Y combinator
    %% as described in SPJ:
    %%
    %%  (let rec v = B in E) ==   ,  B=<anonymous-function>
    %%     let val Y = <y-combinator>
    %%     in let val v = Y(\v.B) in E)
    %%
    Yvar = get_non_used_var(),
    Arity = arity(FunExpr),
    Vs = [y(Line,Yvar,Arity),
          {val, Line, Var,
           {call, Line, {var,Line,Yvar},
            [{'fun', Line,
              {clauses,
                 [{clause, Line, [Var],[], [FunExpr]}]}}]}}
          |E],
    r({'let',Line,Vs,Exprs});

r({'let',_Line,[],Exprs}) ->
    [r(E) || E <- Exprs];


r({'case', Line, Expr, Clauses}) ->
    %%
    %% We transform case expressions as:
    %%
    %% (case E of P1 => B1 | P2 => B2 end) ==
    %%    (let v0 = E in (fn P1 = B1 | fn P2 = B2)(v0))
    %%
    Var = get_non_used_var(),
    R = {'let', Line,
         [{val, Line, {var, Line, Var}, Expr}],
         [{call, Line, {'fun', Line, {clauses, Clauses}}, [{var, Line, Var}]}]},
    r(R);


r({op_fun, Line, Op}) ->
    %%
    %% Corresponds to '@+' as in: foldl(@+, 0, [1,2,3])
    %%
    {'fun',Line,
     {clauses,
      [{clause,Line,
        [{var,Line,'X'},{var,Line,'Y'}],
        [],
        [{op,Line,Op,{var,Line,'X'},{var,Line,'Y'}}]}]}};


r(ParseTree) ->
    ParseTree.

%% Require ('used_vars',VarList) in process dictionary! 
get_non_used_var() ->
    true = erlang:is_list(get(used_vars)), % assert!
    list_to_atom(get_non_used_var("_EML_", 1)).

get_non_used_var(V,N) when is_list(V), is_integer(N) ->
    Var = V ++ integer_to_list(N),
    case lists:member(Var, get(used_vars)) of
        false -> Var;
        true  -> get_non_used_var(V,N+1)
    end.

return(List) when is_list(List) -> List;
return(Term)                    -> [Term].
    

%% FIXME: Get non-used variables in Var&FunBody
%%        Also, only one Y function is required in case of multiple letrec's
%%
%% The Y-combinator:
%%
%%  let val Y = 
%%    fn M =>
%%      (let val G = fn F => M(fn A => (F(F))(A))
%%       in  G(G))
%%
y(Line,Var, Arity) ->
    Vs = [{var,Line,list_to_atom("A"++integer_to_list(I))}
          || I <- lists:seq(1,Arity)],
    {val,Line,
     {var,Line,Var},
     {'fun',Line,
      {clauses,
       [{clause,Line,
         [{var,Line,'M'}],
         [],
         [{'let',Line,
           [{val,Line,
             {var,Line,'G'},
             {'fun',Line,
              {clauses,
               [{clause,Line,
                 [{var,Line,'F'}],
                 [],
                 [{call,Line,
                   {var,Line,'M'},
                   [{'fun',Line,
                     {clauses,
                      [{clause,Line,Vs,[],
                        [{call,Line,
                          {call,Line,{var,Line,'F'},[{var,Line,'F'}]},
                          Vs}]}]}}]}]}]}}}],
           [{call,Line,{var,Line,'G'},[{var,Line,'G'}]}]}]}]}}}.


%% Compute the arity of an anonymous function
arity({'fun',_,{clauses,[{clause,_,FormalArgs,_,_}|_]}}) -> length(FormalArgs).

%%% Extract all variables used
vars(R) -> ordsets:to_list(vars(R,ordsets:new())).

vars({var,_,Var}, Acc) -> [Var|Acc];
vars(Tuple,Acc) when is_tuple(Tuple) ->
    lists:foldl(fun(X,Acc1) -> ordsets:union(vars(X),Acc1) end,
                Acc, tuple_to_list(Tuple));
vars(List,Acc) when is_list(List) ->
    lists:foldl(fun(X,Acc1) -> ordsets:union(vars(X),Acc1) end,
                Acc, List);
vars(_,Acc) ->
    Acc.
    

curry([H|T]) -> c([H]) ++ curry(T);
curry([])    -> [].


c([{function,Line,Name,Arity,_Clauses}=H|T]) when Arity > 0 ->
    Vs = [{var,Line,l2a("X"++i2l(N))} || N <- lists:seq(1,Arity)],
    [Last|RevFirst] = lists:reverse(Vs),
    c([{function,Line,Name,Arity-1,
        [{clause,Line,lists:reverse(RevFirst),[],
          [{'fun',Line,
            {clauses,
             [{clause,Line,
               [Last],[],
               [{call,Line,{atom,1,Name},Vs}]}]}}]}]},
       H|T]);
c(L) ->
    L.

l2a(L) when is_list(L)    -> list_to_atom(L).
i2l(I) when is_integer(I) -> integer_to_list(I).

e3(T) -> element(3,T).
e4(T) -> element(4,T).
