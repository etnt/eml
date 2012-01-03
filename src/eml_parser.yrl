%% ---------------------------------------------------------------------
%%
%% Modified: 22 Dec 2011 by etnt@redhoterlang.com
%%
%% ---------------------------------------------------------------------
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
Nonterminals
prefix_op add_op comp_op list_op
attribute atomic basic_type bif_test
clause_body
clause_guard clause_head
expr expr_100 expr_150 expr_160 expr_200 expr_300 expr_400 expr_500
expr_600 expr_700 expr_800 expr_900
expr_max expr_tail
exprs farity farity_list
forms form formal_parameter_list argument_list
case_expr cr_clauses cr_clause
function function_call function_clause
fun_expr fun_clauses fun_clause fun_clause_head fun_cr_clause_body
guard guard_call guard_expr
let_expr val_exprs val_expr
list_comprehension binary_comprehension
lc_exprs lc_expr binary bin_elements bin_element bit_expr
opt_bit_size_expr opt_bit_type_list bit_type_list bit_type bit_size_expr
guard_expr_list guard_exprs guard_expr_tail guard_expr_tuple
guard_parameter_list
guard_tests guard_test list
%if_clause if_clauses
mult_op
pattern patterns comma_patterns pattern_list pattern_tail pattern_tuple
tuple strings.

Terminals
'(' ')' '*' '+' ',' '-' '/' '/=' ':' ';' '<' '=' '=/=' '=:='
'<<' '>>' '<-' '<=' '=<' '==' '>' '>=' '[' ']' '.' 'band' 'bnot' 
'fun' 'val' 'rec'
'bor' 'bsl' 'bsr' 'bxor' 'div' 'let' 'in' 'fn' '=>'
'case' 'of'
'orelse' 'andalso' 'not' 'and' 'or' 'xor' '++' '--'
'rem' '{' '|' '||' '}' 'when' atom float integer string var.


Rootsymbol forms.

forms -> form       : ['$1'].
forms -> form forms : ['$1'|'$2'].

form -> '-' atom '(' attribute ')' :
   {attribute, element(2, '$2'), element(3, '$2'), '$4'}.
form -> function : '$1'.


attribute -> atom : element(3, '$1').
attribute -> '[' farity_list ']' : '$2'.

farity_list -> farity : ['$1'].
farity_list -> farity ',' farity_list : ['$1' | '$3'].

farity -> atom '/' integer : {element(3, '$1'), element(3, '$3')}.


function -> function_clause ';' : '$1'.
function -> function_clause function :
   case '$1' of
       {function, Pos1, Name1, Arity1, [Clause]} ->
	   case '$2' of
	       {function, _, Name1, Arity2, Clauses} ->
		   if
		       Arity1 /= Arity2 ->
			   throw({error, {Pos1, yecc,
				  io_lib:format('arity conflict in definition of ~w',
						[Name1])}});
		       true ->
			   {function, Pos1, Name1, Arity1, [Clause | Clauses]}
		   end;
	       _ ->
		   throw({error, {Pos1, yecc,
			  io_lib:format('missing final semicolon in def of ~w/~w',
					[Name1, Arity1])}})
	   end
   end.

function_clause -> clause_head clause_guard clause_body :
   {Name, Line, Arity, Parameters} = '$1',
   {function, Line, Name, Arity,
    [{clause, element(2, hd('$3')), Parameters, '$2', '$3'}]}.

clause_head -> 'fun' atom formal_parameter_list :
   {element(3, '$2'), element(2, '$2'), length('$3'), '$3'}.

clause_head -> '|' atom formal_parameter_list :
   {element(3, '$2'), element(2, '$2'), length('$3'), '$3'}.

formal_parameter_list -> patterns : '$1'.
formal_parameter_list -> '$empty' : [].

clause_guard -> 'when' guard : '$2'.
clause_guard -> '$empty' : [].

clause_body -> '=' exprs: '$2'.

patterns -> pattern : ['$1'].
patterns -> pattern patterns : ['$1' | '$2'].

comma_patterns -> pattern : ['$1'].
comma_patterns -> pattern ',' comma_patterns : ['$1' | '$3'].

pattern -> basic_type : '$1'.
pattern -> pattern_list : '$1'.
pattern -> pattern_tuple : '$1'.

pattern_list -> '[' ']' : {nil, ?line('$1')}.
pattern_list -> '[' pattern pattern_tail ']' :
   case '$3' of
       {nil,0} -> {cons, ?line('$1'), '$2', {nil, ?line('$1')}};
       _       -> {cons, ?line('$1'), '$2', '$3'}
   end.

pattern_tail -> '|' pattern : '$2'.
pattern_tail -> ',' pattern pattern_tail :
   case '$3' of
       {nil,0} -> {cons, ?line('$2'), '$2', {nil, ?line('$2')}};
       _       -> {cons, ?line('$2'), '$2', '$3'}
   end.
pattern_tail -> '$empty' : {nil,0}.

pattern_tuple -> '{' '}' : {tuple, element(2, '$1'), []}.
pattern_tuple -> '{' comma_patterns '}' : {tuple, element(2, '$1'), '$2'}.


exprs -> expr : ['$1'].
exprs -> expr ',' exprs : ['$1' | '$3'].

expr -> expr_100 : '$1'.

% No Erlang match expressions are allowed. Use 'let' instead!
%expr_100 -> expr_150 '=' expr_100 : {match,?line('$2'),'$1','$3'}.
%expr_100 -> expr_150 '!' expr_100 : ?mkop2('$1', '$2', '$3').
expr_100 -> expr_150 : '$1'.

expr_150 -> expr_160 'orelse' expr_150 : ?mkop2('$1', '$2', '$3').
expr_150 -> expr_160 : '$1'.

expr_160 -> expr_200 'andalso' expr_160 : ?mkop2('$1', '$2', '$3').
expr_160 -> expr_200 : '$1'.

expr_200 -> expr_300 comp_op expr_300 :
	?mkop2('$1', '$2', '$3').
expr_200 -> expr_300 : '$1'.

expr_300 -> expr_400 list_op expr_300 :
	?mkop2('$1', '$2', '$3').
expr_300 -> expr_400 : '$1'.

expr_400 -> expr_400 add_op expr_500 :
	?mkop2('$1', '$2', '$3').
expr_400 -> expr_500 : '$1'.

expr_500 -> expr_500 mult_op expr_600 :
	?mkop2('$1', '$2', '$3').
expr_500 -> expr_600 : '$1'.

expr_600 -> prefix_op expr_700 :
	?mkop1('$1', '$2').
expr_600 -> expr_700 : '$1'.

expr_700 -> function_call : '$1'.
%%expr_700 -> record_expr : '$1'.
expr_700 -> expr_800 : '$1'.

expr_800 -> expr_900 ':' expr_max :
	{remote,?line('$2'),'$1','$3'}.
expr_800 -> expr_900 : '$1'.

expr_900 -> '.' atom :
	{record_field,?line('$1'),{atom,?line('$1'),''},'$2'}.
expr_900 -> expr_900 '.' atom :
	{record_field,?line('$2'),'$1','$3'}.
expr_900 -> expr_max : '$1'.

expr_max -> basic_type : '$1'.
expr_max -> list : '$1'.
expr_max -> binary : '$1'.
expr_max -> list_comprehension : '$1'.
expr_max -> binary_comprehension : '$1'.
expr_max -> tuple : '$1'.
%%expr_max -> struct : '$1'.
expr_max -> '(' expr ')' : '$2'.
%%expr_max -> 'begin' exprs 'end' : {block,?line('$1'),'$2'}.
%%expr_max -> if_expr : '$1'.
expr_max -> case_expr : '$1'.
%%expr_max -> receive_expr : '$1'.
expr_max -> fun_expr : '$1'.
%%expr_max -> try_expr : '$1'.
%%expr_max -> query_expr : '$1'.
expr_max -> let_expr : '$1'.

basic_type -> atomic : '$1'.
basic_type -> var : '$1'.

list -> '[' ']' : {nil, ?line('$1')}.
list -> '[' expr expr_tail ']' :
   case '$3' of
       {nil,0} -> {cons, ?line('$1'), '$2', {nil, ?line('$1')}};
       _       -> {cons, ?line('$1'), '$2', '$3'}
   end.

expr_tail -> '|' expr : '$2'.
expr_tail -> ',' expr expr_tail :
   case '$3' of
       {nil,0} -> {cons, ?line('$2'), '$2', {nil, ?line('$2')}};
       _       -> {cons, ?line('$2'), '$2', '$3'}
   end.
expr_tail -> '$empty' : {nil,0}.

tuple -> '{' '}' : {tuple, ?line('$1'), []}.
tuple -> '{' exprs '}' : {tuple, ?line('$1'), '$2'}.

%% -----------------------------------------------------------------
%% CASE EXPRESSION
%% ---------------
%%
%%  case Expr of
%%     Pat1 => Body1
%%   | Pat2 => Body2
%%
%% -----------------------------------------------------------------
case_expr -> 'case' expr 'of' cr_clauses :
        {'case',?line('$1'),'$2','$4'}.

cr_clauses -> cr_clause : ['$1'].
cr_clauses -> cr_clause '|' cr_clauses : ['$1' | '$3'].

cr_clause -> formal_parameter_list clause_guard fun_cr_clause_body :
        {clause,?line(hd('$1')),'$1','$2','$3'}.


%% -----------------------------------------------------------------
%% FUN EXPRESSION
%% ---------------
%%
%%  fn Pat1  => Body
%%   | fn Pat2 => Body2
%%
%% -----------------------------------------------------------------
fun_expr -> fun_clauses : build_fun('$1').

fun_clauses -> fun_clause : ['$1'].
fun_clauses -> fun_clause '|' fun_clauses : ['$1'|'$3'].

fun_clause -> fun_clause_head clause_guard fun_cr_clause_body :
   {'fn', Line, Parameters} = '$1',
   {clause, Line, 'fun', Parameters, '$2', '$3'}.

fun_clause_head -> 'fn' formal_parameter_list :
   {'fn', ?line('$1'), '$2'}.

fun_cr_clause_body -> '=>' exprs : '$2'.


function_call -> expr_800 argument_list :
   {call,?line('$1'),'$1', element(1,'$2')}.

argument_list -> '(' ')' : {[],?line('$1')}.
argument_list -> '(' exprs ')' : {'$2',?line('$1')}.


%if_expr -> 'if' if_clauses 'end' : {'if', element(2, '$1'), '$2'}.
%if_expr -> 'if' if_clauses : {'if', element(2, '$1'), '$2'}.

%if_clause -> guard clause_body : {clause, element(2, hd('$2')), '$1', '$2'}.

%if_clauses -> if_clause : ['$1'].
%if_clauses -> if_clause ';' if_clauses : ['$1' | '$3'].


let_expr -> 'let' val_exprs 'in' exprs :
   {'let', ?line('$1'), '$2', '$4'}.

val_exprs -> val_expr : ['$1'].
val_exprs -> val_expr val_exprs : ['$1' | '$2'].

val_expr -> 'val' pattern '=' expr : {'val', ?line('$1'), '$2', '$4'}.
val_expr -> 'rec' var '=' fun_expr : {'rec', ?line('$1'), '$2', '$4'}.



list_comprehension -> '[' expr '||' lc_exprs ']' :
	{lc,?line('$1'),'$2','$4'}.
binary_comprehension -> '<<' binary '||' lc_exprs '>>' :
	{bc,?line('$1'),'$2','$4'}.
lc_exprs -> lc_expr : ['$1'].
lc_exprs -> lc_expr ',' lc_exprs : ['$1'|'$3'].

lc_expr -> expr : '$1'.
lc_expr -> expr '<-' expr : {generate,?line('$2'),'$1','$3'}.
lc_expr -> binary '<=' expr : {b_generate,?line('$2'),'$1','$3'}.

binary -> '<<' '>>' : {bin,?line('$1'),[]}.
binary -> '<<' bin_elements '>>' : {bin,?line('$1'),'$2'}.

bin_elements -> bin_element : ['$1'].
bin_elements -> bin_element ',' bin_elements : ['$1'|'$3'].

bin_element -> bit_expr opt_bit_size_expr opt_bit_type_list :
	{bin_element,?line('$1'),'$1','$2','$3'}.

bit_expr -> prefix_op expr_max : ?mkop1('$1', '$2').
bit_expr -> expr_max : '$1'.

opt_bit_size_expr -> ':' bit_size_expr : '$2'.
opt_bit_size_expr -> '$empty' : default.

opt_bit_type_list -> '/' bit_type_list : '$2'.
opt_bit_type_list -> '$empty' : default.

bit_type_list -> bit_type '-' bit_type_list : ['$1' | '$3'].
bit_type_list -> bit_type : ['$1'].

bit_type -> atom             : element(3,'$1').
bit_type -> atom ':' integer : { element(3,'$1'), element(3,'$3') }.

bit_size_expr -> expr_max : '$1'.


guard_expr -> basic_type : '$1'.
guard_expr -> guard_expr_list : '$1'.
guard_expr -> guard_expr_tuple : '$1'.
guard_expr -> guard_call : '$1'.
guard_expr -> '(' guard_expr ')' : '$2'.
guard_expr -> guard_expr add_op guard_expr :
   {Op, Pos} = '$2',
   {arith, Pos, Op, '$1', '$3'}.
guard_expr -> guard_expr mult_op guard_expr :
   {Op, Pos} = '$2',
   {arith, Pos, Op, '$1', '$3'}.
guard_expr -> prefix_op guard_expr:
   case '$2' of
       {float, Pos, N} ->
	   case '$1' of
	       {'-', _} ->
		   {float, Pos, -N};
	       {'+', _} ->
		   {float, Pos, N};
	       {Op, Pos1} ->
		   {arith, Pos1, Op, {float, Pos, N}}
	   end;
       {integer, Pos, N} ->
	   case '$1' of
	       {'-', _} ->
		   {integer, Pos, -N};
	       {'+', _} ->
		   {integer, Pos, N};
	       {Op, Pos1} ->
		   {arith, Pos1, Op, {integer, Pos, N}}
	   end;
       _ ->
	   {Op, Pos} = '$1',
	   {arith, Pos, Op, '$2'}
   end.

guard_expr_list -> '[' ']' : {nil, ?line('$1')}.
guard_expr_list -> '[' guard_expr guard_expr_tail ']' :
   {cons, ?line('$1'), '$2', '$3'}.

guard_expr_tail -> '|' guard_expr : '$2'.
guard_expr_tail -> ',' guard_expr guard_expr_tail :
   case '$3' of
       {nil,0} -> {cons, ?line('$2'), '$2', {nil, ?line('$2')}};
       _       -> {cons, ?line('$2'), '$2', '$3'}
   end.
guard_expr_tail -> '$empty' : {nil,0}.

guard_expr_tuple -> '{' '}' : {tuple, element(2, '$1'), []}.
guard_expr_tuple -> '{' guard_exprs '}' : {tuple, element(2, '$1'), '$2'}.

guard_exprs -> guard_expr : ['$1'].
guard_exprs -> guard_expr ',' guard_exprs : ['$1' | '$3'].


guard_call -> atom '(' guard_parameter_list ')' :
   case erl_parse:erlang_guard_bif(element(3, '$1'), length('$3')) of
       true ->
	   {bif, element(2, '$1'), element(3, '$1'), '$3'};
       false ->
	   throw({error, {element(2, '$1'), yecc, "illegal test in guard **"}})
   end.

guard_parameter_list -> guard_exprs : '$1'.
guard_parameter_list -> '$empty' : [].


bif_test -> atom '(' guard_parameter_list ')' :
   case erl_parse:erlang_guard_test(element(3, '$1'), length('$3')) of
       true ->
	   {test, element(2, '$1'), element(3, '$1'), '$3'};
       false ->
	   throw({error, {element(2, '$1'), yecc, "illegal test in guard **"}})
   end.


guard_test -> bif_test : '$1'.
guard_test -> guard_expr comp_op guard_expr :
   {Op, Pos} = '$2',
   {comp, Pos, Op, '$1', '$3'}.

guard_tests -> guard_test : ['$1'].
guard_tests -> guard_test ',' guard_tests : ['$1' | '$3'].

% guard -> 'true' : [].
guard -> atom :
   case '$1' of
       {atom, _, true} ->
           [];
       _ ->
	   throw({error, {element(2, '$1'), yecc, "illegal test in guard **"}})
   end.
guard -> guard_tests : '$1'.



%%atomic -> char : '$1'.
atomic -> integer : '$1'.
atomic -> float : '$1'.
atomic -> atom : '$1'.
atomic -> strings : '$1'.

strings -> string : '$1'.
strings -> string strings :
	{string,?line('$1'),element(3, '$1') ++ element(3, '$2')}.


prefix_op -> '+' : '$1'.
prefix_op -> '-' : '$1'.
prefix_op -> 'bnot' : '$1'.
prefix_op -> 'not' : '$1'.

mult_op -> '/' : '$1'.
mult_op -> '*' : '$1'.
mult_op -> 'div' : '$1'.
mult_op -> 'rem' : '$1'.
mult_op -> 'band' : '$1'.
mult_op -> 'and' : '$1'.

add_op -> '+' : '$1'.
add_op -> '-' : '$1'.
add_op -> 'bor' : '$1'.
add_op -> 'bxor' : '$1'.
add_op -> 'bsl' : '$1'.
add_op -> 'bsr' : '$1'.
add_op -> 'or' : '$1'.
add_op -> 'xor' : '$1'.

list_op -> '++' : '$1'.
list_op -> '--' : '$1'.

comp_op -> '==' : '$1'.
comp_op -> '/=' : '$1'.
comp_op -> '=<' : '$1'.
comp_op -> '<' : '$1'.
comp_op -> '>=' : '$1'.
comp_op -> '>' : '$1'.
comp_op -> '=:=' : '$1'.
comp_op -> '=/=' : '$1'.


Erlang code.

%% mkop(Op, Arg) -> {op,Line,Op,Arg}.
%% mkop(Left, Op, Right) -> {op,Line,Op,Left,Right}.

-define(mkop2(L, OpPos, R),
        begin
            {Op,Pos} = OpPos,
            {op,Pos,Op,L,R}
        end).

-define(mkop1(OpPos, A),
        begin
            {Op,Pos} = OpPos,
            {op,Pos,Op,A}
        end).

%% keep track of line info in tokens
-define(line(Tup), element(2, Tup)).

%% build_fun(Line, [Clause]) -> {'fun',Line,{clauses,[Clause]}}.
build_fun([H|_] = Cs) ->
    Arity = length(element(4, hd(Cs))),
    {'fun',?line(H),{clauses,check_clauses(Cs, 'fun', Arity)}}.

check_clauses(Cs, Name, Arity) ->
     mapl(fun ({clause,L,N,As,G,B}) when N =:= Name, length(As) =:= Arity ->
		 {clause,L,As,G,B};
	     ({clause,L,_N,_As,_G,_B}) ->
		 ret_err(L, "head mismatch") end, Cs).

%% mapl(F,List)
%% an alternative map which always maps from left to right
%% and makes it possible to interrupt the mapping with throw on
%% the first occurence from left as expected.
%% can be removed when the jam machine (and all other machines)
%% uses the standardized (Erlang 5.0) evaluation order (from left to right)
mapl(F, [H|T]) ->
	V = F(H),
	[V | mapl(F,T)];
mapl(_, []) ->
	[].

-spec ret_err(_, _) -> no_return().
ret_err(L, S) ->
    {location,Location} = get_attribute(L, location),
    return_error(Location, S).

%%% [Experimental]. The parser just copies the attributes of the
%%% scanner tokens to the abstract format. This design decision has
%%% been hidden to some extent: use set_line() and get_attribute() to
%%% access the second element of (almost all) of the abstract format
%%% tuples. A typical use is to negate line numbers to prevent the
%%% compiler from emitting warnings and errors. The second element can
%%% (of course) be set to any value, but then these functions no
%%% longer apply. To get all present attributes as a property list
%%% get_attributes() should be used.

set_line(L, F) ->
    erl_scan:set_attribute(line, L, F).

get_attribute(L, Name) ->
    erl_scan:attributes_info(L, Name).

get_attributes(L) ->
    erl_scan:attributes_info(L).
