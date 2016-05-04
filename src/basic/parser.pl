:- module(parser, [parse/2, parse_tree_ast/2]).

:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, codes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dgc helper predicates

:- meta_predicate brackets(//, ?, *).
brackets(Goal) --> [lbracket], Goal, [rbracket].
brackets(Goal) --> [lbracket], brackets(Goal), [rbracket].

:- meta_predicate parens(//, ?, *).
parens(Goal) --> [lparen], Goal, [rparen].
parens(Goal) --> [lparen], parens(Goal), [rparen].

:- meta_predicate optional_parens(//, ?, *).
optional_parens(Goal) --> Goal.
optional_parens(Goal) --> parens(Goal).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parse/2

parse(Tokens, Ast) :-
    phrase(program(ParseTree), Tokens),
    !,
    parse_tree_ast(ParseTree, Ast).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% programs

program([Form | Forms]) -->
    form(Form),
    !,
    program(Forms).
program([]) --> [].

form(Def)  --> definition(Def).
form(Over) --> overload(Over).
form(Inst) --> instance(Inst).
form(Exp)  --> expression(Exp).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% definitions

% variable
definition(defvar(var(Var), Exp)) -->
    [lparen, defvar],
    [ident(Var)],
    expression(Exp),
    [rparen].

% function
definition(defun(var(Fun), Args, Exp, ExpT)) -->
    [lparen, defun],
    [lparen, ident(Fun)],
    formals(Args),
    [colon],
    type(ExpT),
    [rparen],
    expression(Exp),
    [rparen].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% overloading

overload(over(var(Op))) --> [lparen, over, ident(Op), rparen].

instance(inst(var(Op), Type, Exp)) -->
    [lparen, inst],
    parens(variable([var(Op), Type])),
    expression(Exp),
    [rparen].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expessions

% constants
expression(Const) --> constant(Const).

% var
expression(var(Var)) --> [ident(Var)].

% if
expression(if(Cond, Then, Else)) -->
    [lparen, if],
    expression(Cond),
    expression(Then),
    expression(Else),
    [rparen].

% lambda
expression(lambda(Vars, Body)) -->
    [lparen, lambda],
    formals(Vars),
    expression(Body),
    [rparen].

% let
expression(let(Vals, Body)) -->
    [lparen, let],
    bindings(Vals),
    expression(Body),
    [rparen].

% apply
expression(Apply) --> application(Apply).

% precedence
expression(Exp) --> parens(expression(Exp)).

expressions([Exp]) --> expression(Exp).
expressions([Exp | Exps]) -->
    expression(Exp),
    expressions(Exps).

variable([var(Var), T]) -->
    [ident(Var), colon],
    type(T).

type(T)             --> parens(type(T)).
type(T)             --> [ident(T)].
type(list(T))       --> brackets([ident(T)]).
type(arrow(T1, T2)) --> parens(type(T1)), [arrow], type(T2).
type(arrow(T1, T2)) --> [ident(T1), arrow], type(T2).

% constraint([Con]) --> [].

variables([Var]) --> parens(variable(Var)).
variables([Var | Vars]) -->
    parens(variable(Var)),
    variables(Vars).

formals([Var]) --> parens(variable(Var)).
formals(Vars)  --> parens(variables(Vars)).

value([var(Var), Exp]) -->
    [ident(Var)],
    expression(Exp).

values([Val]) --> parens(value(Val)).
values([Val | Vals]) -->
    parens(value(Val)),
    values(Vals).

bindings([Val]) --> parens(value(Val)).
bindings(Vals)  --> parens(values(Vals)).

application(apply(Exp, Args)) -->
    [lparen],
    expression(Exp),
    expressions(Args),
    { \+ Args = [] },
    [rparen].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% constants

constant(true)         --> [true].
constant(false)        --> [false].
constant(int(Int))     --> [int(Int)].
constant(float(Float)) --> [float(Float)].
constant(List)         --> list(List).

list(cons(Head, Tail)) -->
    [lbracket],
    expression(Head),
    list_tail(Tail),
    [rbracket].
list(nil) --> brackets([]).

list_tail(cons(Head, Tail)) -->
    expression(Head),
    list_tail(Tail).
list_tail(nil) --> [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parse_tree_ast/2

parse_tree_ast(ParseTree, Ast) :-
    transform(ParseTree, Ast).

transform([], []).
transform([Head | Tail], [Head2 | Tail2]) :-
    transform(Head, Head2),
    transform(Tail, Tail2).

% defvar (variable)
transform(defvar(var(Var), Exp), defvar(var(Var), Exp2)) :-
    transform(Exp, Exp2).

% defun (function)
transform(defun(var(Fun), [[var(Arg), ArgT]], Exp, ExpT),
        defun(var(Fun), var(Arg), ArgT, Exp2, ExpT)) :-
    transform(Exp, Exp2).
transform(defun(var(Fun), [[var(Arg), ArgT] | Args], Exp, ExpT),
        defun(var(Fun), var(Arg), ArgT, Lambda, RetT)) :-
    defun_args_type(Args, ExpT, RetT),
    transform_lambda(Args, Exp, Lambda).

% overload
transform(over(var(Op)), over(var(Op))).

% instance
transform(inst(var(Op), Type, Exp), inst(var(Op), Type, Exp2)) :-
    transform(Exp, Exp2).

% if
transform(if(Cond, Then, Else), if(Cond2, Then2, Else2)) :-
    transform(Cond, Cond2),
    transform(Then, Then2),
    transform(Else, Else2).

% lambda
transform(lambda(Vars, Body), Lambda) :-
    transform_lambda(Vars, Body, Lambda).

% let
transform(let(Vals, Body), Let) :-
    transform_let(Vals, Body, Let).

% apply
transform(apply(Exp, Args), Apply) :-
    reverse(Args, Args2),
    transform_apply(Exp, Args2, Apply).

% var
transform(var(Var), var(Var)).

% constants
transform(true, true).
transform(false, false).
transform(int(I), int(I)).
transform(float(F), float(F)).

% lists
transform(nil, nil).
transform(cons(Head, Tail), cons(Head2, Tail2)) :-
    transform(Head, Head2),
    transform(Tail, Tail2).

% error
transform(Exp, _) :- throw(transform_error(Exp)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% transform helper predicates

% defun_args_type/3
defun_args_type([[_, T]], RetT, arrow(T, RetT)).
defun_args_type([[_, T1] | Args], RetT, arrow(T1, T2)) :-
    defun_args_type(Args, RetT, T2).

% transform_lambda/3
transform_lambda([[var(Var), VarT]], Body, lambda(var(Var), VarT, Body2)) :-
    transform(Body, Body2).
transform_lambda([[var(Var), VarT] | Rest], Body, lambda(var(Var), VarT, Lambda)) :-
    transform_lambda(Rest, Body, Lambda).

% transform_let/3
transform_let([[var(Var), Exp]], Body, let(var(Var), Exp2, Body2)) :-
    transform(Exp, Exp2),
    transform(Body, Body2).
transform_let([[var(Var), Exp] | Rest], Body, let(var(Var), Exp2, Let)) :-
    transform(Exp, Exp2),
    transform_let(Rest, Body, Let).

% transform_apply/3
transform_apply(Exp, [Arg], apply(Exp2, Arg2)) :-
    transform(Exp, Exp2),
    transform(Arg, Arg2).
transform_apply(Exp, [Arg | Rest], apply(Apply, Arg2)) :-
    transform(Arg, Arg2),
    transform_apply(Exp, Rest, Apply).

