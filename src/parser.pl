:- module(parser, [parse/2, parse_tree_ast/2]).

:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, codes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parse/2

parse(Tokens, Ast) :-
    phrase(program(ParseTree), Tokens),
    !,
    parse_tree_ast(ParseTree, Ast).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Programs

program([Form|Forms]) -->
    form(Form),
    !,
    program(Forms).
program([]) --> [].

form(Node) --> (definition(Node) ; expression(Node)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definitions

% constant
definition(define(var(Name), Expr)) -->
    [lparen, define],
    [ident(Name)],
    expression(Expr),
    [rparen].

% explicit type annotation
definition(define(var(Name), Type, Expr)) -->
    [lparen, define],
    [ident(Name)],
    [colon],
    type(Type),
    expression(Expr),
    [rparen].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expressions

% constants
expression(Const) --> constant(Const).

% var
expression(var(Name)) --> [ident(Name)].

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
expression(Exprs) -->
    [lparen],
    expressions(Exprs),
    [rparen].

expressions([E])    --> expression(E).
expressions([E|Es]) --> expression(E), expressions(Es).

variable(var(Name, Type)) -->
    [ident(Name), colon],
    type(Type).

variables([V]) --> [lparen], variable(V), [rparen].
variables([V|Vs]) -->
    [lparen],
    variable(V),
    [rparen],
    variables(Vs).

type(T)             --> [lparen], type(T), [rparen].
type(T)             --> [ident(T)].
type(list(T))       --> [lbracket, ident(T), rbracket].
type(arrow(T1, T2)) --> [ident(T1), arrow], type(T2).

formals([Var]) --> [lparen], variable(Var), [rparen].
formals(Vars)  --> [lparen], variables(Vars), [rparen].

value(val(Name, Expr)) -->
    [ident(Name)],
    expression(Expr).

values([V]) --> [lparen], value(V), [rparen].
values([V|Vs]) -->
    [lparen],
    value(V),
    [rparen],
    values(Vs).

bindings([Val]) --> [lparen], value(Val), [rparen].
bindings(Vals)  --> [lparen], values(Vals), [rparen].

application(apply(Expr, Args)) -->
    [lparen],
    expression(Expr),
    expressions(Args),
    { \+ Args = [] },
    [rparen].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Constants

constant(Bool)         --> bool(Bool).
constant(int(Int))     --> [int(Int)].
constant(float(Float)) --> [float(Float)].
constant(List)         --> list(List).

bool(true)  --> [true].
bool(false) --> [false].

list(cons(Head, Tail)) --> [lbracket], [Head], list_tail(Tail), [rbracket].
list(nil)              --> [lbracket, rbracket].

list_tail(cons(Head, Tail)) --> [Head], list_tail(Tail).
list_tail(nil)              --> [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parse Tree -> AST

parse_tree_ast(ParseTree, Ast) :-
    transform(ParseTree, Ast).

transform([], []).
transform([Head|Tail], [NewHead|NewTail]) :-
    transform(Head, NewHead),
    transform(Tail, NewTail).

% define (constant)
transform(define(Var, Expr), define(Var2, Expr2)) :-
    transform(Var, Var2),
    transform(Expr, Expr2).

% define (function)
transform(define(Var, Type, Expr), define(Var2, Type, Expr2)) :-
    transform(Var, Var2),
    transform(Expr, Expr2).

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
transform(apply(Expr, Args), Apply) :-
    transform_apply(Expr, Args, Apply).

% constants
transform(true, true).
transform(false, false).

transform(int(I), int(I)).
transform(float(F), float(F)).

transform(var(Name), var(Name)).
transform(var(Name, Type), var(Name, Type)).

transform(val(Name, Expr), val(Name, Expr)).

% lists
transform(nil, nil).
transform(cons(Head, Tail), cons(Head2, Tail2)) :-
    transform(Head, Head2),
    transform(Tail, Tail2).

% error
transform(Expr, _) :- throw(transform_error(Expr)).

transform_lambda([var(Name, Type)], Body, lambda(var(Name), Type, Body2)) :-
    transform(Body, Body2).
transform_lambda([var(Name, Type) | Rest], Body, lambda(var(Name), Type, Lambda)) :-
    transform_lambda(Rest, Body, Lambda).

transform_let([val(Name, Expr)], Body, let(var(Name), Expr2, Body2)) :-
    transform(Expr, Expr2),
    transform(Body, Body2).
transform_let([val(Name, Expr) | Rest], Body, let(var(Name), Expr2, Let)) :-
    transform(Expr, Expr2),
    transform_let(Rest, Body, Let).

transform_apply(Expr, [Arg], apply(Expr2, Arg2)) :-
    transform(Expr, Expr2),
    transform(Arg, Arg2).
transform_apply(Expr, [Arg | Rest], apply(Apply, Arg2)) :-
    transform(Arg, Arg2),
    transform_apply(Expr, Rest, Apply).

