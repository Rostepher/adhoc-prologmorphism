:- module(parser, [parse/2, parse_tree_ast/2]).

:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, codes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Grammar

% parse(Tokens, Ast) :-
%     phrase(program(ParseTree), Tokens),
%     parse_tree_ast(ParseTree, Ast).

parse(Tokens, ParseTree) :- phrase(program(ParseTree), Tokens).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Programs

program([F|Fs]) -->
    form(F),
    program(Fs).
program([]) --> [].

form(Node) --> (definition(Node) ; expression(Node)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definitions

definition(define(var(Name), Expr)) -->
    [lparen],
    [define],
    [ident(Name)],
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
expression(lambda(Vars, Expr)) -->
    [lparen, lambda],
    formals(Vars),
    expression(Expr),
    [rparen].

% apply
expression(Apply) --> application(Apply).

% precedence
expression(Exprs) -->
    [lparen],
    expressions(Exprs),
    [rparen].

expressions([])     --> [].
expressions([E|Es]) --> expression(E), expressions(Es).

variable(var(Name, Type)) -->
    [ident(Name), colon],
    type(Type).

variables([V|Vs]) -->
    variable(V),
    variables(Vs).
variables([V]) --> variable(V).

type(T)             --> [lparen], type(T), [rparen].
type(T)             --> [type(T)].
type(arrow(T1, T2)) --> [type(T1), arrow], type(T2).

formals(Vs) --> [lparen], variables(Vs), [rparen].

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

% define
transform(define(Var, Expr), define(Var2, Expr2)) :-
    transform(Var, Var2),
    transform(Expr, Expr2).

% if
transform(if(Cond, Then, Else), if(Cond2, Then2, Else2)) :-
    transform(Cond, Cond2),
    transform(Then, Then2),
    transform(Else, Else2).

% lambda
transform(lambda(Vars, Exprs), lambda(Vars2, Exprs2)) :-
    transform(Vars, Vars2),
    transform(Exprs, Exprs2).

% apply
transform(apply(Expr, Args), Apply) :-
    transform(Expr, Expr2),
    transform(Args, Args2),
    transform_apply(Expr2, Args2, Apply).

%constants
transform(true, true).
transform(false, false).

transform(int(I), int(I)).
transform(float(F), float(F)).

transform(var(Name), var(Name)).
transform(var(Name, Type), var(Name, Type)).

% lists
transform(nil, nil).
transform(cons(Head, Tail), cons(Head2, Tail2)) :-
    transform(Head, Head2),
    transform(Tail, Tail2).

% error
transform(Expr, _) :- throw(transform_error(Expr)).

transform_apply(Expr, [Arg], apply(Expr, Arg)).
transform_apply(Expr, [H|T], apply(Apply, H)) :-
    transform_apply(Expr, T, Apply).

