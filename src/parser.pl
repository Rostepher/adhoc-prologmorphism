:- module(parser, [parse/2]).

:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, codes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Grammar

parse(Tokens, Ast) :-
    phrase(program(ParseTree), Tokens),
    parse_tree_ast(ParseTree, Ast).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Programs

program([F|Fs]) -->
    form(F),
    program(Fs).
program([]) --> [].

form(Node) --> (definition(Node) ; expression(Node)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definitions

definition(define(Name, Expr)) -->
    [lparen],
    [define],
    [ident(Name)],
    expression(Expr),
    [rparen].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expressions

expression(Const) --> constant(Const).

expression(variable(Name)) --> [ident(Name)].

expression(if(Cond, Then, Else)) -->
    [lparen],
    [if],
    expression(Cond),
    expression(Then),
    expression(Else),
    [rparen].

expression(lambda(Vars, Expr)) -->
    [lparen],
    [lambda],
    formals(Vars),
    expression(Expr),
    [rparen].

expression(Apply) --> application(Apply).

expressions([E|Es]) -->
    expression(E),
    expressions(Es).
expressions([]) --> [].

variable(variable(Name, Type)) -->
    [ident(Name)],
    [colon],
    type(Type).

variables([V|Vs]) -->
    variable(V),
    variables(Vs).
variables([V]) --> variable(V).

type_literal(bool)  --> [bool].
type_literal(float) --> [float].
type_literal(int)   --> [int].

type(T)             --> type_literal(T).
type(arrow(T1, T2)) --> [lparen], type_literal(T1), [arrow], type(T2), [rparen].

constant(true)  --> [true].
constant(false) --> [false].
constant(Int)   --> [int(Int)].
constant(Float) --> [float(Float)].

formals(Vs)  --> [lparen], variables(Vs), [rparen].

application(apply(Expr, Exprs)) -->
    [lparen],
    expression(Expr),
    expressions(Exprs),
    [rparen].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Datum

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parse Tree -> AST

parse_tree_ast(ParseTree, Ast) :- ParseTree = Ast.
