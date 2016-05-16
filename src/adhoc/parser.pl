:- module(parser, [parse/2, parse_tree_ast/2]).

:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, codes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dgc helper predicates

:- meta_predicate brackets(//, ?, *).
brackets(Goal) --> [open_bracket], Goal, [close_bracket].
brackets(Goal) --> [open_bracket], brackets(Goal), [close_bracket].

:- meta_predicate parens(//, ?, *).
parens(Goal) --> [open_paren], Goal, [close_paren].
parens(Goal) --> [open_paren], parens(Goal), [close_paren].

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
    parens((
        [defvar, ident(Var)],
        expression(Exp)
    )).

% function
definition(defun(var(Fun), Schemes, Args, Exp, ExpT)) -->
    parens((
        [defun, ident(Fun)],
        optional_parens((
            [colon],
            optional_type_schemes(Schemes),
            variables(Args),
            [arrow],
            type(ExpT)
        )),
        expression(Exp)
    )).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% overloading

overload(over(var(Op))) --> parens([over, ident(Op)]).

instance(inst(var(Op), OpSchemes, OpT, Exp)) -->
    parens((
        [inst, ident(Op)],
        parens((
            [colon],
            optional_type_schemes(OpSchemes),
            type(OpT)
        )),
        expression(Exp)
    )).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expessions

% constants
expression(Const) --> constant(Const).

% var
expression(var(Var)) --> [ident(Var)].

% if
expression(if(Cond, Then, Else)) -->
    parens((
        [if],
        expression(Cond),
        expression(Then),
        expression(Else)
    )).

% lambda
expression(lambda(Vars, Body)) -->
    parens((
        [lambda],
        parens(formals(Vars)),
        expression(Body)
    )).

% let
expression(let(Vals, Body)) -->
    parens((
        [let],
        bindings(Vals),
        expression(Body)
    )).

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

variables([Var]) --> parens(variable(Var)).
variables([Var | Vars]) -->
    parens(variable(Var)),
    variables(Vars).

formals([Var]) --> variable(Var).
formals([Var | Vars]) -->
    parens(variable(Var)),
    formals(Vars).

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
    parens((
        expression(Exp),
        expressions(Args),
        { \+ Args = [] }
    )).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% types

type(arrow(T1, T2)) --> type_literal(T1), arrow_type(T2).
type(arrow(T1, T2)) --> parens(type(T1)), arrow_type(T2).
type(T)             --> type_literal(T).
type(T)             --> parens(type(T)).

type_literal(T)       --> [ident(T)].
type_literal(list(T)) --> brackets([ident(T)]).

arrow_type(T) --> [arrow], optional_parens(type(T)).

constraint(constraint(var(Op), OpT)) -->
    [ident(Op), colon],
    type(OpT).

constraints([Con | Cons]) -->
    parens(constraint(Con)),
    constraints(Cons).
constraints([Con]) -->
    optional_parens(constraint(Con)).

type_scheme(scheme(Alpha, PiAlpha)) -->
    parens((
        [forall, ident(Alpha)],
        optional_parens(constraints(PiAlpha))
    )).
type_scheme(scheme(Alpha, [])) -->
    parens([forall, ident(Alpha)]).

type_schemes([Scheme | Schemes]) -->
    type_scheme(Scheme),
    type_schemes(Schemes).
type_schemes([Scheme]) --> type_scheme(Scheme).

optional_type_schemes(Schemes) -->
    type_schemes(Schemes),
    [fat_arrow].
optional_type_schemes([]) --> [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% constants

constant(true)         --> [true].
constant(false)        --> [false].
constant(int(Int))     --> [int(Int)].
constant(float(Float)) --> [float(Float)].
constant(List)         --> list(List).

list(cons(Head, Tail)) -->
    brackets((
        expression(Head),
        list_tail(Tail)
    )).
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
transform(defun(var(Fun), Schemes, [[var(Arg), ArgT]], Exp, ExpT),
        defun(var(Fun), var(Arg), forall(Schemes, ArgT), Exp2, ExpT)) :-
    transform(Exp, Exp2).
transform(defun(var(Fun), Schemes, [[var(Arg), ArgT] | Args], Exp, ExpT),
        defun(var(Fun), var(Arg), forall(Schemes, ArgT), Lambda, LambdaT)) :-
    defun_args_type(Args, ExpT, LambdaT),
    transform_lambda(Args, Exp, Lambda).

% overload
transform(over(var(Op)), over(var(Op))).

% instance
transform(inst(var(Op), Schemes, OpT, Exp),
        inst(var(Op), forall(Schemes, OpT), Exp2)) :-
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
transform(nil, var('nil')).
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

