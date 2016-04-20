:- module(type_checker, [init_gamma/1, type_check/3]).

:- use_module(quantifier).

% disable warnings for unused variables
:- style_check(-singleton).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% judge_type/4 type_checks a valid expression in the lanague with a given
% Gamma and produces an output type and possibly extended Gamma.

% defvar
judge_type(Gamma, defvar(var(Var), Exp), T, Gamma2) :-
    judge_type(Gamma, Exp, T, _),
    simple_extend(Gamma, Var, T, Gamma2).

% defun
judge_type(Gamma, defun(var(Fun), var(Var), VarT, Exp, ExpT), arrow(VarT, ExpT), Gamma4) :-
    simple_extend(Gamma, Var, VarT, Gamma2),
    simple_extend(Gamma2, Fun, arrow(VarT, ExpT), Gamma3),
    judge_type(Gamma3, Exp, ExpT, _),
    extend(Gamma, Fun, arrow(VarT, ExpT), Gamma4).

% if
judge_type(Gamma, if(Cond, Then, Else), T, Gamma) :-
    judge_type(Gamma, Cond, bool, _),
    judge_type(Gamma, Then, T, _),
    judge_type(Gamma, Else, T, _).

% lambda
judge_type(Gamma, lambda(var(Var), VarT, Body), arrow(VarT2, BodyT), Gamma) :-
    simple_extend(Gamma, Var, VarT, Gamma2),
    type_from_scheme(VarT, VarT2),
    judge_type(Gamma2, Body, BodyT, _).

% let
judge_type(Gamma, let(var(Var), Exp, Body), BodyT, Gamma) :-
    judge_type(Gamma, Exp, ExpT, _),
    extend(Gamma, Var, ExpT, Gamma2),
    judge_type(Gamma2, Body, BodyT, _).

% apply
judge_type(Gamma, apply(Exp, Arg), T, Gamma) :-
    judge_type(Gamma, Arg, ArgT, _),
    judge_type(Gamma, Exp, arrow(ArgT, T), _).

% nil
judge_type(Gamma, nil, list(T), Gamma).

% lists
judge_type(Gamma, cons(Head, Tail), list(T), Gamma3) :-
    judge_type(Gamma, Head, T, Gamma2),
    judge_type(Gamma2, Tail, list(T), Gamma3).

% vars
judge_type(Gamma, var(X), T, Gamma) :- atom(X), lookup(X, Gamma, T).

% literals
judge_type(Gamma, true,     bool,  Gamma).
judge_type(Gamma, false,    bool,  Gamma).
judge_type(Gamma, float(F), float, Gamma) :- float(F).
judge_type(Gamma, int(I),   int,   Gamma) :- integer(I).

% error
judge_type(Gamma, Exp, _, _) :-
    throw(type_error(Exp, Gamma)).


% convienience predicates
type_check(Gamma, [Prog], Gamma2) :-
    quantify_types([], Prog, Prog2),
    judge_type(Gamma, Prog2, _, Gamma2).

type_check(Gamma, [Prog | Rest], Gamma3) :-
    judge_type(Gamma, Prog, _, Gamma2),
    type_check(Gamma2, Rest, Gamma3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type environment

% simple_extend/4
simple_extend(Gamma, Var, T, [[Var, ST] | Gamma]) :-
    skolemize(T, ST).


% extend/4
extend(Gamma, Var, T, [[Var, forall(STVs2, ST)] | Gamma]) :-
    skolemize(T, forall(STVs, ST)),
    free_env_vars(Gamma, GVs),
    free_type_vars(ST, FreeTVs),
    subtract(FreeTVs, GVs, STVs2).


% lookup/3
lookup(X, [[X, Scheme] | _], Type) :-
    !, type_from_scheme(Scheme, Type).
lookup(X, [_ | Tail], Type) :-
    lookup(X, Tail, Type).


% type_from_scheme/2
type_from_scheme(forall(TVs, T), T2) :-
    skolemize(forall(TVs, T), forall(STVs, ST)),
    fresh_vars(STVs, FreshTVs),
    replace(ST, STVs, FreshTVs, T2).


% fresh_vars
fresh_vars([], []).
fresh_vars([X | Xs], [Y | Ys]) :- fresh_vars(Xs, Ys).


% replace/4
replace(T, Vars, Fresh, Type) :-
    var(T), !,
    replace_var(T, Vars, Fresh, Type).
replace(bool,  _, _, bool).
replace(float, _, _, float).
replace(int,   _, _, int).
replace(list(T), Vars, Fresh, list(Type)) :-
    replace(T, Vars, Fresh, Type).
replace(arrow(T1, T2), Vars, Fresh, arrow(Type1, Type2)) :-
    replace(T1, Vars, Fresh, Type1),
    replace(T2, Vars, Fresh, Type2).


% replace_var/4
replace_var(X, [],       [],       X).
replace_var(X, [Z | Zs], [Y | Ys], Y) :- X == Z, !.
replace_var(X, [Z | Zs], [Y | Ys], W) :- replace_var(X, Zs, Ys, W).


% free vars/2
free_env_vars([], []).
free_env_vars([[X, S] | G], U) :-
    free_scheme_vars(S, SVs),
    free_env_vars(G, GVs),
    union(SVs, GVs, U).


% free_type_vars/2
free_type_vars(T, [T]) :- var(T), !.
free_type_vars(bool, []).
free_type_vars(float, []).
free_type_vars(int, []).

free_type_vars(list(T), TVs) :-
    free_type_vars(T, TVs).

free_type_vars(arrow(T1, T2), TVs) :-
    free_type_vars(T1, T1Vs),
    free_type_vars(T2, T2Vs),
    union(T1Vs, T2Vs, TVs).


% free_scheme_vars/2
free_scheme_vars(forall(Vs, T), SVs) :-
    free_type_vars(T, TVs),
    subtract(TVs, Vs, SVs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% skolemize/2 skolemizes a type scheme by moving all universal quantifiers to
% the front of the type.

skolemize(T,       forall([], T)) :- var(T), !.
skolemize(bool,    forall([], bool)).
skolemize(float,   forall([], float)).
skolemize(int,     forall([], int)).
skolemize(list(T), forall([], list(T))).

skolemize(arrow(T1, T2), forall(TVs, arrow(T1_2, T2_2))) :-
    skolemize(T1, forall(T1Vs, T1_2)),
    skolemize(T2, forall(T2Vs, T2_2)),
    union(T1Vs, T2Vs, TVs).

skolemize(forall(TVs, T), forall(TVs3, T2)) :-
    skolemize(T, forall(TVs2, T2)),
    union(TVs, TVs2, TVs3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% initial environment

init_gamma([
    % bool
    ['not', forall([], arrow(bool, bool))],

    % int
    ['=', forall([], arrow(int, arrow(int, bool)))],
    ['+', forall([], arrow(int, arrow(int, int)))],
    ['-', forall([], arrow(int, arrow(int, int)))],
    ['*', forall([], arrow(int, arrow(int, int)))],
    ['/', forall([], arrow(int, arrow(int, int)))],

    % float
    ['=.', forall([], arrow(float, arrow(float, bool)))],
    ['+.', forall([], arrow(float, arrow(float, float)))],
    ['-.', forall([], arrow(float, arrow(float, float)))],
    ['*.', forall([], arrow(float, arrow(float, float)))],
    ['/.', forall([], arrow(float, arrow(float, float)))],

    % bool
    ['not', forall([], arrow(bool, bool))],

    % list
    ['nil?', forall([Tn], arrow(list(Tn), bool))],
    ['cons', forall([Tc], arrow(Tc, arrow(list(Tc), list(Tc))))],
    ['head', forall([Th], arrow(list(Th), Th))],
    ['tail', forall([Tt], arrow(list(Tt), list(Tt)))]
]).
