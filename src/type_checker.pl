:- module(type_checker, [init_gamma/1, type_check/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% judge_type/4 type_checks a valid expression in the lanague with a given
% Gamma and produces an output type and possibly extended Gamma.

% const
% judge_type(Gamma1, const(X, M), T, Gamma2) :-
%     judge_type(Gamma1, M, T, _),
%     extend(Gamma1, X, T, Gamma2).

% define
% judge_type(Gamma1, define(Y, X, T_X, M, T_M), arrow(T_X, T_M), Gamma4) :-
%     extend(Gamma1, X, T_X, Gamma2),
%     extend(Gamma2, Y, arrow(T_X, T_M), Gamma3),
%     judge_type(Gamma3, M, T_M, Gamma4).

% define (constant)
judge_type(Gamma1, define(var(X), M), T, Gamma2) :-
    judge_type(Gamma1, M, T, _),
    extend(Gamma1, X, T, Gamma2).

% define (function)
judge_type(Gamma1, define(var(X), T, M), T, Gamma4) :-
    simple_extend(Gamma1, X, T, Gamma2),
    judge_type(Gamma2, M, T, _),
    extend(Gamma1, X, T, Gamma4).

% if
judge_type(Gamma, if(M, N1, N2), T, Gamma) :-
    judge_type(Gamma, M, bool, _),
    judge_type(Gamma, N1, T, _),
    judge_type(Gamma, N2, T, _).

% lambda
judge_type(Gamma1, lambda(var(X), T_X, M), arrow(T_X, T_M), Gamma1) :-
    simple_extend(Gamma1, X, T_X, Gamma2),
    judge_type(Gamma2, M, T_M, _).

% let
judge_type(Gamma1, let(var(X), M, N), T, Gamma1) :-
    judge_type(Gamma1, M, T1, _),
    extend(Gamma1, X, T1, Gamma2),
    judge_type(Gamma2, N, T, _).

% apply
judge_type(Gamma, apply(M, N), T, Gamma) :-
    judge_type(Gamma, N, T_N, _),
    judge_type(Gamma, M, arrow(T_N, T), _).

% nil
judge_type(Gamma, nil, list(T), Gamma).

% lists
judge_type(Gamma, cons(Head, Tail), list(T), Gamma3) :-
    judge_type(Gamma, Head, T, Gamma2),
    judge_type(Gamma2, Tail, list(T), Gamma3).

% literals
judge_type(Gamma, true,  bool, Gamma).
judge_type(Gamma, false, bool, Gamma).

% int
judge_type(Gamma, int(I), int, Gamma) :- integer(I).

% float
judge_type(Gamma, float(F), float, Gamma) :- float(F).

% variable
judge_type(Gamma, var(X), T, Gamma) :- atom(X), lookup(X, Gamma, T).

% error
judge_type(Gamma, Exp, _, _) :-
    throw(type_error(Exp, Gamma)).


% convienience predicates
type_check(Gamma1, [Prog], Gamma2) :-
    judge_type(Gamma1, Prog, _, Gamma2).
type_check(Gamma1, [Prog | Rest], Gamma3) :-
    judge_type(Gamma1, Prog, _, Gamma2),
    type_check(Gamma2, Rest, Gamma3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type environment

% extend
simple_extend(Gamma, X, T, [[X, forall([], T)] | Gamma]).

extend(Gamma, X, T, [[X, forall(Vs, T)] | Gamma]) :-
    free_env_vars(Gamma, GVs),
    free_type_vars(T, TVs),
    subtract(GVs, TVs, Vs).


% lookup
lookup(X, [[X, Scheme] | _], Type) :-
    !, type_from_scheme(Scheme, Type).
lookup(X, [_ | Tail], Type) :-
    lookup(X, Tail, Type).

type_from_scheme(forall(Vs, T), Type) :-
    fresh_vars(Vs, Fresh),
    replace(T, Vs, Fresh, Type).

fresh_vars([], []).
fresh_vars([X | Xs], [Y | Ys]) :- fresh_vars(Xs, Ys).

replace(T, Vars, Fresh, Type) :-
    var(T), !,
    replace_var(T, Vars, Fresh, Type).
replace(int,  _, _, int).
replace(bool, _, _, bool).
replace(list(T), Vars, Fresh, list(Type)) :-
    replace(T, Vars, Fresh, Type).
replace(arrow(T1, T2), Vars, Fresh, arrow(Type1, Type2)) :-
    replace(T1, Vars, Fresh, Type1),
    replace(T2, Vars, Fresh, Type2).

replace_var(X, [], [], X).
replace_var(X, [Z | Zs], [Y | Ys], Y) :- X == Z, !.
replace_var(X, [Z | Zs], [Y | Ys], W) :- replace_var(X, Zs, Ys, W).


% free vars
free_env_vars([], []).
free_env_vars([[X, S] | G], U) :-
    free_scheme_vars(S, SVs),
    free_env_vars(G, GVs),
    union(SVs, GVs, U).

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

free_scheme_vars(forall(Vs, T), SVs) :-
    free_type_vars(T, TVs),
    subtract(TVs, Vs, SVs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
