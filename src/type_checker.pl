:- module(type_checker, [type_check/2, type_check/1]).

lookup(X, [[X, Type] | _], Type) :- !.
lookup(X, [_ | Tail], Type) :-
    lookup(X, Tail, Type).

extend(Gamma, X, T, [[X, T] | Gamma]).

% number
judge_type(_, N, int) :- number(N).

% variable
judge_type(Gamma, X, T) :- atom(X), lookup(X, Gamma, T).

% if
judge_type(Gamma, if(M, N1, N2), T) :-
    judge_type(Gamma, M, bool),
    judge_type(Gamma, N1, T),
    judge_type(Gamma, N2, T).

% let
judge_type(Gamma, let(X, M, N), T) :-
    judge_type(Gamma, M, T1),
    extend(Gamma, X, T1, Gamma2),
    judge_type(Gamma2, N, T).

% lambda
judge_type(Gamma, lambda(X, T_X, M), arrow(T_X, T_M)) :-
    extend(Gamma, X, T_X, Gamma2),
    judge_type(Gamma2, M, T_M).

% define
judge_type(Gamma, define(Y, X, T_X, M, T_M), arrow(T_X, T_M)) :-
    extend(Gamma, X, T_X, Gamma2),
    extend(Gamma2, Y, arrow(T_X, T_M), Gamma3),
    judge_type(Gamma3, M, T_M).

% const
judge_type(Gamma, const(_, M), T) :-
    judge_type(Gamma, M, T).

% call
judge_type(Gamma, call(M, N), T) :-
    judge_type(Gamma, N, T_N),
    judge_type(Gamma, M, arrow(T_N, T)).

% error
judge_type(_, Exp, _) :-
    throw(type_error(Exp)).

init_gamma([
    [eq, arrow(int, arrow(int, bool))],
    [prod, arrow(int, arrow(int, int))],
    [sum, arrow(int, arrow(int, int))],
    [diff, arrow(int, arrow(int, int))]
]).

type_check(Prog, T) :-
    init_gamma(Gamma),
    judge_type(Gamma, Prog, T).

type_check(Prog) :- type_check(Prog, _).
