:- module(type_checker, [init_gamma/1, type_check/3]).

lookup(X, [[X, Type] | _], Type) :- !.
lookup(X, [_ | Tail], Type) :-
    lookup(X, Tail, Type).

extend(Gamma, X, T, [[X, T] | Gamma]).

% judge_type/4 type_checks a valid expression in the lanague with a given
% Gamma and produces an output type and possibly extended Gamma.

% if
judge_type(Gamma, if(M, N1, N2), T, Gamma) :-
    judge_type(Gamma, M, bool, _),
    judge_type(Gamma, N1, T, _),
    judge_type(Gamma, N2, T, _).

% let
judge_type(Gamma1, let(X, M, N), T, Gamma1) :-
    judge_type(Gamma1, M, T1, _),
    extend(Gamma1, X, T1, Gamma2),
    judge_type(Gamma2, N, T, _).

% lambda
judge_type(Gamma1, lambda(X, T_X, M), arrow(T_X, T_M), Gamma1) :-
    extend(Gamma1, X, T_X, Gamma2),
    judge_type(Gamma2, M, T_M, _).

% define
judge_type(Gamma1, define(Y, X, T_X, M, T_M), arrow(T_X, T_M), Gamma4) :-
    extend(Gamma1, X, T_X, Gamma2),
    extend(Gamma2, Y, arrow(T_X, T_M), Gamma3),
    judge_type(Gamma3, M, T_M, Gamma4).

% const
judge_type(Gamma1, const(X, M), T, Gamma2) :-
    judge_type(Gamma1, M, T, _),
    extend(Gamma1, X, T, Gamma2).

% call
judge_type(Gamma, call(M, N), T, Gamma) :-
    judge_type(Gamma, N, T_N, _),
    judge_type(Gamma, M, arrow(T_N, T), _).

% nil
judge_type(Gamma, nil(T), list(T), Gamma).

% cons
judge_type(Gamma, cons(M, N), list(T), Gamma) :-
    judge_type(Gamma, M, T, _),
    judge_type(Gamma, N, list(T), _).

% is_nil
judge_type(Gamma, is_nil(M), bool, Gamma) :-
    judge_type(Gamma, M, list(_), _).

% head
judge_type(Gamma, head(M), T, Gamma) :-
    judge_type(Gamma, M, list(T), _).

% tail
judge_type(Gamma, tail(M), list(T), Gamma) :-
    judge_type(Gamma, M, list(T), _).

% literals
judge_type(Gamma, true,  bool, Gamma).
judge_type(Gamma, false, bool, Gamma).

% int
judge_type(Gamma, N, int, Gamma) :- integer(N).

% float
judge_type(Gamma, N, float, Gamma) :- float(N).

% variable
judge_type(Gamma, X, T, Gamma) :- atom(X), lookup(X, Gamma, T).

% error
judge_type(Gamma, Exp, _, _) :-
    throw(type_error(Exp, Gamma)).

init_gamma([
    [eq, arrow(int, arrow(int, bool))],
    [prod, arrow(int, arrow(int, int))],
    [sum, arrow(int, arrow(int, int))],
    [diff, arrow(int, arrow(int, int))]
]).

type_check(Gamma1, Prog, Gamma2) :- judge_type(Gamma1, Prog, _, Gamma2).
