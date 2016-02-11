:- module(type_checker, [init_gamma/1, type_check/3]).

lookup(X, [[X, Type] | _], Type) :- !.
lookup(X, [_ | Tail], Type) :-
    lookup(X, Tail, Type).

extend(Gamma, X, T, [[X, T] | Gamma]).

% judge_type/4 type_checks a valid expression in the lanague with a given
% Gamma and produces an output type and possibly extended Gamma.

% if
judge_type(Gamma, if(M, N1, N2), T, _) :-
    judge_type(Gamma, M, bool, _),
    judge_type(Gamma, N1, T, _),
    judge_type(Gamma, N2, T, _).

% let
judge_type(Gamma1, let(X, M, N), T, _) :-
    judge_type(Gamma1, M, T1, _),
    extend(Gamma1, X, T1, Gamma2),
    judge_type(Gamma2, N, T, _).

% lambda
judge_type(Gamma1, lambda(X, T_X, M), arrow(T_X, T_M), _) :-
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
judge_type(Gamma, call(M, N), T, _) :-
    judge_type(Gamma, N, T_N, _),
    judge_type(Gamma, M, arrow(T_N, T), _).

% literals
judge_type(_, true,  bool, _).
judge_type(_, false, bool, _).

% int
judge_type(_, N, int, _) :- integer(N).

% float
judge_type(_, N, float, _) :- float(N).

% variable
judge_type(Gamma, X, T, _) :- atom(X), lookup(X, Gamma, T).

% error
judge_type(_, Exp, _, _) :-
    throw(type_error(Exp)).

init_gamma([
    [eq, arrow(int, arrow(int, bool))],
    [prod, arrow(int, arrow(int, int))],
    [sum, arrow(int, arrow(int, int))],
    [diff, arrow(int, arrow(int, int))]
]).

type_check(Gamma1, Prog, Gamma2) :- judge_type(Gamma1, Prog, _, Gamma2).
