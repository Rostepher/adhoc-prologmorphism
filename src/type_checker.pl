:- module(type_checker, [type_check/1]).

lookup(X, [[X, Type] | Tail], Type) :- !.
lookup(X, [[Y, T] | Tail], Type) :-
    lookup(X, Tail, Type).

extend(Gamma, X, T, [[X, T] | Gamma]).

judge_type(Gamma, N, int) :- number(N).

judge_type(Gamma, X, T) :- atom(X), lookup(X, Gamma, T).

judge_type(Gamma, if(M, N1, N2), T) :-
    judge_type(Gamma, M, bool),
    judge_type(Gamma, N1, T),
    judge_type(Gamma, N2, T).

judge_type(Gamma, let(X, M, N), T) :-
    judge_type(Gamma, M, T1),
    extend(Gamma, X, T1, Gamma2),
    judge_type(Gamma2, N, T).

judge_type(Gamma, define(Y, X, TX, TM, M, N), T) :-
    extend(Gamma, X, TX, Gamma2),
    extend(Gamma2, Y, arrow(TX, TM), Gamma3),
    judge_type(Gamma3, M, TM),
    extend(Gamma, Y, arrow(TX, TM), Gamma4),
    judge_type(Gamma4, N, T).

judge_type(Gamma, const(X, M), T) :-
    judge_type(Gamma, M, T).

judge_type(Gamma, lambda(X, T, M), arrow(T, TM)) :-
    extend(Gamma, X, T, Gamma2),
    judge_type(Gamma2, M, TM).

judge_type(Gamma, call(M, N), T) :-
    judge_type(Gamma, N, TN),
    judge_type(Gamma, M, arrow(TN, T)).

init_gamma([
    [eq, arrow(int, arrow(int, bool))],
    [prod, arrow(int, arrow(int, int))],
    [sum, arrow(int, arrow(int, int))],
    [diff, arrow(int, arrow(int, int))]
]).

type_check(Prog) :-
    init_gamma(Gamma),
    judge_type(Gamma, Prog, _).
