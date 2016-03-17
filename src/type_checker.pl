:- module(type_checker, [init_gamma/1, type_check/3]).

lookup(X, [[X, Type] | _], Type) :- !.
lookup(X, [_ | Tail], Type) :-
    lookup(X, Tail, Type).

extend(Gamma, X, T, [[X, T] | Gamma]).

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
judge_type(Gamma1, define(var(X), T, M), T, Gamma3) :-
    extend(Gamma1, X, T, Gamma2),
    judge_type(Gamma2, M, T, Gamma3).

% if
judge_type(Gamma, if(M, N1, N2), T, Gamma) :-
    judge_type(Gamma, M, bool, _),
    judge_type(Gamma, N1, T, _),
    judge_type(Gamma, N2, T, _).

% lambda
judge_type(Gamma1, lambda(var(X), T_X, M), arrow(T_X, T_M), Gamma1) :-
    extend(Gamma1, X, T_X, Gamma2),
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
judge_type(Gamma, nil, list(_), Gamma).

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

init_gamma([
    % int
    ['=', arrow(int, arrow(int, bool))],
    ['+', arrow(int, arrow(int, int))],
    ['-', arrow(int, arrow(int, int))],
    ['*', arrow(int, arrow(int, int))],
    ['/', arrow(int, arrow(int, int))],

    % float
    ['=.', arrow(float, arrow(float, bool))],
    ['+.', arrow(float, arrow(float, float))],
    ['-.', arrow(float, arrow(float, float))],
    ['*.', arrow(float, arrow(float, float))],
    ['/.', arrow(float, arrow(float, float))],

    % bool
    ['not', arrow(bool, bool)],

    % list
    % ['nil?', generic([a], arrow(list(a), a))],
    % ['cons', generic([a], arrow(a, arrow(list(a), list(a))))],
    % ['head', generic([a], arrow(list(a), a))],
    % ['tail', generic([a], arrow(list(a), list(a)))]
    ['nil?', arrow(list(T), T)],
    ['cons', arrow(T, arrow(list(T), list(T)))],
    ['head', arrow(list(T), T)],
    ['tail', arrow(list(T), list(T))]
]).

type_check(Gamma1, [Prog], Gamma2) :-
    judge_type(Gamma1, Prog, _, Gamma2).
type_check(Gamma1, [Prog | Rest], Gamma3) :-
    judge_type(Gamma1, Prog, _, Gamma2),
    type_check(Gamma2, Rest, Gamma3).
