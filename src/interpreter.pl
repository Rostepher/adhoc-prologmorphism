:- module(interpreter, [evaluate/4, init_env/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% environment

% extend/4 extends a given environment with a key/value pair.
extend(Env, Key, Val, [[Key, Val] | Env]).

% lookup/3 searches for the value of a given key in an environment.
lookup(Key, [[Key, Val] | _], Val) :- !.
lookup(Key, [_ | Tail], Val) :-
    lookup(Key, Tail, Val).

lookup(Key, env(Rho, Gamma), Val) :-
    append(Rho, Gamma, Env),
    lookup(Key, Env, Val).

init_global_env([
    [eq,   prim(eq)],
    [sum,  prim(sum)],
    [prod, prim(prod)],
    [diff, prim(diff)],

    [is_nil, prim(is_nil)],
    [cons,   prim(cons)],
    [head,   prim(head)],
    [tail,   prim(tail)]
]).

% init_env/1 provides the initial local and global environments.
init_env(env([], Gamma)) :-
    init_global_env(Gamma).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% delta

% delta/3 evaluates primitives by parially applying values to a primitive atom
% or by executing the primitive operation once it has been partially applied.

delta(eq(X), X, true).
delta(eq(_), _, false).

delta(sum(X),  Y, Val) :- Val is X + Y.
delta(prod(X), Y, Val) :- Val is X * Y.
delta(diff(X), Y, Val) :- Val is X / Y.

delta(is_nil, nil(_), true).
delta(is_nil, _,      false).

delta(cons(M), N, cons(M, N)).

delta(head, nil(T), nil(T)).
delta(head, cons(M, _), M).

delta(tail, nil(T), nil(T)).
delta(tail, cons(_, N), N).

delta(Prim, Arg, prim(Val)) :- Val =.. [Prim, Arg].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% evaluate

% evaluate_if/6
evaluate_if(true, M2, _, env(Rho, Global), Val, Global2) :-
    evaluate(M2, env(Rho, Global), Val, Global2).

evaluate_if(false, _, M3, env(Rho, Global), Val, Global2) :-
    evaluate(M3, env(Rho, Global), Val, Global2).

% evaluate_call/5 is a helper fuction for evaluating a call expression
% where the evaluation of M is a closure or primitive function.

% closure
evaluate_call(closure(X, M_C, Rho_C), N, env(Rho, Global), Val2, Global3) :-
    evaluate(N, env(Rho, Global), Val, Global2),
    extend(Rho_C, X, Val, Rho2),
    evaluate(M_C, env(Rho2, Global2), Val2, Global3).

% prim
evaluate_call(prim(F), N, env(Rho, Global), Val2, Global2) :-
    evaluate(N, env(Rho, Global), Val, Global2),
    delta(F, Val, Val2).

% evaluate/4 evaluates a valid expression in the language with a given
% local and global envrionment and produces an ouput value and possibly
% extended global environment.
%
% Rho    - local environment
% Global - global environment

% if
evaluate(if(M1, M2, M3), env(Rho, Global), Val, Global3) :-
    evaluate(M1, env(Rho, Global), Cond, Global2),
    evaluate_if(Cond, M2, M3, env(Rho, Global2), Val, Global3).

% let
evaluate(let(X, M1, M2), env(Rho, Global), Val2, Global3) :-
    evaluate(M1, env(Rho, Global), Val, Global2),
    extend(Rho, X, Val, Rho2),
    evaluate(M2, env(Rho2, Global2), Val2, Global3).

% lambda
evaluate(lambda(X, _, M), env(Rho, Global), closure(X, M, Rho), Global).

% define
evaluate(define(Y, X, _, M, _), env(Rho, Global), _, Global2) :-
    extend(Global, Y, closure(X, M, Rho), Global2).

% const
evaluate(const(X, M), env(Rho, Global), _, Global3) :-
    evaluate(M, env(Rho, Global), Val, Global2),
    extend(Global2, X, Val, Global3).

% call
evaluate(call(M, N), env(Rho, Global), Val2, Global3) :-
    evaluate(M, env(Rho, Global), M2, Global2),
    evaluate_call(M2, N, env(Rho, Global2), Val2, Global3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% list builtins

% nil
evaluate(nil(T), env(_, Global), nil(T), Global).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% literals
%
% the number, variable and error cases encur extra computations, they have
% been moved to the bottom for the sake of optimization.

% bool
evaluate(true,  env(_, Global), true,  Global).
evaluate(false, env(_, Global), false, Global).

% int
evaluate(N, env(_, Global), N, Global) :- integer(N).

% float
evaluate(N, env(_, Global), N, Global) :- float(N).

% variable
evaluate(Var, env(Rho, Global), Val, Global) :-
    lookup(Var, env(Rho, Global), Val).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% error

evaluate(Exp, env(Rho, Global), _, _) :-
    throw(runtime_error(Exp, env(Rho, Global))).

