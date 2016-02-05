:- module(interpreter, [evaluate/2]).

:- use_module(environment).

:- reexport(environment, [init_env/1]).

% evaluate/4 evaluates a valid expression in the language with a given
% local and global envrionment and produces an ouput value and possibly
% extended global environment
%
% Rho   - local environment
% Gamma - global environment

% if
evaluate(if(M1, M2, _), env(Rho, Gamma1), Val, Gamma3) :-
    evaluate(M1, env(Rho, Gamma1), true, Gamma2),
    evaluate(M2, env(Rho, Gamma2), Val, Gamma3).

evaluate(if(M1, _, M3), env(Rho, Gamma1), Val, Gamma3) :-
    evaluate(M1, env(Rho, Gamma1), false, Gamma2),
    evaluate(M3, env(Rho, Gamma2), Val, Gamma3).

% let
evaluate(let(X, M1, M2), env(Rho1, Gamma1), Val2, Gamma3) :-
    evaluate(M1, env(Rho1, Gamma1), Val1, Gamma2),
    extend(Rho1, X, Val1, Rho2),
    evaluate(M2, env(Rho2, Gamma2), Val2, Gamma3).

% lambda
evaluate(lambda(X, _, M), env(Rho, Gamma), closure(X, M, Rho), Gamma).

% define
evaluate(define(Y, X, _, M, _), env(Rho, Gamma1), _, Gamma2) :-
    extend(Gamma1, Y, closure(X, M, Rho), Gamma2).

% const
evaluate(const(X, M), env(_, Gamma1), _, Gamma2) :-
    extend(Gamma1, X, M, Gamma2).

% call
evaluate(call(M, N), env(Rho1, Gamma1), Val2, Gamma4) :-
    evaluate(M, env(Rho1, Gamma1), closure(X, M_C, Rho2), Gamma2),
    evaluate(N, env(Rho2, Gamma2), Val1, Gamma3),
    extend(Rho2, X, Val1, Rho3),
    evaluate(M_C, env(Rho3, Gamma3), Val2, Gamma4).

% primitives
evaluate(primitive_eq(X, Y), env(Rho, Gamma), true, Gamma) :-
    lookup(X, env(Rho, Gamma), Z),
    lookup(Y, env(Rho, Gamma), Z).
evaluate(primitive_eq(_, _), env(_, Gamma), false, Gamma).

evaluate(primitive_sum(X1, Y1), env(Rho, Gamma), Val, Gamma) :-
    lookup(X1, env(Rho, Gamma), X2),
    lookup(Y1, env(Rho, Gamma), Y2),
    Val is X2 + Y2.

evaluate(primitive_prod(X1, Y1), env(Rho, Gamma), Val, Gamma) :-
    lookup(X1, env(Rho, Gamma), X2),
    lookup(Y1, env(Rho, Gamma), Y2),
    Val is X2 * Y2.

evaluate(primitive_diff(X1, Y1), env(Rho, Gamma), Val, Gamma) :-
    lookup(X1, env(Rho, Gamma), X2),
    lookup(Y1, env(Rho, Gamma), Y2),
    Val is X2 / Y2.

% the number, variable and error cases encur extra computations, they have
% been moved to the bottom for the sake of optimization.

% number
evaluate(N, env(_, Gamma), N, Gamma) :-
    number(N).

% variable
evaluate(Var, env(Rho, Gamma), Val, Gamma) :-
    lookup(Var, env(Rho, Gamma), Val).

% error
evaluate(Exp, env(Rho, Gamma), _, _) :-
    throw(runtime_error(Exp, env(Rho, Gamma))).

% evaluate/2 is a convenience predicate to evaluate an expression with the
% initial environment.
evaluate(Exp, Val) :-
    init_env(Env),
    evaluate(Exp, Env, Val, _).
