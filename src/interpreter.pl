:- module(interpreter, [evaluate/4]).

:- use_module(environment).

:- reexport(environment, [init_env/1]).

% delta/3 evaluates primitives by parially applying values to a primitive atom
% or by executing the primitive operation once it has been partially applied.

delta(eq(X), X, true).
delta(eq(_), _, false).

delta(sum(X),  Y, Val) :- Val is X + Y.
delta(prod(X), Y, Val) :- Val is X * Y.
delta(diff(X), Y, Val) :- Val is X / Y.

delta(Prim, Arg, prim(Val)) :- Val =.. [Prim, Arg].

% evaluate/4 evaluates a valid expression in the language with a given
% local and global envrionment and produces an ouput value and possibly
% extended global environment.
%
% Rho    - local environment
% Global - global environment

% if
evaluate(if(M1, M2, _), env(Rho, Global1), Val, Global3) :-
    evaluate(M1, env(Rho, Global1), true, Global2),
    evaluate(M2, env(Rho, Global2), Val, Global3).

evaluate(if(M1, _, M3), env(Rho, Global1), Val, Global3) :-
    evaluate(M1, env(Rho, Global1), false, Global2),
    evaluate(M3, env(Rho, Global2), Val, Global3).

% let
evaluate(let(X, M1, M2), env(Rho1, Global1), Val2, Global3) :-
    evaluate(M1, env(Rho1, Global1), Val1, Global2),
    extend(Rho1, X, Val1, Rho2),
    evaluate(M2, env(Rho2, Global2), Val2, Global3).

% lambda
evaluate(lambda(X, _, M), env(Rho, Global), closure(X, M, Rho), Global).

% define
evaluate(define(Y, X, _, M, _), env(Rho, Global1), _, Global2) :-
    extend(Global1, Y, closure(X, M, Rho), Global2).

% const
evaluate(const(X, M), env(Rho, Global1), _, Global3) :-
    evaluate(M, env(Rho, Global1), Val, Global2),
    extend(Global2, X, Val, Global3).

% call
evaluate(call(M, N), env(Rho1, Global1), Val2, Global3) :-
    evaluate(M, env(Rho1, Global1), prim(F), Global2),
    evaluate(N, env(Rho1, Global2), Val1, Global3),
    delta(F, Val1, Val2).

evaluate(call(M, N), env(Rho1, Global1), Val2, Global4) :-
    evaluate(M, env(Rho1, Global1), closure(X, M_C, Rho2), Global2),
    evaluate(N, env(Rho2, Global2), Val1, Global3),
    extend(Rho2, X, Val1, Rho3),
    evaluate(M_C, env(Rho3, Global3), Val2, Global4).

% literals
evaluate(true,  env(_, Global), true,  Global).
evaluate(false, env(_, Global), false, Global).

% the number, variable and error cases encur extra computations, they have
% been moved to the bottom for the sake of optimization.

% number
evaluate(N, env(_, Global), N, Global) :-
    integer(N);
    float(N).

% variable
evaluate(Var, env(Rho, Global), Val, Global) :-
    lookup(Var, env(Rho, Global), Val).

% error
evaluate(Exp, env(Rho, Global), _, _) :-
    throw(runtime_error(Exp, env(Rho, Global))).
