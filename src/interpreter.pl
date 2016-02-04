:- module(interpreter, [evaluate/4]).

:- use_module(environment).

evaluate(N, Env, N, Env) :- number(N).

evaluate(Var, env(Rho, Gamma), Value, env(Rho, Gamma)) :-
    lookup(Var, Gamma, Value).

% evaluate(if(M1, M2, M3), env(Rho, Gamma), Value, env(Rho, Gamma3)) :-

evaluate(Exp, env(Rho, Gamma), Value, env(Rho2, Gamma2)) :- fail.
