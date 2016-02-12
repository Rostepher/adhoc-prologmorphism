:- module(environment, [extend/4, lookup/3, init_env/1]).

% extend/4 extends a given environment with a key/value pair.
extend(Env, Key, Val, [[Key, Val] | Env]).

% lookup/3 searches for the value of a given key in an environment.
lookup(Key, [[Key, Val] | _], Val) :- !.
lookup(Key, [_ | Tail], Val) :-
    lookup(Key, Tail, Val).

lookup(Key, env(Rho, Gamma), Val) :-
    lookup(Key, Rho, Val);
    lookup(Key, Gamma, Val).

init_global_env([
    [eq,   prim(eq)],
    [sum,  prim(sum)],
    [prod, prim(prod)],
    [diff, prim(diff)]
]).

% init_env/1 provides the initial local and global environments.
init_env(env([], Gamma)) :-
    init_global_env(Gamma).
