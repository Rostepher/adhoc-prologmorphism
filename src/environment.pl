:- module(environment, [extend/4, lookup/3, init_env/1]).

extend(Env, Key, Val, [[Key, Val] | Env]).

lookup(Key, [[Key, Val] | _], Val) :- !.
lookup(Key, [_ | Tail], Val) :-
    lookup(Key, Tail, Val).

lookup(Key, env(Rho, Gamma), Val) :-
    lookup(Key, Rho, Val);
    lookup(Key, Gamma, Val).

% TODO: add stdlib functions, somehow...
init_global_env([
    [eq,   closure(x, lambda(y, int, primitive_eq(x, y)),   [])],
    [sum,  closure(x, lambda(y, int, primitive_sum(x, y)),  [])],
    [prod, closure(x, lambda(y, int, primitive_prod(x, y)), [])],
    [diff, closure(x, lambda(y, int, primitive_diff(x, y)), [])]
]).

% init_env/1 provides the initial local and global environments.
init_env(env([], Gamma)) :-
    init_global_env(Gamma).
