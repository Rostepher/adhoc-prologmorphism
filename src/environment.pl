:- module(environment, [extend/4, lookup/3, init_env/1]).

extend(Env, Name, Exp, [[Name, Exp] | Env]).

lookup(X, [[X, Exp] | _], Exp) :- !.
lookup(X, [_ | Tail], Exp) :-
    lookup(X, Tail, Exp).

% init_env/1 provides the initial local and global environments.
init_env(env([], Gamma)) :-
    % TODO: add stdlib functions, somehow...
    Gamma = [].
