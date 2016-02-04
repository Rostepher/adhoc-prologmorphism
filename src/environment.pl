:- module(environment, [lookup/3, extend/4]).

lookup(X, [[X, Exp] | _], Exp) :- !.
lookup(X, [[Y, _] | Tail], Exp) :-
    lookup(X, Tail, Exp).

extend(Env, Name, Exp, [[Name, Exp] | Env]).
