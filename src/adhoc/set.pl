:- module(set, [
    insert/3,
    remove/3,
    intersection/3,
    subtract/3,
    union/3
]).


member_eq(X, [Y | _])  :- X == Y, !.
member_eq(X, [_ | Ys]) :- member_eq(X, Ys).


insert(E, Set, Set2) :-
    union([E], Set, Set2).


remove(E, Set, Set2) :-
    subtract(Set, [E], Set2).


intersection([], _, []).
intersection([X | Xs], L, I) :-
    member_eq(X, L),
    !,
    I = [X | I2],
    intersection(Xs, L, I2).
intersection([_ | Xs], L, I) :-
    intersection(Xs, L, I).


subtract([], _, []).
subtract([X | Xs], L, S) :-
    member_eq(X, L),
    !,
    subtract(Xs, L, S).
subtract([X | Xs], L, [X | S]) :-
    subtract(Xs, L, S).


union([], L, L).
union([X | Xs], L, U) :-
    member_eq(X, L),
    !,
    union(Xs, L, U).
union([X | Xs], L, [X | U]) :-
    union(Xs, L, U).
