:- module(quantifier, [quantify_types/3]).

:- use_module(set).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% quantify_types/3 traverses the AST and quantifies all types, transforming
% them into type schemes.

quantify_types(_, [], []).
quantify_types(TVs, [Prog | Rest], [Prog2 | Rest2]) :-
    quantify_types(TVs, Prog, Prog2),
    quantify_types(TVs, Rest, Rest2).

% defvar
quantify_types(TVs, defvar(var(Var), Exp), defvar(var(Var), Exp2)) :-
    quantify_types(TVs, Exp, Exp2).

% defun
quantify_types(TVs, defun(var(Fun), var(Arg), ArgT, Exp, ExpT),
        defun(var(Fun), var(Arg), ArgT2, Exp2, ExpT2)) :-
    type_to_scheme(TVs,  ArgT, ArgT2, TVs2),
    type_to_scheme(TVs2, ExpT, ExpT2, _),
    quantify_types(TVs2, Exp, Exp2).

% if
quantify_types(TVs, if(Cond, Then, Else), if(Cond2, Then2, Else2)) :-
    quantify_types(TVs, Cond, Cond2),
    quantify_types(TVs, Then, Then2),
    quantify_types(TVs, Else, Else2).

% lambda
quantify_types(TVs, lambda(var(Var), VarT, Body),
        lambda(var(Var), VarT2, Body2)) :-
    type_to_scheme(TVs, VarT, VarT2, TVs2),
    quantify_types(TVs2, Body, Body2).

% let
quantify_types(TVs, let(var(Var), Exp, Body),
        let(var(Var), Exp2, Body2)) :-
    quantify_types(TVs, Exp,  Exp2),
    quantify_types(TVs, Body, Body2).

% apply
quantify_types(TVs, apply(Exp, Arg), apply(Exp2, Arg2)) :-
    quantify_types(TVs, Exp, Exp2),
    quantify_types(TVs, Arg, Arg2).

% var
quantify_types(_, var(Var), var(Var)).

% constants
quantify_types(_, true,     true).
quantify_types(_, false,    false).
quantify_types(_, float(F), float(F)).
quantify_types(_, int(I),   int(I)).

% lists
quantify_types(_, nil, nil).
quantify_types(TVs, cons(Head, Tail), cons(Head2, Tail2)) :-
    quantify_types(TVs, Head, Head2),
    quantify_types(TVs, Tail, Tail2).

% error
quantify_types(TVs, Exp, _) :-
    throw(quantify_type_error(TVs, Exp)).


% TODO: skolemize type schemes

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type_to_scheme/3 transforms a type to a type scheme by introducing
% universal quantifiers for free type variables.

type_to_scheme(TVs, bool,  forall([], bool),  TVs).
type_to_scheme(TVs, float, forall([], float), TVs).
type_to_scheme(TVs, int,   forall([], int),   TVs).

type_to_scheme(TVs, list(Type), forall(TVs2, list(Type2)), TVs) :-
    type_to_scheme(TVs, Type, forall(TVs2, Type2), _).

type_to_scheme(TVs, arrow(Type1, Type2), forall(UTVs, arrow(T1, T2)), TVs4) :-
    type_to_scheme(TVs,  Type1, forall(T1_Vs, T1), TVs2),
    type_to_scheme(TVs2, Type2, forall(T2_Vs, T2), TVs3),
    set:union(T1_Vs, T2_Vs, UTVs),
    set:union(TVs2, TVs3, TVs4).

type_to_scheme(TVs, T, forall([], Var), TVs) :-
    atom(T),
    lookup(T, TVs, Var).
type_to_scheme(TVs, T, forall([Var], Var), TVs2) :-
    atom(T),
    var(Var),
    extend(TVs, T, Var, TVs2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lookup/3 finds the variable assigned to a type name in a type variable
% envrionment.

lookup(T, [[T, Var] | _], Var) :- !.
lookup(T, [_ | Rest], Var) :- lookup(T, Rest, Var).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% extend/4 inserts into a type variable envrionment the type and associated
% variable.

extend(TVs, T, Var, [[T, Var] | TVs]).
