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
quantify_types(TVs, defun(var(Fun), Schemes, var(Arg), ArgT, Exp, ExpT),
        defun(var(Fun), FunT, var(Arg), ArgT3, Exp2, ExpT3)) :-
    create_fun_type(TVs, Schemes, arrow(ArgT, ExpT), FunT, TVs2),

    type_to_scheme(TVs2, ArgT, ArgT2, TVs3),
    type_to_scheme(TVs3, ExpT, ExpT2, _),
    remove_empty_scheme(ArgT2, ArgT3),
    remove_empty_scheme(ExpT2, ExpT3),
    quantify_types(TVs3, Exp, Exp2).

% over
quantify_types(_, over(var(Op)), over(var(Op))).

% inst
quantify_types(TVs, inst(var(Op), forall(Schemes, OpT), Exp),
        inst(var(Op), forall(Schemes2, OpT3), Exp2)) :-
    quantify_type_schemes(TVs, Schemes, Schemes2),
    type_to_scheme(TVs, OpT, OpT2, TVs2),
    remove_empty_scheme(OpT2, OpT3),
    quantify_types(TVs2, Exp, Exp2).

% if
quantify_types(TVs, if(Cond, Then, Else), if(Cond2, Then2, Else2)) :-
    quantify_types(TVs, Cond, Cond2),
    quantify_types(TVs, Then, Then2),
    quantify_types(TVs, Else, Else2).

% lambda
quantify_types(TVs, lambda(var(Var), VarT, Body),
        lambda(var(Var), VarT3, Body2)) :-
    type_to_scheme(TVs, VarT, VarT2, TVs2),
    remove_empty_scheme(VarT2, VarT3),
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


create_fun_type(TVs, Schemes, FunT, forall(FunSchemes2, FunT2), TVs2) :-
    quantify_type_schemes(TVs, Schemes, Schemes2, TVs2),
    type_to_scheme(TVs2, FunT, forall(FunSchemes, FunT2), _),
    set:union(Schemes2, FunSchemes, FunSchemes2).


% union_schemes([], S2, S2).
% union_schemes([scheme(Alpha, Cons) | Schemes],


quantify_type_schemes(_, [], []).
quantify_type_schemes(TVs, [scheme(Alpha, PiAlpha) | Schemes],
        [scheme(Alpha2, PiAlpha2) | Schemes2], TVs) :-
    lookup(TVs, Alpha, Alpha2),
    quantify_type_constraints(TVs, PiAlpha, PiAlpha2),
    quantify_type_schemes(TVs, Schemes, Schemes2).
quantify_type_schemes(TVs, [scheme(Alpha, PiAlpha) | Schemes],
        [scheme(Alpha2, PiAlpha2) | Schemes2], TVs2) :-
    extend(TVs, Alpha, Alpha2, TVs2),
    quantify_type_constraints(TVs2, PiAlpha, PiAlpha2),
    quantify_type_schemes(TVs2, Schemes, Schemes2).


quantify_type_constraints(_, [], []).
quantify_type_constraints(TVs, [constraint(Op, OpT) | Cons],
        [constraint(Op, OpT3) | Cons2]) :-
    type_to_scheme(TVs, OpT, OpT2, TVs2),
    remove_empty_scheme(OpT2, OpT3),
    quantify_type_constraints(TVs2, Cons, Cons2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type_to_scheme/3 transforms a type to a type scheme by introducing
% universal quantifiers for free type variables.

type_to_scheme(TVs, bool,  forall([], bool),  TVs).
type_to_scheme(TVs, float, forall([], float), TVs).
type_to_scheme(TVs, int,   forall([], int),   TVs).

type_to_scheme(TVs, list(Type), forall(Schemes, list(Type2)), TVs) :-
    type_to_scheme(TVs, Type, forall(Schemes, Type2), _).

type_to_scheme(TVs, arrow(Type1, Type2), forall(UTVs, arrow(T1, T2)), TVs4) :-
    type_to_scheme(TVs,  Type1, forall(T1_Vs, T1), TVs2),
    type_to_scheme(TVs2, Type2, forall(T2_Vs, T2), TVs3),
    set:union(T1_Vs, T2_Vs, UTVs),
    set:union(TVs2, TVs3, TVs4).

type_to_scheme(TVs, T, forall([], Var), TVs) :-
    atom(T),
    lookup(T, TVs, Var).
type_to_scheme(TVs, T, forall([scheme(Var, [])], Var), TVs2) :-
    atom(T),
    var(Var),
    extend(TVs, T, Var, TVs2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% remove_empty_scheme/2 removes the forall quantifier if there are no type
% variables.

remove_empty_scheme(forall([], T), T).
remove_empty_scheme(forall(TVs, T), forall(TVs, T)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lookup/3 finds the variable assigned to a type name in a type variable
% envrionment.

lookup(T, [[T, Var] | _], Var) :- !.
lookup(T, [_ | Rest], Var) :- lookup(T, Rest, Var).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% extend/4 inserts into a type variable envrionment the type and associated
% variable.

extend(TVs, T, Var, [[T, Var] | TVs]).
