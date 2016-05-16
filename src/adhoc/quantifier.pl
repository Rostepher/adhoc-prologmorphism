:- module(quantifier, [quantify_ast/3]).

:- use_module(set).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% quantify_ast/3 traverses the AST and quantifies all types, transforming
% them into type schemes.

% defvar
quantify_ast(TVs, defvar(var(Var), Exp), Defvar) :-
    quantify_ast(TVs, Exp, Exp2),
    Defvar = defvar(var(Var), Exp2).

% defun
quantify_ast(TVs, defun(var(Fun), var(Arg), ArgT, Exp, ExpT), Defun) :-
    quantify_type(TVs,  ArgT, ArgT2, TVs2),
    quantify_type(TVs2, ExpT, ExpT2, _),
    quantify_ast(TVs2, Exp, Exp2),
    Defun = defun(var(Fun), var(Arg), ArgT2, Exp2, ExpT2).

% over
quantify_ast(_, over(var(Op)), Over) :-
    Over = over(var(Op)).

% inst
quantify_ast(TVs, inst(var(Op), OpT, Exp), Inst) :-
    quantify_type(TVs, OpT, OpT2, TVs2),
    quantify_ast(TVs2, Exp, Exp2),
    Inst = inst(var(Op), OpT2, Exp2).

% if
quantify_ast(TVs, if(Cond, Then, Else), If) :-
    quantify_ast(TVs, Cond, Cond2),
    quantify_ast(TVs, Then, Then2),
    quantify_ast(TVs, Else, Else2),
    If = if(Cond2, Then2, Else2).

% lambda
quantify_ast(TVs, lambda(var(Var), VarT, Body), Lambda) :-
    type_to_scheme(TVs, VarT, VarT2, TVs2),
    remove_empty_scheme(VarT2, VarT3),
    quantify_ast(TVs2, Body, Body2),
    Lambda = lambda(var(Var), VarT3, Body2).

% let
quantify_ast(TVs, let(var(Var), Exp, Body), Let) :-
    quantify_ast(TVs, Exp,  Exp2),
    quantify_ast(TVs, Body, Body2),
    Let = let(var(Var), Exp2, Body2).

% apply
quantify_ast(TVs, apply(Exp, Arg), Apply) :-
    quantify_ast(TVs, Exp, Exp2),
    quantify_ast(TVs, Arg, Arg2),
    Apply = apply(Exp2, Arg2).

% var
quantify_ast(_, var(Var), var(Var)).

% constants
quantify_ast(_, true,     true).
quantify_ast(_, false,    false).
quantify_ast(_, float(F), float(F)).
quantify_ast(_, int(I),   int(I)).

% lists
% TODO: fix nil type, make it polymorphic
quantify_ast(_, nil, nil).
quantify_ast(TVs, cons(Head, Tail), cons(Head2, Tail2)) :-
    quantify_ast(TVs, Head, Head2),
    quantify_ast(TVs, Tail, Tail2).

% error
quantify_ast(TVs, Exp, _) :-
    throw(quantifier_error(TVs, Exp)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% quantify_type_schemes/4

quantify_type_schemes(TVs, [], [], TVs).
quantify_type_schemes(TVs, [scheme(Alpha, Constraints) | Schemes],
        [scheme(Alpha2, Constraints2) | Schemes2], TVs2) :-
    lookup(TVs, Alpha, Alpha2),
    quantify_type_constraints(TVs, Constraints, Constraints2),
    quantify_type_schemes(TVs, Schemes, Schemes2, TVs2).

quantify_type_schemes(TVs, [scheme(Alpha, Constraints) | Schemes],
        [scheme(Alpha2, Constraints2) | Schemes2], TVs3) :-
    extend(TVs, Alpha, Alpha2, TVs2),
    quantify_type_constraints(TVs2, Constraints, Constraints2),
    quantify_type_schemes(TVs2, Schemes, Schemes2, TVs3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% quantify_type_constraints/3

quantify_type_constraints(_, [], []).
quantify_type_constraints(TVs, [constraint(Op, OpT) | Cons],
        [constraint(Op, OpT3) | Cons2]) :-
    type_to_scheme(TVs, OpT, OpT2, TVs2),
    remove_empty_scheme(OpT2, OpT3),
    quantify_type_constraints(TVs2, Cons, Cons2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type_vars_from_scheme/2

type_vars_from_scheme([], []).
type_vars_from_scheme([scheme(Var, _) | Schemes], [Var | Vars]) :-
    type_vars_from_scheme(Schemes, Vars).


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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% quantify_type/4

quantify_type(TVs, forall(Schemes, T), T3, TVs4) :-
    quantify_type_schemes(TVs, Schemes, Schemes2, TVs2),
    type_vars_from_scheme(Schemes2, SchemeTVs),
    set:union(TVs2, SchemeTVs, TVs3),
    type_to_scheme(TVs3, T, T2, TVs4),
    remove_empty_scheme(T2, T3).

quantify_type(TVs, T, T3, TVs2) :-
    type_to_scheme(TVs, T, T2, TVs2),
    remove_empty_scheme(T2, T3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lookup/3 finds the variable assigned to a type name in a type variable
% envrionment.

lookup(T, [[T, Var] | _], Var) :- !.
lookup(T, [_ | Rest], Var) :- lookup(T, Rest, Var).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% extend/4 inserts into a type variable envrionment the type and associated
% variable.

extend(TVs, T, Var, [[T, Var] | TVs]).
