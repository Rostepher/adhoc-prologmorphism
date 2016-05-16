:- module(type_checker, [init_type_env/1, type_check/5]).

:- use_module(quantifier).
:- use_module(set).

% disable warnings for unused variables
:- style_check(-singleton).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% assert_free_type_vars checks that there are no free type variables by walking
% the AST while keeping an envrionment of previously seen type variables. As
% as side effect, this should catch mistakes with univeral quantification and
% overloaded constraints.

% TODO


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% judge_type/4 type_checks a valid expression in the lanague with a given
% Env and produces an output type and possibly extended Env.

% defvar
judge_type(Env, defvar(var(Var), Exp), Defvar, DefvarT, Env2) :-
    judge_type(Env, Exp, Exp2, T, _),
    extend_simple(Env, Var, T, Env2),
    Defvar = defvar(var(Var), Exp2).


% defun
judge_type(Env, defun(var(Fun), var(Var), VarT, Exp, ExpT), Defun, DefunT, Env4) :-
    extend_simple(Env, Var, VarT, Env2),
    extend_simple(Env2, Fun, arrow(VarT, ExpT), Env3),
    judge_type(Env3, Exp, Exp2, ExpT, _),
    create_arrow_type(VarT, ExpT, DefunT),
    extend(Env, Fun, DefunT, Env4),
    Defun = defun(var(Fun), var(Var), VarT, Exp2, ExpT).


% over
judge_type(Env, over(var(Op)), over(var(Op)), none, Env2) :-
    extend_over(Env, Op, Env2).


% inst
judge_type(Env, inst(var(Op), OpT, Exp), Impl2, ImplT, Env3) :-
    Inst = inst(var(Op), OpT, Exp),
    extend_inst(Env, Inst, Impl, Env2),
    judge_type(Env2, Impl, Impl2, ImplT, Env3).


% if
judge_type(Env, if(Cond, Then, Else), If, IfT, Env) :-
    judge_type(Env, Cond, Cond2, bool, _),
    judge_type(Env, Then, Then2, IfT, _),
    judge_type(Env, Else, Else2, IfT, _),
    If = if(Cond2, Then2, Else2).


% lambda
judge_type(Env, lambda(var(Var), VarT, Body), Lambda, LambdaT, Env) :-
    extend_simple(Env, Var, VarT, Env2),
    judge_type(Env2, Body, Body2, BodyT, _),
    LambdaT = arrow(VarT, BodyT),
    Lambda = lambda(var(Var), VarT, Body2).


% let
judge_type(Env, let(var(Var), Exp, Body), Let, LetT, Env) :-
    judge_type(Env, Exp, Exp2, ExpT, _),
    extend(Env, Var, ExpT, Env2),
    judge_type(Env2, Body, Body2, BodyT, _),
    LetT = BodyT,
    Let = let(var(Var), Exp2, Body2).


% apply
judge_type(Env, apply(Exp, Arg), Apply, ApplyT, Env) :-
    judge_type(Env, Arg, Arg2, ArgT, _),
    judge_type(Env, Exp, Exp2, arrow(ArgT, ExpT), _),
    ApplyT = ExpT,
    Apply = apply(Exp2, Arg2).


% nil
judge_type(Env, nil, nil, list(_T), Env).


% lists
judge_type(Env, cons(Head, Tail), Cons, list(T), Env3) :-
    judge_type(Env,  Head, Head2, T, Env2),
    judge_type(Env2, Tail, Tail2, list(T), Env3),
    Cons = cons(Head2, Tail2).


% vars
judge_type(Env, var(X), var(X), T, Env) :-
    atom(X),
    lookup(X, Env, T).


% literals
judge_type(Env, true,     true,     bool,  Env).
judge_type(Env, false,    false,    bool,  Env).
judge_type(Env, float(F), float(F), float, Env) :- float(F).
judge_type(Env, int(I),   int(I),   int,   Env) :- integer(I).


% error
judge_type(Env, Exp, _, _, _) :-
    throw(type_error(Exp, Env)).


% convienience predicates
type_check(Env, [], [], [], Env).
type_check(Env, [Prog | Rest], [NewProg | NewRest], [T | Ts], Env3) :-
    quantify_types([], Prog, Prog2),
    judge_type(Env, Prog2, NewProg, T, Env2),
    type_check(Env2, Rest, NewRest, Ts, Env3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type environment
%
% `env(Gamma, Overs, Insts)`
%
% A type environment consists of a few components
%   * Gamma - the typothesis
%   * Overs - mapping from overloaded operator to lists of instances
%   * Insts - mapping from instance operator and type to unique function name


% env_over_set/2
env_over_set(env(_, Overs, _), OverSet) :-
    env_over_set(Overs, OverSet).

env_over_set([], []).
env_over_set([over(Op, _) | Overs], [Op | Ops]) :-
    env_over_set(Overs, Ops).


% extend_over/3
extend_over(env(Gamma, Overs, Insts), Op, env(Gamma, Overs2, Insts)) :-
    env_over_set(Overs, OverSet),
    \+ member(Op, OverSet),
    Overs2 = [over(Op, []) | Overs].

extend_over(Env, Op, _) :-
    throw(over_error(already_exists, Env, Op)).


% extend_inst/4
extend_inst(env(Gamma, Overs, Insts), Inst, Impl, env(Gamma, Overs2, Insts2)) :-
    insert_inst_overs(Overs, Inst, Overs2),
    insert_inst_insts(Insts, Inst, Impl, Insts2).


% insert_inst_overs/3
insert_inst_overs([over(Op, Insts) | Overs], Inst, Overs2) :-
    Inst = inst(var(Op), OpT, Exp),

    % sanity check
    (\+ member(inst(Op, OpT, _), Insts)
    -> true
    ;  throw(inst_error(allready_exists, Inst, Insts))),

    Overs2 = [over(Op, [Inst | Insts]) | Overs].

insert_inst_overs([_ | Overs], Inst, Overs2) :-
    insert_inst_overs(Overs, Inst, Overs2).

insert_inst_overs([], Inst, _) :-
    throw(inst_error(missing_over, Inst)).


% implement_inst/3
implement_inst(inst(var(Op), OpT, Exp), ImplT, Impl) :-
    inst_name(Op, OpT, Name),
    skolemize(OpT, Type),
    remove_empty_scheme(Type, ImplT),
    Impl = defvar(var(Name), Exp).


% insert_inst_insts/3
insert_inst_insts(Insts, Inst, Impl, Insts2) :-
    implement_inst(Inst, ImplT, Impl),
    Inst = inst(var(Op), _, _),
    Impl = defvar(var(Name), _),
    \+ member([Op, ImplT, ImplName], Insts),
    Insts2 = [[Op, ImplT, ImplName] | Insts].

insert_inst_insts(Insts, Inst, _, _) :-
    throw(impl_inst_error(already_exists, Inst, Insts)).


% alpha_type/2
alpha_type(arrow(list(_), _), list).
alpha_type(arrow(Alpha, _), Alpha).
% alpha_type(Alpha, Alpha).
alpha_type(T, _) :-
    throw(inst_type_error(missing_arrow_type, T)).


% inst_name/3
inst_name(Op, forall(_, OpT), Name) :-
    alpha_type(OpT, Alpha),
    atomic_list_concat(['inst_', Op, '_', Alpha], Name).


% extend_simple/4 replaces simple_extend
extend_simple(env(Gamma, Overs, Insts), Var, T, env(Gamma2, Overs, Insts)) :-
    skolemize(T, ST),
    Gamma2 = [[Var, ST] | Gamma].


% extend/4
extend(env(Gamma, Overs, Insts), Var, T, env(Gamma2, Overs, Insts)) :-
    skolemize(T, forall(Schemes, ST)),
    free_env_vars(Gamma, GVs),
    free_type_vars(ST, FreeTVs),
    set:subtract(FreeTVs, GVs, Schemes2),
    Gamma2 = [[Var, forall(Schemes2, ST)] | Gamma].


% forall elimination
% lookup/3
lookup(X, env(Gamma, _, _), Type) :-
    lookup(X, Gamma, Type).
lookup(X, [[X, Scheme] | _], Type) :-
    !, type_from_scheme(Scheme, Type).
lookup(X, [_ | Tail], Type) :-
    lookup(X, Tail, Type).


% lookup_inst/?
%lookup_inst()


% type_from_scheme/2
type_from_scheme(forall(TVs, T), FreshT) :-
    skolemize(forall(TVs, T), forall(STVs, ST)),
    fresh_vars(STVs, FreshTVs),
    replace(ST, STVs, FreshTVs, FreshT).


% fresh_vars
fresh_vars([], []).
fresh_vars([X | Xs], [Y | Ys]) :- fresh_vars(Xs, Ys).


% replace/4
replace(T, Vars, Fresh, Type) :-
    var(T), !,
    replace_var(T, Vars, Fresh, Type).
replace(bool,  _, _, bool).
replace(float, _, _, float).
replace(int,   _, _, int).
replace(list(T), Vars, Fresh, list(Type)) :-
    replace(T, Vars, Fresh, Type).
replace(arrow(T1, T2), Vars, Fresh, arrow(Type1, Type2)) :-
    replace(T1, Vars, Fresh, Type1),
    replace(T2, Vars, Fresh, Type2).


% replace_var/4
replace_var(X, [],       [],       X).
replace_var(X, [Z | Zs], [Y | Ys], Y) :- X == Z, !.
replace_var(X, [Z | Zs], [Y | Ys], W) :- replace_var(X, Zs, Ys, W).


% free vars/2
free_env_vars([], []).
free_env_vars([[X, S] | G], U) :-
    free_scheme_vars(S, SVs),
    free_env_vars(G, GVs),
    set:union(SVs, GVs, U).


% free_type_vars/2
free_type_vars(T,     [T]) :- var(T), !.
free_type_vars(bool,  []).
free_type_vars(float, []).
free_type_vars(int,   []).

free_type_vars(list(T), TVs) :-
    free_type_vars(T, TVs).

free_type_vars(arrow(T1, T2), TVs) :-
    free_type_vars(T1, T1Vs),
    free_type_vars(T2, T2Vs),
    set:union(T1Vs, T2Vs, TVs).


% free_scheme_vars/2
% TODO: fix, schemes are `scheme(TyVar, Constraints)`
free_scheme_vars(forall(Schemes, T), SVars) :-
    free_type_vars(T, TVars),
    type_vars_from_scheme(Schemes, SchemeVars),
    set:subtract(TVars, Schemes, SVars).


% type_vars_from_scheme/2
type_vars_from_scheme([], []).
type_vars_from_scheme([scheme(Var, _) | Schemes], [Var | Vars]) :-
    type_vars_from_scheme(Schemes, Vars).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% skolemize/2 skolemizes a type scheme by moving all universal quantifiers to
% the front of the type.

skolemize(T,       forall([], T)) :- var(T), !.
skolemize(bool,    forall([], bool)).
skolemize(float,   forall([], float)).
skolemize(int,     forall([], int)).
skolemize(list(T), forall([], list(T))).

skolemize(arrow(T1, T2), forall(Schemes, arrow(T1_2, T2_2))) :-
    skolemize(T1, forall(T1Schemes, T1_2)),
    skolemize(T2, forall(T2Schemes, T2_2)),
    set:union(T1Schemes, T2Schemes, Schemes).

skolemize(forall(Schemes, T), forall(Schemes3, T2)) :-
    skolemize(T, forall(Schemes2, T2)),
    set:union(Schemes, Schemes2, Schemes3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% remove_empty_scheme/2 removes the forall quantifier if the type variables are
% empty.

remove_empty_scheme(forall([], T), T).
remove_empty_scheme(forall(TVs, T), forall(TVs, T)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_arrow_type/3

create_arrow_type(T1, T2, Arrow) :-
    skolemize(arrow(T1, T2), ST),
    remove_empty_scheme(ST, Arrow).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% initial environment

init_gamma([
    % bool
    ['not',   forall([], arrow(bool, bool))],
    ['and',   forall([], arrow(bool, arrow(bool, bool)))],
    ['or',    forall([], arrow(bool, arrow(bool, bool)))],
    ['=bool', forall([], arrow(bool, arrow(bool, bool)))],

    % int
    ['=int', forall([], arrow(int, arrow(int, bool)))],
    ['<int', forall([], arrow(int, arrow(int, bool)))],
    ['+int', forall([], arrow(int, arrow(int, int)))],
    ['-int', forall([], arrow(int, arrow(int, int)))],
    ['*int', forall([], arrow(int, arrow(int, int)))],
    ['/int', forall([], arrow(int, arrow(int, int)))],

    % float
    ['=float', forall([], arrow(float, arrow(float, bool)))],
    ['<float', forall([], arrow(float, arrow(float, bool)))],
    ['+float', forall([], arrow(float, arrow(float, float)))],
    ['-float', forall([], arrow(float, arrow(float, float)))],
    ['*float', forall([], arrow(float, arrow(float, float)))],
    ['/float', forall([], arrow(float, arrow(float, float)))],

    % list
    ['nil?', forall([Tn], arrow(list(Tn), bool))],
    ['cons', forall([Tc], arrow(Tc, arrow(list(Tc), list(Tc))))],
    ['head', forall([Th], arrow(list(Th), Th))],
    ['tail', forall([Tt], arrow(list(Tt), list(Tt)))]
]).


init_type_env(env(Gamma, [], [])) :-
    init_gamma(Gamma).
