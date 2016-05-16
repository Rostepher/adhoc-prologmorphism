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
judge_type(Env, defun(var(Fun), var(Arg), ArgT, Exp, ExpT), Defun, DefunT, Env4) :-
    extend_simple(Env, Arg, ArgT, Env2),
    extend_simple(Env2, Fun, arrow(ArgT, ExpT), Env3),

    % find overloaded constraints and replace them
    create_arrow_type(ArgT, ExpT, DefunT),
    get_type_schemes(DefunT, Schemes),
    get_constraint_set(Schemes, Constraints),
    gen_over_vars(Constraints, OverVars),
    replace_over_vars(OverVars, Exp2, Exp3),

    judge_type(Env3, Exp, Exp2, ExpT, _),
    extend(Env, Fun, DefunT, Env4),

    Defun = defun(var(Fun), var(Var), VarT, Exp3, ExpT).


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
% judge_type(Env, nil, nil, forall([scheme(Tn, [])], list(Tn)), Env).
% judge_type(Env, nil, nil, list(_T), Env).


% lists
judge_type(Env, cons(Head, Tail), Cons, list(T), Env3) :-
    judge_type(Env,  Head, Head2, T, Env2),
    judge_type(Env2, Tail, Tail2, list(T), Env3),
    Cons = cons(Head2, Tail2).


% vars
judge_type(Env, var(Var), var(Var2), VarT, Env) :-
    atom(Var),
    lookup(Var, Env, Var2, VarT).


% over_vars
judge_type(Env, over_var(Over, OverT), var(Inst), InstT, Env) :-
    env_insts(Env, Insts),
    lookup_inst(Over, OverT, Insts, Inst),
    judge_type(Env, var(Inst), var(Inst), InstT, Env).

judge_type(Env, over_var(Over, OverT), _, _, _) :-
    throw(type_error(inst_missing(Env, Over, OverT))).


% literals
judge_type(Env, true,     true,     bool,  Env).
judge_type(Env, false,    false,    bool,  Env).
judge_type(Env, float(F), float(F), float, Env) :- float(F).
judge_type(Env, int(I),   int(I),   int,   Env) :- integer(I).


% error
judge_type(Env, Exp, _, _, _) :-
    throw(type_error(Exp, Env)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% convienience predicates

type_check(Env, [], [], [], Env).
type_check(Env, [Ast | Rest], [NewAst | NewRest], [T | Ts], Env3) :-
    quantify_ast([], Ast, Ast2),
    judge_type(Env,  Ast2, NewAst,  T,  Env2),
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

% accessors
env_gamma(env(Gamma, _, _), Gamma).
env_overs(env(_, Overs, _), Overs).
env_insts(env(_, _, Insts), Insts).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% env_over_set/2 computes the set of overloaded operators in the envronment.

env_over_set([], []).
env_over_set([over(Op, _) | Overs], [Op | Ops]) :-
    env_over_set(Overs, Ops).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% extend_over/3 extends the envrionment with an overload declaration.

extend_over(env(Gamma, Overs, Insts), Op, env(Gamma, Overs2, Insts)) :-
    env_over_set(Overs, OverSet),
    \+ member(Op, OverSet),
    Overs2 = [over(Op, []) | Overs].

extend_over(Env, Op, _) :-
    throw(over_error(already_exists, Env, Op)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% extend_inst/4 extends the envrionment with an instance declaration and
% generates the implementation for the instance.

extend_inst(env(Gamma, Overs, Insts), Inst, Impl, env(Gamma, Overs2, Insts2)) :-
    insert_inst_overs(Overs, Inst, Overs2),
    insert_inst_insts(Insts, Inst, Impl, Insts2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% insert_inst_overs/3 inserts and instance into the provided overloads
% envrionment, checking that the overloaded operator exists and that the
% instance has not already been declared.

insert_inst_overs([over(Op, Insts) | Overs], Inst, Overs2) :-
    Inst = inst(var(Op), OpT, Exp),

    % sanity check
    (\+ member(inst(Op, OpT, _), Insts)
    -> true
    ;  throw(inst_error(already_exists, Inst, Insts))),

    Overs2 = [over(Op, [Inst | Insts]) | Overs].

insert_inst_overs([_ | Overs], Inst, Overs2) :-
    insert_inst_overs(Overs, Inst, Overs2).

insert_inst_overs([], Inst, _) :-
    throw(inst_error(missing_over, Inst)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% implement_inst/3 generates the implementation of an instances, which is just
% a defvar declaration.

implement_inst(inst(var(Op), OpT, Exp), ImplT, Impl) :-
    inst_name(Op, OpT, Name),
    skolemize(OpT, Type),
    remove_empty_scheme(Type, ImplT),
    Impl = defvar(var(Name), Exp).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% insert_inst_insts/3 generates the implementation of an instance and inserts
% it into the provided instances environment.

insert_inst_insts(Insts, Inst, Impl, Insts2) :-
    implement_inst(Inst, ImplT, Impl),
    Inst = inst(var(Op), _, _),
    Impl = defvar(var(ImplName), _),
    \+ member(inst(Op, ImplT, ImplName), Insts),
    Insts2 = [inst(Op, ImplT, ImplName) | Insts].

insert_inst_insts(Insts, Inst, _, _) :-
    throw(impl_inst_error(already_exists, Inst, Insts)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% alpha_type/2 determines the alpha type for an instance, which is of the form
% alpha -> tau, where tau can contain other uses of alpha.

alpha_type(arrow(list(_), _), list).
alpha_type(arrow(Alpha, _), Alpha).
alpha_type(forall(_, T), Alpha) :-
    alpha_type(T, Alpha).
alpha_type(T, _) :-
    throw(inst_type_error(missing_arrow_type, T)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% inst_name/3 generates a unique name for an instance.

inst_name(Op, OpT, Name) :-
    alpha_type(OpT, Alpha),
    atomic_list_concat(['inst', Op], Base),
    gensym(Base, Name).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% not_defined/2

not_defined(env(Gamma, _, _), Var) :-
    \+ member([Var, _], Gamma).

not_defined(Env, Var) :-
    throw(type_error(already_defined(Env, Var))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% extend_simple/4 extends the envrionment with the provided variable and
% associated type.

extend_simple(env(Gamma, Overs, Insts), Var, T, env(Gamma2, Overs, Insts)) :-
    not_defined(env(Gamma, Overs, Insts), Var),
    skolemize(T, ST),
    Gamma2 = [[Var, ST] | Gamma].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% extend/4 extends the environment with the provided variable and associated
% type after first generating fresh type variables.
%
% forall introduction

extend(env(Gamma, Overs, Insts), Var, T, env(Gamma2, Overs, Insts)) :-
    not_defined(env(Gamma, Overs, Insts), Var),
    skolemize(T, forall(Schemes, ST)),
    free_env_vars(Gamma, GVs),
    free_type_vars(ST, FreeTVs),
    set:subtract(FreeTVs, GVs, Schemes2),
    Gamma2 = [[Var, forall(Schemes2, ST)] | Gamma].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% replace_over_var/4 walks the ast and replaces all uses of `var(Over)` with
% `over_var(Over, OverT)`, which is a unique name for an overloaded variable.

replace_over_var(Over, OverT, defvar(Var, Exp), Defvar) :-
    replace_over_var(Over, OverT, Exp, Exp2),
    Defvar = defvar(Var, Exp2).

replace_over_var(Over, OverT, defun(Fun, Arg, ArgT, Exp, ExpT), Defun) :-
    replace_over_var(Over, OverT, Arg, Arg2),
    replace_over_var(Over, OverT, Exp, Exp2),
    Defun = defun(Fun, Arg2, ArgT, Exp2, ExpT).

replace_over_var(Over, OverT, if(Cond, Then, Else), If) :-
    replace_over_var(Over, OverT, Cond, Cond2),
    replace_over_var(Over, OverT, Then, Then2),
    replace_over_var(Over, OverT, Else, Else2),
    If = if(Cond2, Then2, Else2).

replace_over_var(Over, OverT, lambda(Arg, ArgT, Body), Lambda) :-
    replace_over_var(Over, OverT, Arg,  Arg2),
    replace_over_var(Over, OverT, Body, Body2),
    Lambda = lambda(Arg2, ArgT, Body2).

replace_over_var(Over, OverT, let(Var, Exp, Body), Let) :-
    replace_over_var(Over, OverT, Var,  Var2),
    replace_over_var(Over, OverT, Exp,  Exp2),
    replace_over_var(Over, OverT, Body, Body2),
    Let = let(Var2, Exp2, Body2).

replace_over_var(Over, OverT, apply(Exp, Arg), Apply) :-
    replace_over_var(Over, OverT, Exp, Exp2),
    replace_over_var(Over, OverT, Arg, Arg2),
    Apply = apply(Exp2, Arg2).

replace_over_var(_, _, nil, nil).
replace_over_var(Over, OverT, cons(Head, Tail), Cons) :-
    replace_over_var(Over, OverT, Head, Head2),
    replace_over_var(Over, OverT, Tail, Tail2),
    Cons = cons(Head2, Tail2).

replace_over_var(Over, OverT, var(Over), over_var(Over, OverT)).
replace_over_var(_,    _,     var(Var),  var(Var)).

replace_over_var(_, _, Exp, Exp).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% replace_over_vars/4

replace_over_vars([], Exp, Exp).
replace_over_vars([over_var(Over, OverT) | Vars], Exp, Exp3) :-
    replace_over_var(Over, OverT, Exp, Exp2),
    replace_over_vars(Vars, Exp2, Exp3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_type_schemes/2

get_type_schemes(forall(Schemes, _), Schemes).
get_type_schemes(_, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_constraint_set/2

get_constraint_set([], []).
get_constraint_set([scheme(Var, Constraints) | Schemes], Set) :-
    get_constraint_set(Schemes, Constraints2),
    set:union(Constraints, Constraints2, Set).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_over_vars

gen_over_vars([], []).
gen_over_vars([constraint(Op, OpT) | Constraints], [Var | Vars]) :-
    Var = over_var(Op, OpT),
    gen_over_vars(Constraints, Vars).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lookup/3 attempts to find a variable in the envrionment, first in the set of
% overloaded operators and then in the typothesis.
%
% forall elimination

lookup(Var, Env, Inst, VarT) :-
    env_overs(Env, Overs),
    env_over_set(Overs, Set),
    member(Var, Set),

    env_insts(Env, Insts),
    lookup_inst(Var, VarT, Insts, Inst).

lookup(Var, Env, Var, VarT) :-
    lookup_scheme(Var, Env, VarT).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lookup_scheme/3

lookup_scheme(Var, env([[Var, Scheme] | _], Overs, _), Type) :-
    !,
    type_from_scheme(Scheme, Type).

lookup_scheme(Var, env([_ | Tail], Overs, Insts), Type) :-
    lookup_scheme(Var, env(Tail, Overs, Insts), Type).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lookup_inst/4

lookup_inst(Op, OpT, [inst(Op, OpT, InstName) | Insts], InstName) :- !.
lookup_inst(Op, OpT, [_ | Insts], InstName) :-
    lookup_inst(Op, OpT, Insts, InstName).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type_from_scheme/2

type_from_scheme(forall(TVs, T), FreshT) :-
    skolemize(forall(TVs, T), forall(STVs, ST)),
    fresh_vars(STVs, FreshTVs),
    replace(ST, STVs, FreshTVs, FreshT).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fresh_vars

fresh_vars([], []).
fresh_vars([X | Xs], [Y | Ys]) :- fresh_vars(Xs, Ys).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% replace_var/4

replace_var(X, [],       [],       X).
replace_var(X, [Z | Zs], [Y | Ys], Y) :- X == Z, !.
replace_var(X, [Z | Zs], [Y | Ys], W) :- replace_var(X, Zs, Ys, W).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% free_env_vars/2 generates the set of free variables from the typothesis.

free_env_vars([], []).
free_env_vars([[X, S] | G], U) :-
    free_scheme_vars(S, SVs),
    free_env_vars(G, GVs),
    set:union(SVs, GVs, U).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% free_type_vars/2 generates the set of free type variables from a type.

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% free_scheme_vars/2 generates the set of free type variables from a scheme.

free_scheme_vars(forall(Schemes, T), SVars) :-
    free_type_vars(T, TVars),
    type_vars_from_scheme(Schemes, SchemeVars),
    set:subtract(TVars, Schemes, SVars).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type_vars_from_scheme/2 generates the set of type variables from a scheme.

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
% create_arrow_type/3 creates an arrow type from the given parts, enforcing
% skolemization.

create_arrow_type(T1, T2, Arrow) :-
    skolemize(arrow(T1, T2), ST),
    remove_empty_scheme(ST, Arrow).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% init_gamma/1 creates the initial typothesis.

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
    ['nil',  forall([Tl], list(Tl))],
    ['nil?', forall([Tn], arrow(list(Tn), bool))],
    ['cons', forall([Tc], arrow(Tc, arrow(list(Tc), list(Tc))))],
    ['head', forall([Th], arrow(list(Th), Th))],
    ['tail', forall([Tt], arrow(list(Tt), list(Tt)))]
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% init_type_env/1 creates the initial type environment.

init_type_env(env(Gamma, [], [])) :-
    init_gamma(Gamma).
