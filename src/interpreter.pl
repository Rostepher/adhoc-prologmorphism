:- module(interpreter, [evaluate/4, init_env/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% environment

% extend/4 extends a given environment with a key/value pair.
extend(Env, Key, Val, [[Key, Val] | Env]).

% lookup/3 searches for the value of a given key in an environment.
lookup(Key, [[Key, Val] | _], Val) :- !.
lookup(Key, [_ | Tail], Val) :-
    lookup(Key, Tail, Val).

lookup(Key, env(Rho, Gamma), Val) :-
    append(Rho, Gamma, Env),
    lookup(Key, Env, Val).

init_global_env([
    % bool
    ['not', prim('not')],

    % int
    ['=', prim('=')],
    ['+', prim('+')],
    ['-', prim('-')],
    ['*', prim('*')],
    ['/', prim('/')],

    % float
    ['=.', prim('=.')],
    ['+.', prim('+.')],
    ['-.', prim('-.')],
    ['*.', prim('*.')],
    ['/.', prim('/.')],

    % list
    ['nil?', prim('nil?')],
    ['cons', prim('cons')],
    ['head', prim('head')],
    ['tail', prim('tail')]
]).

% init_env/1 provides the initial local and global environments.
init_env(env([], Gamma)) :-
    init_global_env(Gamma).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% delta

% delta/3 evaluates primitives by parially applying values to a primitive atom
% or by executing the primitive operation once it has been partially applied.

% bools
delta('not', true, false).
delta('not', false, true).

% ints
delta('='(X), X, true).
delta('='(_), _, false).

delta('+'(X), Y, Val) :- Val is X + Y.
delta('-'(X), Y, Val) :- Val is X - Y.
delta('*'(X), Y, Val) :- Val is X * Y.
delta('/'(X), Y, Val) :- Val is X / Y.

% floats
delta('=.'(X), X, true).
delta('=.'(_), _, false).

delta('+.'(X), Y, Val) :- Val is X + Y.
delta('-.'(X), Y, Val) :- Val is X - Y.
delta('*.'(X), Y, Val) :- Val is X * Y.
delta('/.'(X), Y, Val) :- Val is X / Y.

% lists
delta('nil?', nil, true).
delta('nil?', _,   false).

delta('cons'(M), N, cons(M, N)).

delta('head', nil, nil).
delta('head', cons(M, _), M).

delta('tail', nil, nil).
delta('tail', cons(_, N), N).

% catch-all
delta(Prim, Arg, prim(Val)) :- Val =.. [Prim, Arg].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% evaluate

% evaluate_if/6
evaluate_if(true, M2, _, env(Rho, Global), Val, Global2) :-
    evaluate(M2, env(Rho, Global), Val, Global2).

evaluate_if(false, _, M3, env(Rho, Global), Val, Global2) :-
    evaluate(M3, env(Rho, Global), Val, Global2).

% evaluate_apply/5 is a helper fuction for evaluating an apply expression
% where the evaluation of M is a closure or primitive function.

% closure
evaluate_apply(closure(X, M_C, Rho_C), N, env(Rho, Global), Val2, Global3) :-
    evaluate(N, env(Rho, Global), Val, Global2),
    extend(Rho_C, X, Val, Rho2),
    evaluate(M_C, env(Rho2, Global2), Val2, Global3).

% prim
evaluate_apply(prim(F), N, env(Rho, Global), Val2, Global2) :-
    evaluate(N, env(Rho, Global), Val, Global2),
    delta(F, Val, Val2).

% evaluate/4 evaluates a valid expression in the language with a given
% local and global envrionment and produces an ouput value and possibly
% extended global environment.
%
% Rho    - local environment
% Global - global environment

% define
% evaluate(define(Y, X, _, M, _), env(Rho, Global), _, Global2) :-
%     extend(Global, Y, closure(X, M, Rho), Global2).

% const
% evaluate(const(X, M), env(Rho, Global), _, Global3) :-
%     evaluate(M, env(Rho, Global), Val, Global2),
%     extend(Global2, X, Val, Global3).

% define
evaluate(define(var(X), M), env(Rho, Global), none, Global3) :-
    evaluate(M, env(Rho, Global), Val, Global2),
    extend(Global2, X, Val, Global3).

% define (explicit type)
evaluate(define(var(X), _, M), env(Rho, Global), none, Global3) :-
    evaluate(M, env(Rho, Global), Val, Global2),
    extend(Global2, X, Val, Global3).

% if
evaluate(if(M1, M2, M3), env(Rho, Global), Val, Global3) :-
    evaluate(M1, env(Rho, Global), Cond, Global2),
    evaluate_if(Cond, M2, M3, env(Rho, Global2), Val, Global3).

% let
evaluate(let(var(X), M1, M2), env(Rho, Global), Val2, Global3) :-
    evaluate(M1, env(Rho, Global), Val, Global2),
    extend(Rho, X, Val, Rho2),
    evaluate(M2, env(Rho2, Global2), Val2, Global3).

% lambda
evaluate(lambda(var(X), _, M), env(Rho, Global), closure(X, M, Rho), Global).

% apply
evaluate(apply(M, N), env(Rho, Global), Val2, Global3) :-
    evaluate(M, env(Rho, Global), M2, Global2),
    evaluate_apply(M2, N, env(Rho, Global2), Val2, Global3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% list builtins

% nil
evaluate(nil, env(_, Global), nil, Global).

% cons
evaluate(cons(M, N), env(_, Global), cons(M, N), Global).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% literals
%
% the number, variable and error cases encur extra computations, they have
% been moved to the bottom for the sake of optimization.

% bool
evaluate(true,  env(_, Global), true,  Global).
evaluate(false, env(_, Global), false, Global).

% int
evaluate(int(N), env(_, Global), N, Global).

% float
evaluate(float(N), env(_, Global), N, Global).

% variable
evaluate(var(Var), env(Rho, Global), Val, Global) :-
    lookup(Var, env(Rho, Global), Val).
evaluate(var(Var, _), env(Rho, Global), Val, Global) :-
    lookup(Var, env(Rho, Global), Val).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% error

evaluate(Exp, env(Rho, Global), _, _) :-
    throw(runtime_error(Exp, env(Rho, Global))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% eval/2

eval([Prog], env(Rho, Global), [Result], Global2) :-
    evaluate(Prog, env(Rho, Global), Result, Global2).
eval([Prog | Rest], env(Rho, Global), [Result | Result2], Global3) :-
    evaluate(Prog, env(Rho, Global), Result, Global2),
    eval(Rest, env(Rho, Global2), Result2, Global3).
