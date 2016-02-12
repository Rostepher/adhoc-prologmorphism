:- module(interpreter, [evaluate/4]).

:- use_module(environment).

:- reexport(environment, [init_env/1]).


% delta/3 evaluates primitives by parially applying values to a primitive atom
% or by executing the primitive operation once it has been partially applied.

delta(eq(X), X, true).
delta(eq(_), _, false).

delta(sum(X),  Y, Val) :- Val is X + Y.
delta(prod(X), Y, Val) :- Val is X * Y.
delta(diff(X), Y, Val) :- Val is X / Y.

delta(Prim, Arg, prim(Val)) :- Val =.. [Prim, Arg].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% evaluate

% evaluate_call_closure/4 is a helper fuction for evaluating a call expression
% where the evaluation of M is a closure.
evaluate_call_closure(call(M, N), env(_, Global1), Val2, Global3) :-
    M = closure(X, M_C, Rho1),
    evaluate(N, env(Rho1, Global1), Val1, Global2),
    extend(Rho1, X, Val1, Rho2),
    evaluate(M_C, env(Rho2, Global2), Val2, Global3).

% evaluate_call_prim/4 is a helper fuction for evaluating a call expression
% where the evaluation of M is a primitive.
evaluate_call_prim(call(prim(F), N), env(Rho1, Global1), Val2, Global2) :-
    evaluate(N, env(Rho1, Global1), Val1, Global2),
    delta(F, Val1, Val2).

% evaluate/4 evaluates a valid expression in the language with a given
% local and global envrionment and produces an ouput value and possibly
% extended global environment.
%
% Rho    - local environment
% Global - global environment

% if
evaluate(if(M1, M2, _), env(Rho, Global1), Val, Global3) :-
    evaluate(M1, env(Rho, Global1), true, Global2),
    evaluate(M2, env(Rho, Global2), Val, Global3).

evaluate(if(M1, _, M3), env(Rho, Global1), Val, Global3) :-
    evaluate(M1, env(Rho, Global1), false, Global2),
    evaluate(M3, env(Rho, Global2), Val, Global3).

% let
evaluate(let(X, M1, M2), env(Rho1, Global1), Val2, Global3) :-
    evaluate(M1, env(Rho1, Global1), Val1, Global2),
    extend(Rho1, X, Val1, Rho2),
    evaluate(M2, env(Rho2, Global2), Val2, Global3).

% lambda
evaluate(lambda(X, _, M), env(Rho, Global), closure(X, M, Rho), Global).

% define
evaluate(define(Y, X, _, M, _), env(Rho, Global1), _, Global2) :-
    extend(Global1, Y, closure(X, M, Rho), Global2).

% const
evaluate(const(X, M), env(Rho, Global1), _, Global3) :-
    evaluate(M, env(Rho, Global1), Val, Global2),
    extend(Global2, X, Val, Global3).

% call

evaluate(call(M, N), env(Rho1, Global1), Val2, Global3) :-
    evaluate(M, env(Rho1, Global1), Val1, Global2),
    % if
    (functor(Val1, closure, 3) ->
        % then
        evaluate_call_closure(call(Val1, N), env(Rho1, Global2), Val2, Global3);
        % else
        evaluate_call_prim(call(Val1, N), env(Rho1, Global2), Val2, Global3)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% list builtins

% nil
evaluate(nil(T), env(_, Global), nil(T), Global).

% cons
evaluate(cons(M, N), env(_, Global), cons(M, N), Global).

% is_nil
evaluate(is_nil(nil(_)), env(_, Global), true, Global).
evaluate(is_nil(_), env(_, Global), false, Global).

% head
evaluate(head(nil(T)), env(_, Global), nil(T), Global).
evaluate(head(M), env(Rho, Global1), Head, Global2) :-
    evaluate(M, env(Rho, Global1), cons(Head, _), Global2).

% tail
evaluate(tail(nil(T)), env(_, Global), nil(T), Global).
evaluate(tail(M), env(Rho, Global1), Tail, Global2) :-
    evaluate(M, env(Rho, Global1), cons(_, Tail), Global2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% literals
%
% the number, variable and error cases encur extra computations, they have
% been moved to the bottom for the sake of optimization.

% bool
evaluate(true,  env(_, Global), true,  Global).
evaluate(false, env(_, Global), false, Global).

% int
evaluate(N, env(_, Global), N, Global) :- integer(N).

% float
evaluate(N, env(_, Global), N, Global) :- float(N).

% variable
evaluate(Var, env(Rho, Global), Val, Global) :-
    lookup(Var, env(Rho, Global), Val).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% error

evaluate(Exp, env(Rho, Global), _, _) :-
    throw(runtime_error(Exp, env(Rho, Global))).

