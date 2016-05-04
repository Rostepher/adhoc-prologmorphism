:- module(pretty_printer, [print_error/1, print_results/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% print_error/1 writes an error message for custom error predicates that can
% be thrown in repl/0.

% lexer_error
print_error(lexer_error(Char)) :-
    format('A lexer error occurred with character: "~w"~n', [Char]).

% parser_error
print_error(parser_error(Token)) :-
    format('A parser error occurred with token: "~w"~n', [Token]).

% transform_error
print_error(transform_error(Exp)) :-
    format('A transformation error occurred with expression: "~w"~n', [Exp]).

% type_error
print_error(type_error(Exp, Gamma)) :-
    format('A type error occurred with expression: "~w"~n', [Exp]),
    format('    gamma: ~w~n', [Gamma]).

% runtime_error
print_error(runtime_error(Exp, env(Rho, Global))) :-
    format('A runtime error occurred with expression: "~w"~n', [Exp]),
    format('    local env:  ~w~n', [Rho]),
    format('    global env: ~w~n', [Global]).

% other error
print_error(Err) :-
    format('An unknown error occurred: ~w~n', [Err]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% print_result/1

print_result(result(none, _)).

print_result(result(Val, Type)) :-
    val_to_string(Val, ValStr),
    type_to_string(Type, TypeStr),
    format('~w : ~w~n', [ValStr, TypeStr]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% print_results/1

print_results([]).
print_results([Result | Results]) :-
    print_result(Result),
    print_results(Results).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% val_to_string/2

val_to_string(true,  'true').
val_to_string(false, 'false').

val_to_string(nil, '[]').
val_to_string(cons(H, T), Str) :-
    val_to_string(H, HStr),
    val_to_string(T, TStr),
    format(atom(Str), '(cons ~w ~w)', [HStr, TStr]).

val_to_string(closure(_, _, _), '<#closure>').
val_to_string(prim(Op), Str) :-
    format(atom(Str), '<#built-in:~w>', [Op]).

val_to_string(Val, Str) :-
    format(atom(Str), '~w', [Val]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type_to_string/2

type_to_string(Var, Str) :-
    var(Var),
    format(atom(Str), '~w', [Var]).

type_to_string(Atom, Str) :-
    atom(Atom),
    atom_string(Atom, Str).

type_to_string(list(T), Str) :-
    type_to_string(T, TStr),
    format(atom(Str), '[~w]', [TStr]).

type_to_string(arrow(T1, T2), Str) :-
    type_to_string(T1, T1Str),
    type_to_string(T2, T2Str),
    format(atom(Str), '~w -> ~w', [T1Str, T2Str]).

type_to_string(forall([], T), Str) :-
    type_to_string(T, Str).
type_to_string(forall(TVs, T), Str) :-
    type_to_string(T, TStr),
    forall_to_string(TVs, ForallStr),
    format(atom(Str), '~w~w', [ForallStr, TStr]).

forall_to_string([], '').
forall_to_string([TV | TVs], Str) :-
    forall_to_string(TVs, ForallStr),
    format(atom(Str), 'forall ~w . ~w', [TV, ForallStr]).
