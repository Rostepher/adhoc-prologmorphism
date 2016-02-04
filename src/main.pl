:- module(main, [main/0]).

:- use_module(type_checker).
:- use_module(interpreter).

read_prompt(Msg, Input) :-
    write(Msg),
    prompt(_, ''),
    read(Input).

write_exp(Exp) :-
    write(Exp),
    nl.

% Read Evaluate Print Loop (REPL), that reads a program from the user, then
% type checks and evaluates the program, printing the final result to stdout.
repl() :-
    read_prompt('> ', Input),
    type_check(Input),
    evaluate(Input, env([], []), Value, Env),
    write_exp(Value),
    write_exp(Env),
    repl().

main() :-
    repl().
