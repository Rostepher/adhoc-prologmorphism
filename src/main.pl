:- module(main, [main/0]).

:- use_module(type_checker).
:- use_module(interpreter).

% error_handler/1 writes an error message for custom error predicates that can
% be thrown in repl/0.

% type_error
error_handler(type_error(Exp)) :-
    format('A type error occurred with expression: "~w"~n', [Exp]),
    halt.

% runtime_error
error_handler(runtime_error(Exp, env(Rho, Gamma))) :-
    format('A runtime error occurred with expression: "~w"~n', [Exp]),
    format('    local env:  ~w~n', [Rho]),
    format('    global env: ~w~n', [Gamma]),
    halt.

% other error
error_handler(Err) :-
    format('An unknown error occurred: ~w~n', [Err]),
    halt.

% read_prompt/2 is a helper predicate to wirte a prompt to stdout and then
% read user input.
read_prompt(Msg, Input) :-
    write(Msg),
    prompt(_, ''),
    read(Input).

% repl/0 is a Read Evaluate Print Loop (REPL), that reads a program from the
% user, then type checks and evaluates the program, printing the final result
% to stdout.
repl :-
    read_prompt('> ', Input),
    catch(type_check(Input), Err1, error_handler(Err1)),
    catch(evaluate(Input, Value), Err2, error_handler(Err2)),
    writeln(Value).

% main/0 is a helper predicate to call repl/0 repeatedly, until interrupted by
% the user.
main :-
    repeat,
    repl.
