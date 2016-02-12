:- module(main, [main/0]).

:- use_module(type_checker).
:- use_module(interpreter).

% write_error/1 writes an error message for custom error predicates that can
% be thrown in repl/0.

% type_error
write_error(type_error(Exp, Gamma)) :-
    nl,
    format('A type error occurred with expression: "~w"~n', [Exp]),
    format('    gamma: ~w~n', [Gamma]),
    nl.

% runtime_error
write_error(runtime_error(Exp, env(Rho, Global))) :-
    nl,
    format('A runtime error occurred with expression: "~w"~n', [Exp]),
    format('    local env:  ~w~n', [Rho]),
    format('    global env: ~w~n', [Global]),
    nl.

% other error
write_error(Err) :-
    nl,
    format('An unknown error occurred: ~w~n', [Err]),
    nl.

% read_prompt/2 is a helper predicate to wirte a prompt to stdout and then
% read user input.
read_prompt(Msg, Input) :-
    write(Msg),
    prompt(_, ''),
    read(Input).

% repl/2 is a Read Evaluate Print Loop (REPL), that reads a program from the
% user, then type checks and evaluates the program, printing the final result
% to stdout.
repl(Gamma1, Global1) :-
    % read
    read_prompt('> ', Input),

    % Ctl-D (a.k.a. end_of_file)
    (Input == end_of_file -> fail; true),

    % type check
    catch(type_check(Gamma1, Input, Gamma2), Err1,
        (write_error(Err1), fail)),

    % evaluate
    catch(evaluate(Input, env([], Global1), Val, Global2), Err2,
        (write_error(Err2), fail)),

    % print
    write_term(Val, [attributes(write), nl(true)]),

    % repeat
    repl(Gamma2, Global2).

% main/0 is a helper predicate to call repl/0 repeatedly, until interrupted by
% the user.
main :-
    % print welcome message
    writeln('Welcome to the "Adhoc-Prologmorphism" REPL.'),
    writeln('Use Ctl-D to exit'),
    nl,

    % initialise gamma and global environment
    init_gamma(Gamma),
    init_env(env(_, Global)),

    % start the REPL
    repl(Gamma, Global).
