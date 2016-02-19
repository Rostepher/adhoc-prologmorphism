#!/usr/bin/env swipl

:- module(main, [main/0]).

:- use_module(type_checker).
:- use_module(interpreter).

% start main
% :- initialization main.

% write_error/1 writes an error message for custom error predicates that can
% be thrown in repl/0.

% type_error
write_error(type_error(Exp, Gamma)) :-
    format('A type error occurred with expression: "~w"~n', [Exp]),
    format('    gamma: ~w~n', [Gamma]).

% runtime_error
write_error(runtime_error(Exp, env(Rho, Global))) :-
    format('A runtime error occurred with expression: "~w"~n', [Exp]),
    format('    local env:  ~w~n', [Rho]),
    format('    global env: ~w~n', [Global]).

% other error
write_error(Err) :-
    format('An unknown error occurred: ~w~n', [Err]).

% read_prompt/2 is a helper predicate to wirte a prompt to stdout and then
% read user input.
read_prompt(Msg, Input) :-
    write(Msg),
    prompt(_, ''),
    read(Input).

% rep/4 is the 'R', 'E' and 'P' in REPL (Read Evaluate Print Loop), that reads
% a program from the user, then type checks and evaluates the program, printing
% the final result to stdout.
rep(Gamma1, Global1, Gamma2, Global2) :-
    % read
    read_prompt('> ', Input),

    % Ctl-D (a.k.a. end_of_file)
    (Input == end_of_file -> halt; true),

    % type check
    type_check(Gamma1, Input, Gamma2),

    % evaluate
    evaluate(Input, env([], Global1), Val, Global2),

    % print
    write_term(Val, [attributes(write), nl(true)]).

% repl/2 is the loop enclosing rep/4, which makes up the complete REPL.
repl(Gamma1, Global1) :-
    (catch((R = success, rep(Gamma1, Global1, Gamma2, Global2)),
        Err,
        R = error(Err))),

    % if rep failed
    (R = error(Err) ->
        % then
        write_error(Err),
        repl(Gamma1, Global1);

        % else
        repl(Gamma2, Global2)).

% main/0 is a helper predicate to setup the initial environment and then to
% call repl/2, until interrupted by the user.
main :-
    % command line arguments
    current_prolog_flag(argv, Argv),

    % print welcome message
    writeln('Welcome to the "Adhoc-Prologmorphism" REPL.'),
    writeln('Use Ctl-D to exit'),

    % initialise gamma and global environment
    init_gamma(Gamma),
    init_env(env(_, Global)),

    % turn on trace
    (member('--trace', Argv) -> trace; true),

    % start the REPL
    repl(Gamma, Global),

    halt.

