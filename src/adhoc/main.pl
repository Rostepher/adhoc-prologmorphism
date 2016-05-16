:- module(main, [main/0]).

:- use_module(lexer).
:- use_module(parser).
:- use_module(type_checker).
:- use_module(interpreter).
:- use_module(pretty_printer).

:- use_module(library(readutil)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% zip_results/3

zip_results([], _, []).
zip_results(_, [], []).
zip_results([Val | Vals], [Type | Types], [result(Val, Type) | Results]) :-
    zip_results(Vals, Types, Results).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% interpret_codes/5

interpret_codes(Codes, TypeEnv, Global, TypeEnv2, Global2) :-
    % tokenize
    lexer:tokenize(Codes, Tokens), !,

    % parse the tokens into the ast
    parser:parse(Tokens, Ast), !,

    % type check the ast
    type_checker:type_check(TypeEnv, Ast, Ast2, Types, TypeEnv2), !,

    % evaluate the ast
    interpreter:eval(Ast2, env([], Global), Vals, Global2), !,

    % print the results
    zip_results(Vals, Types, Results),
    print_results(Results).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% interpret_file/5

interpret_file(File, TypeEnv, Global, TypeEnv2, Global2) :-
    read_file_to_codes(File, Codes, []),
    catch(interpret_codes(Codes, TypeEnv, Global, TypeEnv2, Global2),
        Error,
        (print_error(Error),
            copy_term(TypeEnv, TypeEnv2),
            copy_term(Global, Global2))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% interpret_files/5

interpret_files([], TypeEnv, Global, TypeEnv, Global).
interpret_files([F | Fs], TypeEnv, Global, TypeEnv3, Global3) :-
    interpret_file(F, TypeEnv, Global, TypeEnv2, Global2),
    interpret_files(Fs, TypeEnv2, Global2, TypeEnv3, Global3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read_prompt/2 is a helper predicate to wirte a prompt to stdout and then
% read user input.
read_prompt(Msg, Input) :-
    prompt(_, Msg),
    read_line_to_codes(user_input, Input).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rep/4 is the 'R', 'E' and 'P' in REPL (Read Evaluate Print Loop), that reads
% a program from the user, then type checks and evaluates the program, printing
% the final result to stdout.

rep(TypeEnv, Global, TypeEnv2, Global2) :-
    % read
    read_prompt('> ', Codes),

    % Ctl-D (a.k.a. end_of_file)
    (Codes == end_of_file -> halt; true),

    catch(interpret_codes(Codes, TypeEnv, Global, TypeEnv2, Global2),
        Error,
        (print_error(Error),
            copy_term(TypeEnv, TypeEnv2),
            copy_term(Global, Global2))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% repl/2 is the loop enclosing rep/4, which makes up the complete REPL.

repl(TypeEnv, Global) :-
    rep(TypeEnv, Global, TypeEnv2, Global2),
    repl(TypeEnv2, Global2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% init_opts_spec/1 command line options spec.

init_opts_spec([
    [
        opt(help), type(boolean), default(false),
        shortflags(['h']),
        longflags(['help']),
        help('prints this help message')
    ],
    [
        opt(debug), type(boolean), default(false),
        longflags(['debug']),
        help('enables debug mode allowing users to step through execution of their programs')
    ],
    [
        opt(stop), type(atom), default(eval),
        longflags(['stop']),
        help([
            'stops evaluation of the program at the given step, displaying relevant information',
            'possible values are: lex, parse, type-check and eval'
        ])
    ]
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parse_args/2

parse_args(opts(help(Help), debug(Debug), stop(Stop)), files(Args)) :-
    current_prolog_flag(argv, Argv),
    init_opts_spec(OptsSpec),
    opt_parse(OptsSpec, Argv, Opts, Args),

    % command-line options
    member(help(Help),   Opts),
    member(debug(Debug), Opts),
    member(stop(Stop),   Opts).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% print_help/2

print_help(false).
print_help(true) :-
    init_opts_spec(OptsSpec),
    opt_help(OptsSpec, HelpText),
    writeln('Usage: ap [opts] [file]'),
    writeln(HelpText),
    halt.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% main/0 is a helper predicate to setup the initial environment and then to
% call repl/2, until interrupted by the user.

main :-
    % command line arguments
    parse_args(opts(help(Help), debug(Debug), stop(_)), files(Files)),

    % help
    print_help(Help),

    % enable gtrace for debug options
    (Debug == true -> gtrace; true),

    % initialise type environment and global environment
    type_checker:init_type_env(TypeEnv),
    interpreter:init_env(env(_, Global)),

    % interpret files, then drop into repl
    interpret_files(Files, TypeEnv, Global, TypeEnv2, Global2),

    % print welcome message
    writeln('Welcome to the "Adhoc-Prologmorphism" REPL.'),
    writeln('Use Ctl-D to exit'),

    % start the REPL
    repl(TypeEnv2, Global2),

    halt.
