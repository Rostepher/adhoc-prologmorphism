:- module(parser, []).

:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, codes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Grammar

parse(Prog, ParseTree) :-
    phrase(program(Prog), ParseTree).

program([]) --> [].
program([Head | Tail]) -->
    blanks,
    expr(Head),
    blanks,
    program(Tail),
    !.

% expr(ident(I)) --> symbol(Literal), { atom_codes(Symbol, Literal) }.
expr(int_lit(I))   --> integer(I).
expr(float_lit(F)) --> float(F).
expr(List)         --> "(", program(List), ")".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parse Tree -> AST
