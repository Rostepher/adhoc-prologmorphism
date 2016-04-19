:- module(lexer, [tokenize/2]).

:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, codes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% helper predicates

is_letter(Code) :- code_type(Code, alpha).
is_digit(Code)  :- code_type(Code, digit).

is_initial(Code) :-
    is_letter(Code);
    memberchk(Code, "!$&+-*/:<=>?~_^"). % TODO: add '%' character

is_subsequent(Code) :-
    is_initial(Code);
    is_digit(Code);
    memberchk(Code, ".").

is_keyword(Codes) :- memberchk(Codes, [
    % keywords
    "defun",
    "defvar",
    "if",
    "lambda",
    "let",

    % literals
    "true",
    "false"
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rules

ident([C|Cs])  --> [C], { is_initial(C) }, identr(Cs).
identr([C|Cs]) --> [C], { is_subsequent(C) }, identr(Cs).
identr([])     --> [].

keyword(K) --> ident(K), { is_keyword(K) }.

symbol(lparen)    --> "(".
symbol(rparen)    --> ")".
symbol(lbracket)  --> "[".
symbol(rbracket)  --> "]".
symbol(comma)     --> ",".
symbol(arrow)     --> "->".
symbol(colon)     --> ":".
symbol(semicolon) --> ";".

% float must come before int
token(float(Float)) --> dcg_basics:float(Float).
token(int(Int))     --> dcg_basics:integer(Int).

token(Symbol)       --> symbol(Symbol).
token(Keyword)      --> keyword(K), { atom_codes(Keyword, K) }.
token(ident(Ident)) --> ident(I),   { atom_codes(Ident, I) }.

% illegal token
token(_) --> [Code], { char_code(Char, Code), throw(lexer_error(Char)) }.

tokens([Token|Tokens]) -->
    dcg_basics:blanks,
    token(Token),
    tokens(Tokens),
    !. % longest wins
tokens([]) --> dcg_basics:blanks, [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tokenize/2 is a helper predicate to transform a list of codes into a list of
% tokens.

tokenize(Codes, Tokens) :-
    phrase(tokens(Tokens), Codes).
