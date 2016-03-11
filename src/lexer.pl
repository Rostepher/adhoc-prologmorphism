:- module(lexer, [tokenize/2]).

:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, codes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper Predicates

is_alpha(Code)    :- code_type(Code, alpha).
is_alphanum(Code) :- code_type(Code, alnum).
is_special(Code)  :- memberchk(Code, "+-*/<>=_@^&!?").

is_ident(Code)  :- is_alpha(Code); is_special(Code).
is_identr(Code) :- is_alphanum(Code); is_special(Code).

is_keyword(Codes) :- memberchk(Codes, [
    % keywords
    "define",
    "if",
    "lambda",
    "let",

    % literals
    "true",
    "false"
]).

is_type(Codes) :- memberchk(Codes, [
    "bool",
    "float",
    "int"
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rules

ident([C|Cs])  --> [C], { is_ident(C)  }, identr(Cs).
identr([C|Cs]) --> [C], { is_identr(C) }, identr(Cs).
identr([])     --> [].

keyword(K) --> ident(K), { is_keyword(K) }.

type(T) --> ident(T), { is_type(T) }.

symbol(lparen)    --> "(".
symbol(rparen)    --> ")".
symbol(arrow)     --> "->".
symbol(colon)     --> ":".
symbol(semicolon) --> ";".

token(Symbol)       --> symbol(Symbol).
token(Keyword)      --> keyword(K), { atom_codes(Keyword, K) }.
token(type(Type))   --> type(T), { atom_codes(Type, T) }.
token(ident(Ident)) --> ident(Ident).
token(int(Int))     --> integer(Int).
token(float(Float)) --> float(Float).

% illegal token
token(_)            --> [Token], { throw(lexer_error(Token)) }.

tokens([Token|Tokens]) -->
    blanks,
    token(Token),
    tokens(Tokens),
    !. % longest wins
tokens([]) --> blanks, [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tokenize/2

tokenize(Codes, Tokens) :-
    phrase(tokens(Tokens), Codes).
