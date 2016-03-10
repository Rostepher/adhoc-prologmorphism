% :- module(lexer, [lex/2]).

:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, codes).

alpha(A) --> [A], { code_type(A, alpha) }.

alpha_num(C)       --> [C], { code_type(C, alnum) }.
alpha_nums([C|Cs]) --> alpha_num(C), alpha_nums(Cs).
alpha_nums([C])    --> alpha_num(C).

ident([C|Cs]) --> alpha(C), alpha_nums(Cs).
ident([C])    --> alpha(C).

token(ident(Token)) --> ident(Token).
token(int(Token))   --> integer(Token).
token(float(Token)) --> float(Token).
token(lparen)       --> [Code], { char_code('(', Code) }.
token(rparen)       --> [Code], { char_code(')', Code) }.

tokenize([Token|Tokens]) --> blanks, token(Token), tokenize(Tokens).
tokenize([])             --> [].
