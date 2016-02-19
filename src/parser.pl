% :- module(parser, []).

:- set_prolog_flag(double_quotes, codes).


special_code(Code) :-
    member(Code, "+/-*<>=?").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Grammar

expressions([]) --> [].
expressions([Head | Tail]) -->
    whitespace,
    expression(Head),
    whitespace,
    expressions(Tail),
    !.

expression(symbol(Symbol)) --> symbol(Literal), { atom_codes(Symbol, Literal) }.
expression(number(Number)) --> number(Literal), { number_codes(Number, Literal) }.
expression(List)           --> "(", expressions(List), ")".

whitespace --> [C], { code_type(C, space) }, whitespace.
whitespace --> [].

symbol([Head | Tail]) -->
    [Head],
    { special_code(Head); code_type(Head, alpha) },
    symbol_tail(Tail).

symbol_tail([])            --> [].
symbol_tail([Head | Tail]) -->
    [Head],
    { special_code(Head); code_type(Head, alnum) },
    symbol_tail(Tail).

number([45 | N]) --> "-", numeral(N), !. % 45 = '-'
number(N)         --> numeral(N), !.

numeral([Head | Tail]) --> digits(Head), mantissa(Tail).
numeral([Code])        --> digit(Code).

mantissa([46 | N]) --> ".", digits(Tail). % 46 = '.'

digit(Code) --> [Code], { code_type(Code, digit) }.

digits([Head | Tail]) --> digit(Head), digits(Tail).
digits([Code])        --> digit(Code).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parse Tree -> AST
