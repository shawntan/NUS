% Benjamin Tan Wei Hao
% U077129N

/*
	Problem 1 [2 marks]
	
	Write a translator from the toy language defined in Lecture 1, that outputs correct C code
	fragments for the given code.

*/

:- op(1099,yf,;).
:- op(960,fx,while).
:- op(959,xfx,do).
:- op(960,fx,if).
:- op(959,xfx,then).
:- op(961,xfx,else).
:- op(960,fx,for).
:- op(960,fx,switch).
:- op(959,xfx,of).
:- op(959,xfx,::).
:- op(959,xfx,=).

% Statements are composed of a single statement (terminated by a semi colon), and
% other statements.
statement(S;Ss) :- statement(S), rest_statements(Ss),!.

% remove redundant braces, by `absorbing` it in the head of the rule.
statement({S;}) :- nl, statement(S;), !.
statement({S;Ss}) :- writeln(' {' ), statement(S;Ss), writeln('}' ).

% while statements
statement(while X do Y;) :- 
	write('while '), isExpr(X), statement(Y), !.

statement(while X do Y) :- 
	write('while '), isExpr(X), statement(Y), !.
	
% if statements 	
statement(if X then Y;) :- 
	write('if '), isExpr(X), statement(Y), !.	

statement(if X then Y) :- 
	write('if '), isExpr(X), statement(Y), !.

statement(if X then Y else Z;) :- 
	write('if '), isExpr(X), statement(Y), write('else '), statement(Z), !.

statement(if X then Y else Z) :- 
	write('if '), isExpr(X), statement(Y), write('else '), statement(Z), !.

% switch statements
statement(switch X of Y;) :- write('switch '), isExpr(X),  statement(Y).
statement(switch X of Y) :- write('switch '), isExpr(X),  statement(Y).

% case statements. Constructing the predicates this way 'absorbs' the curly braces, 
% effectively removing it.
statement(X :: {Y};) :-
	write('case '), write(X), writeln(' :' ), statement(Y), writeln('break ;'), !.

statement(X :: {Y}) :-
	write('case '), write(X), write(' : ' ), statement(Y), writeln('break ;'), !.


statement(S;) :- writeln(S;).
statement(S)  :- writeln(S;). 

% helper predicates
% special case is handling the '\=' operator and transforming it into '!=' instead.
isExpr(X) :- 
	X =.. [F,A,B], 
	(member(F, [+,-,*,/, >, <, >=, \=, =<, mod, and, or, /\, \/, <<, >>, xor]), !, isExpr(A), isExpr(B), 
	write('('), write(A), 
	(F == \= -> write('!=') ; write(F)), 
	write(B), write(')')).
	
isExpr(X) :- identifier(X), ! ; value(X).

identifier(X) :- atom(X),!.
value(X) :- number(X),!.


rest_statements(S;Ss) :- statement(S;), rest_statements(Ss).
rest_statements(S;) :- statement(S;).

rest_statements(S)    :- statement(S).
rest_statements       :- true.


/*

Example invocation:

Code = (
	while (x<1) do { y=2; };
	while (x>0) do {
	  switch (y+z) of {
		0 :: { if (x<10) then { a = 1 ; if (y\=1) then {a=2; b=3; if (y\=1) then {a=2; b=3;};}; if (y\=1) then {a=2; b=3;}; } else { if (y\=1) then {a=2; b=3;}; a = 2; }; }; 
		1 :: { if (y\=1) then {a=2; b=3;}; while (x>1) do {yah=2;};}; 
		default :: { a = 0 ; } ;
	};
	a=a+1; };
),
statement( Code ).

*/