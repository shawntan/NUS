% Benjamin Tan Wei Hao
% U077129N

/*
	Problem 4 [1 mark]
	
	Suppose we have the following restriction to our toy language: 
	else branches are no longer allowed. 
	In other words, if statements are restricted to having only a then branch. 
	Write a Prolog program that converts a program of the toy language into a 
	program of the restricted toy language, as defined above.
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

% Statements are composed of a single statement (terminated by a semi colon), and other statements.
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


% modified if statement: translate else branch into if

% Copy the first if statement, then flip the condition of the `else` part of the if statement 	
statement(if X then Y else Z;) :- 
	write('if '), isExpr(X), statement(Y), 
	write('if '), invert_condition(X), statement(Z), !.

statement(if X then Y else Z) :- 
	write('if '), isExpr(X), statement(Y), 
	write('if '), invert_condition(X), statement(Z), !.

% switch statements
statement(switch X of Y;) :- write('switch '), isExpr(X),  statement(Y).
statement(switch X of Y) :- write('switch '), isExpr(X),  statement(Y).

% case statements. Constructing the predicates this way 'absorbs' the curly braces, effectively removing it.
statement(X :: {Y};) :-
	write('case '), write(X), writeln(' :' ), statement(Y), writeln('break ;'), !.

statement(X :: {Y}) :-
	write('case '), write(X), writeln(' : ' ), statement(Y), writeln('break ;'), !.


statement(S;) :- writeln(S;).
statement(S)  :- writeln(S;). 

% helper predicates. Special case for handling the '\=' operator and transforming it into '!=' instead.
isExpr(X) :- 
	X =.. [F,A,B], 
	(member(F, [+,-,*,/, >, <, >=, \=, =, =<, mod, and, or, /\, \/, <<, >>, xor]), !, isExpr(A), isExpr(B), 
	write('('), write(A), 
	(F == \= -> write('!=') ; write(F)), 
	write(B), write(')')).
	
isExpr(X) :- identifier(X), ! ; value(X).

identifier(X) :- atom(X),!.
value(X) :- number(X),!.

% rest of the statements.
rest_statements(S;Ss) :- statement(S;), rest_statements(Ss).
rest_statements(S;) :- statement(S;).

rest_statements(S)    :- statement(S).
rest_statements       :- true.

%Invertion predicates needed to flip the various conditions 
invert_condition(X) :- 
	X =.. [F,A,B], 
	(member(F, [>, <, >=, \=, =, =<]), !, isExpr(A), isExpr(B), 
	write('('), write(A), 
	(
		F == \= -> write('=='), ! ;
		F ==  = -> write('!='), ! ;
		F ==  < -> write('>='), ! ;
		F ==  > -> write('<='), ! ;
		F == =< -> write('>'), ! ;
		F == >= -> write('<'), !
	), 
	write(B), write(')')).

/*
	Example invocation:
	
	Code = (
		while (x<1) do { y=2; };
		while (x>0) do {
	  		switch (y+z) of {
				0 :: { if (x<10) then { a = 1 ; if (y\=1) then {a=2; b=3; if (y\=1) then {a=2; b=3;};}; if (y\=1) then {a=2; b=3;}; } else { if (y\=1) then {a=2; b=3;}; a = 2; }; }; 
				1 :: { if (y\=1) then {a=2; b=3;}; while (x>1) do { hello=2;};}; 
				default :: { a = 0 ; } ;
			};
			a=a+1; 
		};
	),
	statement( Code ).
	
*/

