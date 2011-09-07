/* Benjamin Tan U077129N */

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
:- dynamic count/1.

count(0).

statement(S;Ss) :- statement(S;), rest_statements(Ss), !, count(X), writeln(X).
statement({S}) :- writeln('{'), statement(S), writeln('}').

statement(while X do Y;) :- 
	statement(X), statement(Y), !.
	
statement(if X then Y;) :- 
	statement(X), statement(Y), !.	
	
statement(if X then Y else Z;) :- 
	statement(X), statement(Y), statement(Z), !.

statement(switch X of Y;) :- statement(X), statement(Y).

statement(X :: Y;) :- identifier(X), statement(Y), !.
statement(X :: Y) :- identifier(X), statement(Y), !.


statement(X\=Y;) :- identifier(X), identifier(Y).
statement(X=Y)  :- identifier(X), identifier(Y).
statement(X\=Y) :- identifier(X), identifier(Y).
statement(X<Y)  :- identifier(X), identifier(Y).
statement(X>Y)  :- identifier(X), identifier(Y).
statement(X<Y)  :- identifier(X), identifier(Y).
statement(X=<Y) :- identifier(X), identifier(Y).
statement(X>=Y) :- identifier(X), identifier(Y).
statement(X)    :- identifier(X).
statement(S;) :- assignment(S;); writeln(S;).
statement(S)  :- assignment(S); write(S). 

assignment(_=_).


rest_statements(S;Ss) :- statement(S), rest_statements(Ss).
rest_statements(S)    :- statement(S).
rest_statements       :- true.

identifier(X) :- atom(X), count(N), N1 is N+1, retractall(count(_)), assert(count(N1)), !.


	


/*

statement(
while (x>0) do {
  switch (y+z) of {
0 :: { if (x<10) then { a = 1 ; } else { a = 2; }; }; 1 ::{ if (y\=1) then {a=2; b=3;};}; default :: { a = 0 ; } ;
};
a=a+1; };
).

*/
