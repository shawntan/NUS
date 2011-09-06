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
	write('while '), write('('), statement(X), write(') '), statement(Y), !.
	
statement(if X then Y;) :- 
	write('if '), write('('), statement(X), write(') '), statement(Y), !.	
	
statement(if X then Y else Z;) :- 
	write('if '), write('('), statement(X), write(') '), statement(Y), write('else '), statement(Z), !.

statement(switch X of Y;) :- write('switch '), statement(X), statement(Y).

statement(X :: Y;) :-
	write('case '), write(X), write(' : ' ), statement(Y), writeln('break ;'), !.

statement(X :: Y) :-
	write('case '), write(X), write(' : ' ), statement(Y), writeln('break ;'), !.

statement(X\=Y)  :- write(X), write('!='), write(Y).
statement(X\=Y;) :- write(X), write('!='), writeln(Y;).

statement(S;) :- assignment_statement(S;); writeln(S;).
statement(S)  :- assignment_statement(S); write(S). 

rest_statements(S;Ss) :- statement(S), rest_statements(Ss).
rest_statements(S)    :- statement(S).
rest_statements       :- true.


assignment_statement(X=Y;) :- 
	((identifier(X), value(Y)) ; (identifier(X), identifier(Y))),
	count(N), M is N+1, retractall(count(_)), assert(count(M)), !.

identifier(X) :- atom(X),!.
value(X) :- number(X),!.

count_assignment_statement(X) :- retractall(count(_)), assert(count(0)), statement(X).



/*

count_assignment_statement(
while (x>0) do {
  switch (y+z) of {
0 :: { if (x<10) then { a = 1 ; } else { a = 2; }; }; 1 ::{ if (y\=1) then {a=2; b=3;};}; default :: { a = 0 ; } ;
};
a=a+1; };
).

*/
