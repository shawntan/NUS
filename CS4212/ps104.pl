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


statement(S;Ss) :- statement(S;), rest_statements(Ss),!.
statement({S}) :- writeln('{'), statement(S), writeln('}').

statement(while X do Y;) :- 
	write('while '), write('('), statement(X), write(') '), statement(Y), !.
	
statement(if X then Y;) :- 
	write('if '), write('('), statement(X), write(') '), statement(Y), !.	

/* Copy the first if statement, the flip the condition of the `else` part of the if statement */	
statement(if X then Y else Z;) :- 
	write('if '), write('('), statement(X), write(') '), statement(Y),
	write('if '), write('('), invert_condition(X), write(') '), statement(Z), !.

statement(switch X of Y;) :- write('switch '), statement(X), statement(Y).

statement(X :: Y;) :-
	write('case '), write(X), write(' : ' ), statement(Y), writeln('break ;'), !.

statement(X :: Y) :-
	write('case '), write(X), write(' : ' ), statement(Y), writeln('break ;'), !.

statement(X\=Y)  :- write(X), write('!='), write(Y).
statement(X\=Y;) :- write(X), write('!='), writeln(Y;).

statement(S;) :- writeln(S;).
statement(S)  :- write(S). 


assignment(_=_).

rest_statements(S;Ss) :- statement(S), rest_statements(Ss).
rest_statements(S)    :- statement(S).
rest_statements       :- true.

/* Invertion predicated needed to flip the various conditions */

invert_condition(X=Y)  :- write(X),write('!='),write(Y).
invert_condition(X\=Y) :- write(X),write('='),write(Y).
invert_condition(X<Y)  :- write(X),write('>='),write(Y).
invert_condition(X>Y)  :- write(X),write('<='),write(Y).
invert_condition(X<Y)  :- write(X),write('>='),write(Y).
invert_condition(X=<Y) :- write(X),write('>'),write(Y).
invert_condition(X>=Y) :- write(X),write('<'),write(Y).

