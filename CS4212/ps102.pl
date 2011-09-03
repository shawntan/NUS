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


count_assign(S;Ss, N) :- 
	count_assign(S;, N1), count_assign(Ss, N2), N is N1+N2.

count_assign({S};, N) :- count_assign(S, N).
	
count_assign({S}, N) :- count_assign(S, N).
count_assign(S, N) :- 
	(assignment_statement(S) -> N1 is N+1, writeln(N1));
	(while_statement(S, N)).

while_statement(while _ do Y;, N) :- count_assign(Y, N).
	
/*if_then_statement(if X then Y).

	
if_then_else_statement(if X then Y else Z;, N).
switch_statement(X :: Y;, N).
switch_statement(X :: Y, N).*/

assignment_statement(X=Y;) :- (identifier(X), value(Y)) ; (identifier(X), identifier(Y)).

identifier(X) :- atom(X),!.
value(X) :- number(X),!.

