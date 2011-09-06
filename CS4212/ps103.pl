/* Benjamin Tan U077129N */

/*Count the number of identifiers.*/

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


get_identifier(S;Ss, InList, OutList) :- 
	get_identifier(S, InList, OutList1), 
	get_identifier(Ss, OutList1, OutList), !.

% Statements with '{ }' 
get_identifier({S}, InList, OutList) :- get_identifier(S, InList, OutList), !.

% Assignments
get_identifier(X=E;, InList, OutList) :- 
	get_identifier(X, InList, OutList1),
	get_identifier(E, OutList1, OutList), !.
	
get_identifier(X=E, InList, OutList) :- 
	get_identifier(X, InList, OutList1),
	get_identifier(E, OutList1, OutList), !.	

% Operators
get_identifier(S, InList, OutList) :- 
	S =.. [F,A,B],
	member(F, [+,-,*,/, >, <, mod, and, or, /\, \/, <<, >>, xor]), !, isExpr(A), isExpr(B),
	get_identifier(A, InList, OutList1),
	get_identifier(B, OutList1, OutList), !.

% while statements
get_identifier(while X do Y;, InList, OutList) :- 
	get_identifier(X, InList, OutList1),
	get_identifier(Y, OutList1, OutList), !. 
	
get_identifier(while X do Y, InList, OutList) :- 
	get_identifier(X, InList, OutList1),
	get_identifier(Y, OutList1, OutList), !.

% if statements
get_identifier(if X then Y;, InList, OutList) :- 
	get_identifier(X, InList, OutList1), 	
	get_identifier(Y, OutList1, OutList), !.
	
get_identifier(if X then Y, InList, OutList) :- 
	get_identifier(X, InList, OutList1), 	
	get_identifier(Y, OutList1, OutList), !.	

get_identifier(if X then Y else Z;, InList, OutList) :- 
	get_identifier(X, InList, OutList1), 	
	get_identifier(Y, OutList1, OutList2), 
	get_identifier(Z, OutList2, OutList), !.

get_identifier(if X then Y else Z, InList, OutList) :- 
	get_identifier(X, InList, OutList1), 
	get_identifier(Y, OutList1, OutList2), 
	get_identifier(Z, OutList2, OutList), !.

% switch statement
get_identifier(switch X of Y;, InList, OutList) :- 
	get_identifier(X, InList, OutList1),
	get_identifier(Y, OutList1, OutList), !.
	
get_identifier(switch X of Y, InList, OutList) :- 
	get_identifier(X, InList, OutList1),
	get_identifier(Y, OutList1, OutList), !.

% case statement

get_identifier(_X :: Y;, InList, OutList) :-
	get_identifier(Y, InList, OutList), !.

get_identifier(_X :: Y, InList, OutList) :-
	get_identifier(Y, InList, OutList), !.


% Base case for a single identifier
get_identifier(X, InList, OutList) :- 
	(\+member(X, InList), identifier(X) -> append(InList, [X], OutList)); append(InList, [], OutList).

% Predicate helpers
isExpr(X) :- X =.. [F,A,B],
	member(F, [+,-,*,/, >, <, mod, and, or, /\, \/, <<, >>, xor]), !, isExpr(A), isExpr(B).
	
isExpr(X) :- identifier(X), ! ; value(X).

identifier(X) :- atom(X),!.
value(X) :- number(X),!.

/*

Code = (
while (x>0) do {
  switch (y+z) of {
0 :: { if (x<10) then { a = 1 ; } else { a = 2; }; }; 1 ::{ if (y\=1) then {a=2; b=3;};}; default :: { a = 0 ; } ;
};
a=a+1; };), get_identifier(Code, [], ResList).

*/