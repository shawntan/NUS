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





/*get_identifer(S;Ss, List) :- 
	get_identifer(S, L1), get_identifer(Ss, L2), append(L1, L2, List).

get_identifer(S;, List) :- 
	get_identifer(S, L1), append(List, L1, List).

get_identifer({S}, List) :- 
	get_identifer(S, List).*/


get_identifer(S;Ss, InList, OutList) :- 
	get_identifer(S, InList, OutList1), 
	get_identifer(Ss, OutList1, OutList).

get_identifer(X=E;, InList, OutList) :- 
	identifier(X), !, 
	append(InList, [X], OutList1),
	get_identifer(E, OutList1, OutList).
	
get_identifer(X=E, InList, OutList) :- 
	identifier(X), !, 
	append(InList, [X], OutList1),
	get_identifer(E, OutList1, OutList).	

% Base case for a single identifier
get_identifer(X, InList, OutList) :- 
	identifier(X), !, append(InList, [X], OutList), !.







count_assign(while _X do Y;, N) :- count_assign(Y, N), !.
count_assign(while _X do Y, N) :- count_assign(Y, N), !.
	
count_assign(if _X then Y;, N) :- count_assign(Y, N), !.
count_assign(if _X then Y, N) :- count_assign(Y, N), !.

count_assign(if _X then Y else Z;, N) :- count_assign(Y, N1), count_assign(Z, N2), N is N1+N2, !.
count_assign(if _X then Y else Z, N) :- count_assign(Y, N1), count_assign(Z, N2), N is N1+N2, !.

count_assign(switch _X of Y;, N) :- count_assign(Y, N).
count_assign(switch _X of Y, N) :- count_assign(Y, N).

count_assign(_X :: Y;, N) :- count_assign(Y, N), !.
count_assign(_X :: Y, N) :- count_assign(Y, N), !.

count_assign(_;,0) :- !.

isAssign(X=E) :- identifier(X), isExpr(E), !.

isExpr(X) :- X =.. [F,A,B],
	member(F, [+,-,*,/, mod, and, or, /\, \/, <<, >>, xor]), !, isExpr(A), isExpr(B).
	
isExpr(X) :- identifier(X), ! ; value(X).

identifier(X) :- atom(X),!.
value(X) :- number(X),!.




/*

count_assign(
while (x>0) do {
  switch (y+z) of {
0 :: { if (x<10) then { a = 1 ; } else { a = 2; }; }; 1 ::{ if (y\=1) then {a=2; b=3;};}; default :: { a = 0 ; } ;
};
a=a+1; };
, N).

*/