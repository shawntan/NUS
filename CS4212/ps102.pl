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


count_assign(X=E;Ss, N) :- !, count_assign(Ss, N1), (isAssign(X=E) -> N is N1+1 ; N is N1).
count_assign(S;Ss, N) :- count_assign(S, N1), count_assign(Ss, N2), N is N1+N2.
count_assign(X=E;, N) :- !, (isAssign(X=E) -> N is 1 ; N is 0).
count_assign({S}, N) :- count_assign(S, N).

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



isAssign(X=E) :- 
	((identifier(X), value(E)) ; (identifier(X), identifier(E))), !.



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
