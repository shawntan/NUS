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
	
statement(if X then Y else Z;) :- 
	write('if '), write('('), statement(X), write(') '), statement(Y), write('else '), statement(Z), !.

statement(switch X of Y;) :- write('switch '), statement(X), statement(Y).

statement(X :: Y;) :-
	write('case '), write(X), write(' : ' ), statement(Y), writeln('break ;'), !.

statement(X :: Y) :-
	write('case '), write(X), write(' : ' ), statement(Y), writeln('break ;'), !.

statement(X\=Y)  :- write(X), write('!='), write(Y).
statement(X\=Y;) :- write(X), write('!='), writeln(Y;).

statement(S;) :- writeln(S;).
statement(S)  :- write(S). 


if_statement(if _ then Y;) :- [Y]. 

assignment(_=_).

rest_statements(S;Ss) :- statement(S), rest_statements(Ss).
rest_statements(S)    :- statement(S).
rest_statements       :- true.


count_assign(S;Ss, N)   :- 
	(assignment(S) -> N1 is N+1, count_assign(Ss, N1), ! ; count_assign(Ss, N)).

count_assign({S;Ss}, N) :- (assignment(S) -> N1 is N+1, count_assign(Ss, N1), ! ; count_assign(Ss, N)).
count_assign({S;}, N)   :- assignment(S), N1 is N+1, writeln(N1).
count_assign(S;, N)     :- assignment(S), N1 is N+1, writeln(N1).



/*

Cast Statement:

statement( 0 :: { b=a; }; 1 :: { b=c; }; ).


while (x>0) do {
  switch (y+z) of {
0 :: { if (x<10) then { a = 1 ; } else { a = 2; }; }; 1 ::{if(y\=1)then{a=2; b=3;};}; default :: { a = 0 ; } ;
};
a=a+1; };


while (x>0) {
   switch (y+z) {
   case 0 : { if (x<0) { a = 1 ; } else { a = 2 ; } }
            break ;
   case 1 : { if (y!=1) { a = 2 ; b = 3 ; } }
            break ;
   default: { a = 0 ; }
			break ; 
   }
   a = a+1 ; }

Test Cases for overall:

statement(
while (x>0) do {
  switch (y+z) of {
0 :: { if (x<10) then { a = 1 ; } else { a = 2; }; }; 1 ::{ if (y\=1) then {a=2; b=3;};}; default :: { a = 0 ; } ;
};
a=a+1; };
).



Test Cases for case:

statement( 0 :: { if (x<10) then { a = 1 ; } else { a = 2; }; }; ).
statement( 0 :: { b=a; }; 1 :: { b=c; }; 2:: { z=b; }; ).
statement( default :: { if (x<10) then { a = 1 ; } else { a = 2; }; }; ).
statement( 0 :: { a=b; }; 1 ::{ b=3; }; 2 :: { a=0; }; ).



Test Cases for if:

statement( if (z\=0) then { x=a; while (a\=0) do { b=a; if (z\=0) then { x=a; } else { b\=a; };}; }; ).


Test Cases for while:

statement( while (a\=0) do { b=a; }; ).
statement( while (a<0) do { b=a; a=c; b=d; }; ).
statement( while (a<0) do { while (a<0) do { b=a; }; a=c; b=d; }; ).
statement( while (a<0) do { while (a<0) do { while (a<0) do { b=a; }; }; while (a<0) do { b=a; }; while (a<0) do { b=a; }; }; ).
statement( a=b; while (a<0) do { while (a<0) do { while (a<0) do { b=a; }; }; while (a<0) do { b=a; }; while (a<0) do { b=a; }; }; ).
statement( a=b; while (a<0) do { while (a<0) do { b=a; }; a=c; b=d; }; a=b; b=c; while (a<0) do { b=a; }; b=c;).


*/