:- op(1099,yf,;).
:- op(960,fx,while).
:- op(959,xfx,do).
:- op(960,fx,if).
:- op(959,xfx,then).
:- op(961,xfx,else).
:- op(960,fx,for).


statement(S;Ss) :- statement(S;), rest_statements(Ss),!.
statement({S}) :- writeln('{'), statement(S), writeln('}').

statement(while X do Y;) :- 
	write('while '), write('('), write(X), write(') '), statement(Y), !.
	
statement(if X then Y;) :- 
	write('if '), write('('), write(X), write(') '), statement(Y), !.	
	
statement(if X then Y else Z;) :- 
	write('if '), write('('), write(X), write(') '), statement(Y), write('else '), statement(Z), !.


statement(S;) :- writeln(S;).


rest_statements(S;Ss) :- statement(S), rest_statements(Ss).
rest_statements(S) :- statement(S).
rest_statements :- true.


/*
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
break ; }
a = a+1 ; }

Test Cases for if:
statement( if (z<0) then { x=a; } else { b=a; }; ).


Test Cases for while:

statement( while (a<0) do { b=a; }; ).
statement( while (a<0) do { b=a; a=c; b=d; }; ).
statement( while (a<0) do { while (a<0) do { b=a; }; a=c; b=d; }; ).
statement( while (a<0) do { while (a<0) do { while (a<0) do { b=a; }; }; while (a<0) do { b=a; }; while (a<0) do { b=a; }; }; ).
statement( a=b; while (a<0) do { while (a<0) do { while (a<0) do { b=a; }; }; while (a<0) do { b=a; }; while (a<0) do { b=a; }; }; ).
statement( a=b; while (a<0) do { while (a<0) do { b=a; }; a=c; b=d; }; a=b; b=c; while (a<0) do { b=a; }; b=c;).


*/