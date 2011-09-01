:- op(1099,yf,;).
:- op(960,fx,while).
:- op(959,xfx,do).
:- op(960,fx,for).


statement(S;Ss) :- statement(S;), rest_statements(Ss),!.
statement({S}) :- writeln('{'), statement(S), writeln('}').
statement(while X do Y;) :- write('while '), write('('), write(X) ,write(') '), statement(Y), !.
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
a = a+1 ; }*/