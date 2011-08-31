:- op(1099,yf,;).
:- op(960,fx,while).
:- op(959,xfx,do).
:- op(960,fx,for).

while(do(S)) :- while(S).

transform( while(S1) do {S2;}, while(S1t) do {S2t;} ) :-
	!,transform(S1,S1t), transform(S2,S2t), while(do(S2t)).

transform(S,S).

/*
	while(do(S)) :- while(S).
*/


/*
while (x>0) do {
  switch (y+z) of {
0 :: { if (x<10) then { a = 1 ; } else { a = 2; }; }; 1 ::{if(y\=1)then{a=2; b=3;};}; default :: { a = 0 ; } ;
};
a=a+1; };

*/

/*while (x>0) {
   switch (y+z) {
   case 0 : { if (x<0) { a = 1 ; } else { a = 2 ; } }
            break ;
   case 1 : { if (y!=1) { a = 2 ; b = 3 ; } }
            break ;
   default: { a = 0 ; }
break ; }
a = a+1 ; }*/