/* Exercise 1 */

p(X) :- r(X).
p(X) :- q(X).

r(X) :- w1(X),!.
r(X) :- w2(X).

q(X) :- w3(X).
q(X) :- w4(X).
q(X) :- w4(a).

w1(a).
w1(b).
w2(c).
w2(d).
w3(e).
w3(f).
w4(g).
w4(h).

/*
	The first rule has the cut operator denoted by the exclamation mark. So this means 
	that when Prolog manages to satisfies a rule using a value for the variable, it will
	ONLY use that option, even if there are other possibilities.
	
	With this in mind, the resolution process will be as such:
	
	1. Prolog will first search for a rule that would make p(Y) true. The first rule it
	   encounters is r(Y), so it adds that to its list of goals.
	
	2. Similarly, to satisfy r(Y), w1(Y) must be true. Therefore, w1(Y) is added to the
	   list of goals.
	
	3. Now, w1(Y) can be satisfied by unifying Y with a. Therefore, prolog backtracks
	   using w1(a) and r(a) and last p(a).  
	
	4. While q(X) might potentially present another set of valid answers, but because of
	   the cut operator, the other possibilities are not considered.

	So what happens when the cut operator is left out?
	
	Simply put, Prolog would consider every possiblity in the database, and is pretty easy to trace
	
	Lastly, what if we add a cut in the third rule?

	This means that Prolog will settle on the first answer it gets. That means when Prolog
	find an answer that satisfies the goal w1(X), it will stop searching for any other 
	answers for r(X). This doesn't stop the goal q(X) from being explored thoroughly.
	
	1. Prolog will first search for a rule that would make p(Y) true. The first rule it
	   encounters is r(Y), so it adds that to its list of goals.

	2. Similarly, to satisfy r(Y), w1(Y) must be true. Therefore, w1(Y) is added to the
	   list of goals.
	
	3. Now, w1(Y) can be satisfied by unifying Y with a. Therefore, prolog backtracks
	   using w1(a) and r(a) and last p(a). Because of the cut operator, Prolog no
	   longer explores the goal r(X). Note that because of this, the fourth goal is 
	   not explored.
	
	4. However, this doesn't stop Prolog from looking at the rest of the goals. 
	
	   So the answer would be Y = a,e,f,g,h
*/

/* Exercise 2 and 3 */
fib(0,0).
fib(1,1).
fib(N,X):-
	N > 0,
	N1 is N-1,
	N2 is N-2,
	fib(N1,A), fib(N2,B),
	X is A+B.

/*
	This code is a direct translation of the recursive definition of calculating 
	the fibonacci series.
	
	To prevent an infinite loop, we first check if our counter, denoted by N is
	larget than 1. Otherwise, it should use the base cases given by the first 
	two rules.
	
	We then decrement the N by 1 and 2 respectively, to calculate the value 
	of a smaller version of the fibonacci number we wanted to calculate in the 
	first place.
	
	The last predicate simply combines the answers of A and B, which completes
	the recursive definition of the fibonacci series.	
*/

fib(0,0).
fib(1,1).

fib(N,X) :- N>1, fib(N, 1, 0, X).
fib(2, A, B, X) :- X is A + B.
fib(N, A, B, X) :- 
	N>2,
	N1 is N-1,
	M is A+B,
	fib(N1, M, A, X).	
	
/*
	The problem with the first definition is that it makes too many unneeded 
	recrusive calls, that results in exponential time complexity.
	
	A way to achieve O(n) complexity is to convert the recursive definition is
	to convert it into a tail recursive one. This means that only one recursive call
	is made, and that is the only operation that should occur. That means that no
	other operation can be performed on the recursive definition.
	
	Tail recursion similates iteration, something which Prolog doesn't provide.
	
	Take a look at the first rule. The left hand side shows the call to fib, but the
	right hand side serves as an auxillary function in imperative languages. In this
	case, 1 serves as an accumulator, while B is like the temperory variable.
	
	The second rule is like the base case of the whole iteration. Think of it like
	the condition where N == 2, where we can then halt the iteration.
	
	The last rule handles the case where N is larger than 2. In this case, we decrement
	the counter N, combine the intermediate values stored in A and B into M, 
	and then we call the function again with the new values.
*/	

fib(N,X) :- N>1, fib(N, 1, 0, X).
fib(2, A, B, X) :- X is A + B,!.
fib(N, A, B, X) :- 
	N1 is N-1,
	M is A+B,
	fib(N1,M, A, X).		

/*
	To see how cuts can be used to improve the program, it is helpful to see what 
	is wrong with the program in the first place.
	
	Turn trace on, and at the end of the trace, it becomes obvious that the Prolog
	checks that 2>2. But then notice that we already have a rule for that in the second line.
	
	To exploit the cut operator, we can modify the second rule, and remove the check in the 
	third rule.
	
	In this case, Prolog doesn't bother to check once it has a value of X.
*/

/* Exercise 4 */
:- op(1099,yf,;).
:- op(960,fx,while).
:- op(959,xfx,do).
:- op(960,fx,for).

transform( for(S1;S2;S3) do {S4;}, S1t; while(S2t) do {S4t; S3t} ) :-
	!,transform(S1,S1t), transform(S2,S2t), transform(S3,S3t), transform(S4,S4t).
	
transform(S,S).