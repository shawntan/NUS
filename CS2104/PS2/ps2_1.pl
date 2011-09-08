% Benjamin Tan Wei Hao
% U077129N

/*
	Problem 1 [2 mark]
	
	Write a Prolog predicate definition that counts the number of occurrences of an operator
	inside an expression.
*/

count(Expr, Op, C) :-
	(
		(
			Expr =.. [F, ExprA, ExprB],	
		 	count(ExprA, Op, C1),
		 	count(ExprB, Op, C2)
		);
		(
			Expr =.. [F, ExprA],	
			count(ExprA, Op, C1),
			count(F, Op, C2)
		)
	),
	count(F, Op, C3),
	C is C1+C2+C3, !.

count(F, Op, 1) :- F = Op, member(F,[+,-,/,*,^]).
count(F, Op, 0) :- F \= Op.
