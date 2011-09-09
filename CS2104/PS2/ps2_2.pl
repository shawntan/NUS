% Problem Set 2, Exercise 2

/*derive((x+2)*(a-x),x,D).*/

derive(Expr, Var, Result) :-
	Expr =.. [F,A,B],
	F = *,
	derive(B, Var, Result1),
	derive(A, Var, Result2), !,
	Result = (A*Result1+B*Result2).
	
/*	write(A), write(*), write(Result1), write(+), write(B), write(*), writeln(Result2).	*/
/*	Result is (A) * Result1 + (B) * Result2, !.*/


% k*x
derive(Expr, Var, Result) :-
	Expr =.. [F,A,B],
	F = *,
	B = Var,
	Result is A, !.

% _+_
derive(Expr, Var, Result) :-
	Expr =.. [F,A,B],
	F = +,
	derive(A, Var, Result1),
	derive(B, Var, Result2), !,
	Result = (Result1+Result2).

% _-_
derive(Expr, Var, Result) :-
		Expr =.. [F,A,B],
		F = -,
		derive(A, Var, Result1),
		derive(B, Var, Result2), !,
		Result = (Result1-Result2).

% Constants/Base Cases
derive(Var, Var, Result) :- Result is 1, !.
derive(Expr, _, Result) :- number(Expr), Result is 0, !.
derive(_, _, 0).