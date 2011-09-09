% Problem Set 2, Exercise 3

/*derive((x+2)*(a-x),x,D).*/
% 1+0)*(a-x)+(x+2)*(0-1)

derive(Expr, Var, Result) :-
	Expr =.. [F,A,B],
	F = *,
	derive(B, Var, Result1),
	derive(A, Var, Result2), !,
	Result = ((Result2*B)+(A*Result1)).

% x/y
derive(Expr, Var, Result) :-
	Expr =.. [F,A,B],
	F = /,
	derive(B, Var, Result1),
	derive(A, Var, Result2), !,
	Result = ((Result2*B)-(A*Result1))/(B*B).

% k*x
derive(Expr, Var, Result) :-
	Expr =.. [F,A,B],
	F = *, !,
	B = Var,
	Result is A, !.

% _+_
derive(Expr, Var, Result) :-
	Expr =.. [F,A,B],
	F = +, !,
	derive(A, Var, Result1),
	derive(B, Var, Result2), !,
	Result = (Result1+Result2).

% _-_
derive(Expr, Var, Result) :-
		Expr =.. [F,A,B],
		F = -, !,
		derive(A, Var, Result1),
		derive(B, Var, Result2), !,
		Result = (Result1-Result2).
			
% Constants/Base Cases
derive(Var, Var, Result) :- Result is 1, !.
derive(Expr, _, Result) :- number(Expr), Result is 0, !.
derive(_, _, 0).


% Simplify
Write a Prolog program that performs arithmetic expression simplification. 

% Your program should at least eliminate multiplications by 0 and 1, 
% and additions with 0. 
simplify(X+Y, R) :- number(X), number(Y), R is X+Y.
simplify(X*Y, R) :- number(X), number(Y), R is X*Y.
simplify(X-Y, R) :- number(X), number(Y), R is X-Y.
simplify(X/Y, R) :- number(X), number(Y), R is X/Y.

% Ideally, it should also convert any subexpression containing only constants 
% into the value of that expression, and convert multiplications with -1 into 
% the negative of the multiplicand. 















