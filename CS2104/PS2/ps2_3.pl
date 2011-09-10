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
% Write a Prolog program that performs arithmetic expression simplification. 

% Your program should at least eliminate multiplications by 0 and 1, 
% and additions with 0. 
simplify(X+Y, R) :-
	(number(R1), number(R2) -> R is R1+R2, !; 
	
	(
		atom(X), number(Y) -> 
			simplify(Y, R1), simplify(X, R2); 
			simplify(X, R1), simplify(Y, R2)
	), R = R1+R2,!).

simplify(X-Y, R) :-
	simplify(X, R1),
	simplify(Y, R2),
	(number(R1), number(R2) -> R is R1-R2, !; R = R1-R2, !).




/* simplify(X*Y, R) :- (atom(X),number(Y)) -> simplify(Y*X, R), !.*/
	
simplify(X*Y, R) :-
	(X is 0, R = 0), !;
	(X is 1, R = Y), !;
	((number(X),number(Y)) -> R is X*Y; R = X*Y, !);
	((isChar(X),isChar(Y)), R = X*Y; !).
	



simplify(X-Y, R) :- number(X), number(Y), R is X-Y.
simplify(X/Y, R) :- number(X), number(Y), R is X/Y.
simplify(X, R) :- number(X), R is X, !.
simplify(X, R) :- atom(X), R = X, !.




% Ideally, it should also convert any subexpression containing only constants 
% into the value of that expression, and convert multiplications with -1 into 
% the negative of the multiplicand. 
isChar(Expr) :- atom(Expr) ; number(Expr). 
isExpr(Expr) :- \+(atom(Expr);number(Expr)).














