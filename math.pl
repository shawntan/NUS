derive(X + Xs, x, A + B) :- 
	derive(X , x, A),
	derive(Xs, x, B),!.
derive(X - Xs, x, A -B) :- 
	derive(X , x, A),
	derive(Xs, x, B),!.
derive(X * Xs, x, A * Xs + X * B) :-
	derive(X , x, A),
	derive(Xs, x, B),!.

derive(X/Xs, x, (A * Xs - X * B)/(Xs * Xs)):- 
	derive(X, x, A),
	derive(Xs, x, B),!.

derive(N*x,x,N) :- number(N),!.
derive(N, x, 0) :- number(N),!.
derive(N, x, 0) :- atom(N), N\=x,!.
derive(x, x, 1).


simplify(E,F) :-
	E =.. [Op, La, Ra],
	simplify(La, X),
	simplify(Ra, Y),
	s(Op, X, Y, F),!.
simplify(E,E) :- atom(E),!.
simplify(N,N) :- number(N),!.


s(+, N, M+Xs, S+Xs) :- number(N),number(M),S is N+M,!.
s(+, N, Xs+M, S+Xs) :- number(N),number(M),S is N+M,!.
s(*, N, M*Xs, S*Xs) :- number(N),number(M),S is N*M,!.
s(*, N, Xs*M, S*Xs) :- number(N),number(M),S is N*M,!.


s(+, X, N, N+X) :- number(N).
s(*, N, X, N*X) :- number(N).
s(*, X, N, N*X) :- number(N).


s(+, X, 0, X).
s(+, 0, X, X).
s(+, X, X, 2*X).
s(+, X, Y, X+Y).
s(-, X, 0, X).
s(-, 0, X, X).
s(-, X, X, 0).
s(-, X, Y, X-Y).
s(*, _, 0, 0).
s(*, 0, _, 0).
s(*, 1, X, X).
s(*, X, 1, X).
s(*, X, Y, X*Y).
s(/, 0, _, 0).
s(/, X, 1, X).
s(/, X, X, 1).
s(/, X, Y, X/Y).





count(Expr, Op, C) :- 
	Expr =.. [Operator, LeftOp, RightOp],
	Operator = Op,
	count(LeftOp, Op, C1),
	count(RightOp, Op, C2),
	C is C1 + C2 + 1,!.
count(Expr, Op, C) :- 
	Expr =.. [Operator, LeftOp, RightOp],
	Operator \= Op,
	count(LeftOp, Op, C1),
	count(RightOp, Op, C2),
	C is C1 + C2,!.
count(Expr, Op,C) :- 
	Expr =.. [LeftOp,RightOp],
	count(LeftOp, Op, C1),
	count(RightOp, Op, C2),
	C is C1 + C2,!.
count(Expr, _, 0):- (number(Expr);atom(Expr)),!.


