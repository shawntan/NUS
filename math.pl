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


simplify(E,E) :- atom(E),!.
simplify(N,N) :- number(N),!.

simplify(E,F) :-
	E =.. [Op, La, Ra],
	simplify(La, X),
	simplify(Ra, Y),
	s(Op, X, Y, F).

s(+, X, 0, X).
s(+, 0, X, X).
s(-, X, 0, X).

s(+, N, M, S) :- number(N), number(M), S is N+M.
s(+, X, X, 2*X).
s(+, N*X, X, M*X) :- number(N), M is N+1.
s(+, X, N*X, M*X) :- number(N), M is N+1.
s(+, N*X,M*X, S*X) :- number(N),number(M),S is N+M.
s(+, X, Y, X+Y).

s(*, _, 0, 0).
s(*, 0, _, 0).
s(*, 1, X, X).
s(*, X, 1, X).
s(*, X, N, N * X) :- number(N).
s(*, N,M*X, P*X) :- number(N),number(M),P is M*N.
s(*, M*X,N, P*X) :- number(N),number(M),P is M*N.
s(*, N*X,M*Y, P*X*Y) :- number(N),number(M),P is M*N.
s(*, X, Y, X * Y).


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


