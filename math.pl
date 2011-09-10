derive(X + Xs, x, Y) :- 
	derive(X , x, A),
	derive(Xs, x, B),
	simplify(A + B,Y).
derive(X - Xs, x, A - B) :- 
	derive(X , x, A),
	derive(Xs, x, B).
derive(X * Xs, x, A * Xs + X * B) :-
	derive(X , x, A),
	derive(Xs, x, B).
derive(X/Xs, x,(A*Xs-X*B)/(Xs*Xs)) :- 
	derive(X, x, A),
	derive(Xs, x, B).
derive(N*x,x,N).
derive(x, x, 1).
derive(N, x, 0).
derive(y, x, 0) :- atom(y),y\=x.


simplify(E,E) :- atom(E),!.
simplify(N,N) :- number(N),!.

simplify(E,F) :-
	E =.. [Op, La, Ra],
	simplify(La, X),
	simplify(Ra, Y),
	s(Op, X, Y, F).

s(+, X, 0, X).
s(+, 0, X, X).
s(+, N, M, S) :- number(N), number(M), S is N+M.
s(+, X, X, 2*X).
s(+, N*X, X, M*X) :- number(N), M is N+1.
s(+, X, N*X, M*X) :- number(N), M is N+1.
s(+, X, X, 2*X).
s(+, N*X,M*X, S*X) :- number(N),number(M),S is N+M.
s(+, N*X,M*X, (N+M)*X).
s(+, X, Y, X+Y).

s(*, _, 0, 0).
s(*, 0, _, 0).
s(*, 1, X, X).
s(*, X, 1, X).

s(*, X, N, N * X) :- number(N).
s(*, X, Y, X * Y).

