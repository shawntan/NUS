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


simplify(Xs,F) :-
	splitterms(Xs,Syms, Nums),
	sumlist(Nums, Sum),
	groupterms(Syms, Y),
	F = Y + Sum.

splitterms(Xs+T, Syms, [T|Nums]) :- 	number(T), splitterms(Xs, Syms, Nums),!.
splitterms(Xs-T, Syms, [-T|Nums]) :-	number(T), splitterms(Xs, Syms, Nums),!.
splitterms(Xs+N*T, [N*T|Syms], Nums) :-	number(N), splitterms(Xs, Syms, Nums),!.
splitterms(Xs+T, [1*T|Syms], Nums) :-	number(N), splitterms(Xs, Syms, Nums),!.
splitterms(Xs-T, [1*T|Syms], Nums) :-	number(N), splitterms(Xs, Syms, Nums),!.
splitterms(X, [], [X]) :- number(X),!.
splitterms(N*X, [N*X], []).
splitterms(X, [1*X], []).

groupterms([X|Rest],[Final|Grouped]) :-
	countterms(X,Rest,Final,Cleaned),
	groupterms(Cleaned,Grouped).
groupterms([],[]).

countterms(N*X,[M*X|Rest],Final,Cleaned) :-
	number(N),number(M),
	S is N+M,
	countterms(S*X,Rest,Final,Cleaned).
countterms(N*X,[M*X],S*X,[]):-
	number(N),number(M),
	S is N+M.
countterms(X,[Y|Rest],Final,[Y|Cleaned]) :-
	X \= Y,
	countterms(X,Rest,Final,Cleaned).
countterms(X,[Y],X,[Y]) :- X\=Y.

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

