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
	F =  Xs + Sum.

splitterms(Xs+T, Syms, [T|Nums]) :- 	number(T), splitterms(Xs, Syms, Nums),!.
splitterms(T+Xs, Syms, [T|Nums]) :- 	number(T), splitterms(Xs, Syms, Nums),!.
splitterms(T-Xs, Syms, [T|Nums]) :- 	number(T), splitterms(Xs, Syms, Nums),!.
splitterms(Xs-T, Syms, [-T|Nums]) :-	number(T), splitterms(Xs, Syms, Nums),!.
splitterms(Xs+N*T, [N*T|Syms], Nums) :-	number(N), splitterms(Xs, Syms, Nums),!.
splitterms(Xs+T, [1*T|Syms], Nums) :-	number(N), splitterms(Xs, Syms, Nums),!.
splitterms(Xs-T, [-1*T|Syms], Nums) :-	number(N), splitterms(Xs, Syms, Nums),!.
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



/*Parse Tree rotation and partial expansion.*/
simplify_terms(L+(R1+R2), Result) :-
	simplify_terms(L+R1,LResult),simplify_terms(R2,RResult),
	simplify_terms(LResult+RResult,Result).
simplify_terms(L+(R1-R2), Result) :-
	simplify_terms(L+R1,LResult),simplify_terms(-R2,RResult),
	simplify_terms(LResult+RResult,Result).
simplify_terms(L-(R1+R2), Result) :-
	simplify_terms(L-R1,LResult),simplify_terms(-R2,RResult),
	simplify_terms(LResult+RResult,Result).
simplify_terms(L-(R1-R2), Result) :-
	simplify_terms(L-R1,LResult),simplify_terms(R2,RResult),
	simplify_terms(LResult+RResult,Result).
simplify_terms(-(L-R), Result) :-
	simplify_terms(-L,LResult),simplify_terms(R,RResult),
	simplify_terms(LResult+RResult,Result).
simplify_terms(-(L+R), Result) :-
	simplify_terms(-L,LResult),simplify_terms(-R,RResult),
	simplify_terms(LResult+RResult,Result).
simplify_terms(X+Y,LResult+RResult) :- 
	simplify_term(Y,RResult),
	simplify_terms(X,LResult).
simplify_terms(X,Result):- simplify_term(X,Result).

simplify_term(L*(R1*R2), Result) :-
	simplify_term(L*R2,LResult),
	simplify_term(R1,RResult),
	simplify_term(LResult*RResult,Result),!.
simplify_term((L1*L2)*N, Result):- number(N),
	simplify_term(L1*N,LResult),
	simplify_term(L2,RResult),
	simplify_term(LResult*RResult,Result).
simplify_term(X*Y, Result*Y) :- 
	atom(Y), simplify_term(X,Result).
simplify_term(N*M,P) :- number(N),number(M), P is N*M.
simplify_term(X*N,N*X) :- number(N),atom(X).
simplify_term(N*X,N*X) :- number(N),atom(X).
simplify_term(X,X).
