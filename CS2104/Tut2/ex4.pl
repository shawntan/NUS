% Exercise 4

% (2+3)*(4+5-3)+10/2
%  translate((2+3)*(4+5-3)+10/2, [], X), execute_code(X, [], Y).

:- op(960,fx, push).
:- op(1099,yf,;).

translate(Expr, InList, OutList) :- 
	Expr =.. [F,A,B],
	translate(A, InList, OutList1),
	translate(B, OutList1, OutList2),
	writeln(F),
	(
		(F == + -> append(OutList2, [add], OutList), !);
		(F == - -> append(OutList2, [sub], OutList), !);
		(F == / -> append(OutList2, [div], OutList), !);
		(F == * -> append(OutList2, [mul], OutList), !)
	), !. 


translate(Expr, InList, OutList) :- 
	write('push '), writeln(Expr),
	append(InList, [push Expr;], OutList).

execute_code([H|T], InStack, OutStack) :-
	exec(H, InStack, OutStack1), execute_code(T, OutStack1, OutStack).

execute_code([], [H|_], _) :- !, writeln(H).


% Same as Exercise 3.
exec(S;Ss, OutStack) :- 
	exec(S,  [], OutStack1),
	exec(Ss, OutStack1, OutStack), !.

exec(S;Ss, InStack, OutStack) :- 
	exec(S,  InStack, OutStack1),
	exec(Ss, OutStack1, OutStack), !.

exec(push X;, InStack, OutStack) :- 
	append([X], InStack, OutStack).
	
exec(push X, InStack, OutStack) :- 
	append([X], InStack, OutStack).	

exec(add;, InStack, OutStack) :- 
	InStack  = [N1|InStack1],
	InStack1 = [N2|InStack2],
	N3 is N1+N2,
	append([N3], InStack2, OutStack).
	
exec(add, InStack, OutStack) :- 
	InStack  = [N1|InStack1],
	InStack1 = [N2|InStack2],
	N3 is N1+N2,
	append([N3], InStack2, OutStack).

exec(sub;, InStack, OutStack) :- 
	InStack  = [N1|InStack1],
	InStack1 = [N2|InStack2],
	N3 is N2-N1,
	append([N3], InStack2, OutStack).

exec(sub, InStack, OutStack) :- 
	InStack  = [N1|InStack1],
	InStack1 = [N2|InStack2],
	N3 is N2-N1,
	append([N3], InStack2, OutStack).		

exec(mul;, InStack, OutStack) :- 
	InStack  = [N1|InStack1],
	InStack1 = [N2|InStack2],
	N3 is N1*N2,
	append([N3], InStack2, OutStack).

exec(mul, InStack, OutStack) :- 
	InStack  = [N1|InStack1],
	InStack1 = [N2|InStack2],
	N3 is N1*N2,
	append([N3], InStack2, OutStack).	
	
exec(div;, InStack, OutStack) :- 
	InStack  = [N1|InStack1],
	InStack1 = [N2|InStack2],
	N3 is N2/N1,
	append([N3], InStack2, OutStack).

exec(div, InStack, OutStack) :- 
	InStack  = [N1|InStack1],
	InStack1 = [N2|InStack2],
	N3 is N2/N1,
	append([N3], InStack2, OutStack).

