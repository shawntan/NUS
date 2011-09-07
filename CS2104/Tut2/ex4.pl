% Exercise 4

% (2+3)*(4+5-3)+10/2

:- op(960,fx, push).
:- op(1099,yf,;).

translate(Expr, InList, OutList) :- 
	Expr =.. [F,A,B],
	translate(A, InList, OutList1),
	translate(B, OutList1, OutList2),
	writeln(F),
	(
		(F == + -> append(OutList2, [add], OutList), writeln(F), !);
		(F == - -> append(OutList2, [sub], OutList), writeln(F), !);
		(F == / -> append(OutList2, [div], OutList), writeln(F), !);
		(F == * -> append(OutList2, [mul], OutList), writeln(F), !)
	), !. 


/*	translate(Expr, InList, OutList) :- 
		Expr =.. [F,A,B],
		translate(A, InList, OutList1),
		translate(B, OutList1, OutList2),
		(F == + -> append([add;], OutList2, OutList), writeln(F), !);
		(F == - -> append([sub;], OutList2, OutList), writeln(F), !);
		(F == / -> append([div;], OutList2, OutList), writeln(F), !);
		(F == * -> append([mul;], OutList2, OutList), writeln(F), !).


	translate(Expr, InList, OutList) :- 
		write('push '), writeln(Expr), 
		append([push Expr], InList, OutList), !.*/


translate(Expr, InList, OutList) :- 
	write('push '), writeln(Expr),
	append(InList, [push Expr;], OutList).



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
	N3 is N1-N2,
	append([N3], InStack2, OutStack).

exec(sub, InStack, OutStack) :- 
	InStack  = [N1|InStack1],
	InStack1 = [N2|InStack2],
	N3 is N1-N2,
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
	N3 is N1/N2,
	append([N3], InStack2, OutStack).

exec(div, InStack, OutStack) :- 
	InStack  = [N1|InStack1],
	InStack1 = [N2|InStack2],
	N3 is N1/N2,
	append([N3], InStack2, OutStack).

