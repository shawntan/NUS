% Exercise 3.

:- op(960,fx, push).
:- op(1099,yf,;).

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

