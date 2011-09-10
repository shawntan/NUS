:- op(1099,yf,;).
exec((S ; Ss)) :- 
	write(Ss), !.
