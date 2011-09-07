% Exercise 1
% Define a Prolog predicate that takes two list arguments, 
% and succeeds when the second list is the reverse of the first.

reverse([], []).
reverse([H|T], RevList) :- 
	append(RevList1, [H] , RevList),
	reverse(T, RevList1), !.

% Base Case: Succeeds if both Lists are single element, and they both contains the
% 			 same element.
is_reverse(List1, List2) :- reverse(List2, RevList), !, List1 = RevList.

% Base Case: Succeeds if both Lists are empty
is_reverse([], []).