member(A, [A | _]).
member(A, [_ | T]):- member(A, T).

notmember(_, []).
notmember(A, [H|T]):-
    A \= H,
    notmember(A, T).