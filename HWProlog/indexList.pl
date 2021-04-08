indexList(A, 0, [H|_]):-
    H = A.
indexList(A, B, [_|T]):-
    indexList(A, B1, T),
    B1 is B - 1.