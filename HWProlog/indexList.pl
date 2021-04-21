indexList(H, 0, [H|_]).
indexList(A, B, [_|T]):-
    B1 is B - 1,
    indexList(A, B1, T).