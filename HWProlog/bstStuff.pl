get_min(bst(nil, V, _), X):-
    X = V.
get_min(bst(L, _, _), X):-
    get_min(L, X).

get_max(bst(_, A, nil), X):-
    X = A.
get_max(bst(_, _, R), X):-
    get_max(R, X).

is_bst(bst(L, V, R)):-
    get_max(L, X),
    get_min(R, Y),
    V > X,
    V < Y.