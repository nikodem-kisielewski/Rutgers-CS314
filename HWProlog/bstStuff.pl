get_min(bst(nil, X, _), X).
get_min(bst(L, _, _), X):-
    get_min(L, X).

get_max(bst(_, X, nil), X).
get_max(bst(_, _, R), X):-
    get_max(R, X).

is_bst(nil).
is_bst(bst(L, _, R)):-
    is_bst(L),
    is_bst(R).