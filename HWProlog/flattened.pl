flattened([]).
flattened([H|T]):-
    \+is_list(H),
    flattened(T).