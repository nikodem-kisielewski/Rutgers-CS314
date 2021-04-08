flattened([]).
flattened([H|T]):-
    H \= [_|_],
    flattened(T).