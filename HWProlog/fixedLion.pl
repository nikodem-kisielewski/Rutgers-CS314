eats(wolf, lamb).
eats(lamb, grass).
plant(grass).
eats(lion, X):-
    plant(Food),
    eats(X, Food).