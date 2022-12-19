ultimos2([X, Y], X, Y).
ultimos2([_| Resto], X, Y) :- ultimos2(Resto, X, Y).

junta([], L, L).
junta([P|R],L1, [P|L2]) :- junta(R,L1,L2).

test([Cauda| Resto], Resto).

insere_ordenado(E, [], [E]).

insere_ordenado(E, [P|R], [E, P|R]) :-
    P > E.

insere_ordenado(E, [P|R], [P|R1]) :-
    P =< E,
    insere_ordenado(E, R, R1).
