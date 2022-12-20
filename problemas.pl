ultimos2([X, Y], X, Y).
ultimos2([_| Resto], X, Y) :- ultimos2(Resto, X, Y).

junta([], L, L).
junta([P|R],L1, [P|L2]) :- junta(R,L1,L2).

insere_ordenado(E, [], [E]).

insere_ordenado(E, [P|R], [E, P|R]) :-
    P > E.

insere_ordenado(E, [P|R], [P|R1]) :-
    P =< E,
    insere_ordenado(E, R, R1).

nao_membro(_, []).
nao_membro(E, [P| R]) :-
    E \== P,
    nao_membro(E, R).

comp_maior_lista([L], C) :- length(L, C), !.
comp_maior_lista([P | R], C):-
    length(P,CurrLen),
    comp_maior_lista(R, PreviousMax),
    C is max(CurrLen, PreviousMax).