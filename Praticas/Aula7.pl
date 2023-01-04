%7.6.1


insere_ordenado(El, L, R):-
    include(@>(El), L, R).
    
num_occ(Lst, El, N):-
    include(==(El), Lst, LstEl),
    length(LstEl, N).