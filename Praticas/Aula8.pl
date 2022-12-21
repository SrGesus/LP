% 7.7.2
suc(N, M):- M is N+1.
ant(N, M):- M is N-1.

%7.7.3
perimetro(R, P):- P is 2*R*pi.

%7.7.4
divisor(D,N):- N mod D =:= 0.

%7.7.5
aplica_op(Op, Val1, Val2, R):-
    Conta=..[Op, Val1, Val2],
    R is Conta.

%7.7.6
listaDigitos(0, []).
% Lista vai ter os dígitos invertidos
listaDigitos(N, [R| Lista]):-
    N \= 0,
    R is N mod 10,
    NovoN is N // 10,
    listaDigitos(NovoN, Lista).

soma_digitos(N, S):-
    listaDigitos(N, ListaDigitos),
    soma_iter(ListaDigitos, S),
    soma_recur(ListaDigitos, S).

%7.7.6 (a)
soma_recur([], 0).
soma_recur([P|R], Soma):-
    soma_recur(R, PrevSoma),
    Soma is P + PrevSoma.

%7.7.6 (b)
soma_iter(Lista, Soma):- soma_iter(Lista, 0, Soma).
soma_iter([],X,X).
soma_iter([P|R], X, Soma):-
    NewX is P+X,
    soma_iter(R, NewX, Soma).

%7.7.7
digitosLista([], 0).
digitosLista([P| R], N):-
    length([P|R], Len),
    digitosLista(R, RestoN),
    N is P*10^(Len-1) + RestoN.

inverte(N, Inv):-
    listaDigitos(N, ListaDigitos),  % lista já invertida
    digitosLista(ListaDigitos, Inv).

%7.7.8
triangular(N, M):-
    N > 0,
    N2 is N-M,
    M2 is M+1,
    triangular(N2, M2).
triangular(0, _).

triangular(N):- 
    N>0, 
    triangular(N, 1).