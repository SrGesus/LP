% ist1107030 Gabriel Ferreira
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- ['dados.pl'], ['keywords.pl']. % ficheiros a importar.


    % eventosSemSalas

% é verdade se EventoID é um evento semSala
eventosSemSalasAux(EventoID) :- evento(EventoID, _, _, _, semSala).

% é verdade se ListaEvento é uma lista, ordenada e
% sem elementos repetidos, de IDs de eventos sem sala
eventosSemSalas(ListaEvento) 
    :- setof(EventoID, eventosSemSalasAux(EventoID), ListaEvento).


    % eventosSemSalasDiaSemana

eventosSemSalasDiaSemana(DiaDaSemana, ListaEventos)
    :- setof(EventoID, (eventosSemSalasAux(EventoID), horario(EventoID, DiaDaSemana, _, _, _, _)), ListaEventos).