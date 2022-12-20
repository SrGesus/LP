% ist1107030 Gabriel Ferreira
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- ['dados.pl'], ['keywords.pl']. % ficheiros a importar.


    %%%%%%%%%%%%%%%%%%%
    % eventosSemSalas %
    %%%%%%%%%%%%%%%%%%%

% e verdade se EventoID e um evento semSala
eventoSemSala(EventoID):- evento(EventoID, _, _, _, semSala).

% e verdade se ListaEventos e uma lista, ordenada e
% sem elementos repetidos, de IDs de eventos sem sala
eventosSemSalas(ListaEventosSorted):-
    findall(EventoID, eventoSemSala(EventoID), ListaEventos),
    sort(ListaEventos, ListaEventosSorted).


    %%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % eventosSemSalasDiaSemana %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% e verdade se EventoID e um evento semSala com horario no dia da semana.
eventosSemSalasDiaSemanaAux(DiaDaSemana, EventoID):-
    eventoSemSala(EventoID), horario(EventoID, DiaDaSemana, _, _, _, _).

% e verdade se ListaEventos e uma lista, 
% ordenada e sem elementos repetidos, de IDs de eventos sem sala,
% que decorrem em DiaDaSemana.
eventosSemSalasDiaSemana(DiaDaSemana, ListaEventosSorted):-
    findall(EventoID, eventosSemSalasDiaSemanaAux(DiaDaSemana, EventoID), ListaEventos),
    sort(ListaEventos, ListaEventosSorted).


    %%%%%%%%%%%%%%%%%%%%%%%%%%
    % eventosSemSalasPeriodo %
    %%%%%%%%%%%%%%%%%%%%%%%%%%

% horarioPeriodos para p1 e p2 e verdade se horario para p1_2 e verdade
% horarioPeriodos para p3 e p4 e verdade se horario para p3_4 e verdade
horarioPeriodos(A,B,C,D,E,F) :- horario(A,B,C,D,E,F).
horarioPeriodos(A,B,C,D,E,p1) :- horario(A,B,C,D,E,p1_2).
horarioPeriodos(A,B,C,D,E,p2) :- horario(A,B,C,D,E,p1_2).
horarioPeriodos(A,B,C,D,E,p3) :- horario(A,B,C,D,E,p3_4).
horarioPeriodos(A,B,C,D,E,p4) :- horario(A,B,C,D,E,p3_4).

% eventoPeriodo e verdade se EventoID e um evento com horario no Periodo
eventoPeriodo(EventoID, Periodo):-
    horarioPeriodos(EventoID, _, _, _, _, Periodo).

% e verdade se EventoID e um evento sem sala 
% com horario num Periodo em ListaPeriodos
eventosSemSalasPeriodoAux(ListaPeriodos, EventoID):-
    member(Periodo, ListaPeriodos),
    eventoSemSala(EventoID),
    eventoPeriodo(EventoID, Periodo).

% e verdade se ListaEventosSorted e uma lista dos eventos, 
% ordenada e sem elementos repetidos, de IDs de eventos sem sala nos 
% Periodos de ListaPeriodos.
eventosSemSalasPeriodo(ListaPeriodos, ListaEventosSorted):-
    findall(EventoID, eventosSemSalasPeriodoAux(ListaPeriodos, EventoID), ListaEventos),
    sort(ListaEventos, ListaEventosSorted).


    %%%%%%%%%%%%%%%%%%%
    % organizaEventos %
    %%%%%%%%%%%%%%%%%%%

% caso terminal
filtraEventosPeriodo([], _, []).
% caso EventoID decorra no Periodo
filtraEventosPeriodo([EventoID | R], Periodo, [EventoID | ListaEventos]):-
    eventoPeriodo(EventoID, Periodo),
    filtraEventosPeriodo(R, Periodo, ListaEventos).
% caso EventoID nao decorra no Periodo
filtraEventosPeriodo([EventoID | R], Periodo, ListaEventos):-
    \+ eventoPeriodo(EventoID, Periodo),
    filtraEventosPeriodo(R, Periodo, ListaEventos).

% verdade se EventosNoPeriodoSorted e uma lista ordenada sem repeticao 
% dos eventos da ListaEventos que ocorrem no Periodo
organizaEventos(ListaEventos, Periodo, EventosNoPeriodoSorted):-
    filtraEventosPeriodo(ListaEventos, Periodo, EventosNoPeriodo),
    sort(EventosNoPeriodo, EventosNoPeriodoSorted).


    %%%%%%%%%%%%%%%%%%%%%
    % eventosMenoresQue %
    %%%%%%%%%%%%%%%%%%%%%

% e verdade se Duracao e a Duracao do evento EventoID
eventoDuracao(Duracao, EventoID):- horario(EventoID,_,_,_,Duracao,_).

% e verdade se EventoID e um evento com duracao menor ou igual a Duracao
eventosMenoresQueBool(EventoID, Duracao):- 
    eventoDuracao(DuracaoEvento, EventoID), 
    DuracaoEvento =< Duracao.

% e verdade se ListaEventosSorted e uma lista ordenada e sem 
% elementos repetidos dos EventoIDs que teem uma duracao maior 
% ou igual a Duracao
eventosMenoresQue(Duracao, ListaEventosSorted):-
    findall(EventoID, eventosMenoresQueBool(EventoID, Duracao), ListaEventos),
    sort(ListaEventos, ListaEventosSorted).


    %%%%%%%%%%%%%%%%%%%%%%
    % procuraDisciplinas %
    %%%%%%%%%%%%%%%%%%%%%%

% e verdade se Disciplina tem um turno para Curso
disciplinaCurso(Disciplina, Curso):- 
    turno(EventoID, Curso, _, _),
    evento(EventoID, Disciplina, _, _, _).

% e verdade se ListaDisciplinasSorted e a lista
% ordenada alfabeticamente do nome das disciplinas do Curso
procuraDisciplinas(Curso, ListaDisciplinasSorted):-
    findall(Disciplina, disciplinaCurso(Disciplina, Curso), ListaDisciplinas),
    sort(ListaDisciplinas, ListaDisciplinasSorted).


    %%%%%%%%%%%%%%%%%%%%%%%
    % organizaDisciplinas %
    %%%%%%%%%%%%%%%%%%%%%%%

% e verdade se Disciplina tem um turno no curso no respetivo semestre
disciplinaSemestre1(Curso, Disciplina) :- 
    evento(EventoID, Disciplina,_,_,_), 
    turno(EventoID, Curso,_,_), 
    (eventoPeriodo(p1, EventoID); eventoPeriodo(p2, EventoID)).
disciplinaSemestre2(Curso, Disciplina) :- 
    evento(EventoID, Disciplina,_,_,_), 
    turno(EventoID, Curso,_,_),
    (eventoPeriodo(p3, EventoID); eventoPeriodo(p4, EventoID)).

% caso terminal
organizaDisciplinasAux([], _, [[],[]]).
% caso evento tenha Disciplina no Curso no semestre 1
organizaDisciplinasAux([Disciplina| ListaDisciplinas], Curso, [[Disciplina| Semestre1], Semestre2]) :- 
    disciplinaSemestre1(Curso, Disciplina),
    \+ disciplinaSemestre2(Curso, Disciplina),
    organizaDisciplinasAux(ListaDisciplinas, Curso, [Semestre1, Semestre2]).

organizaDisciplinasAux([Disciplina| ListaDisciplinas], Curso, [Semestre1, [Disciplina| Semestre2]]) :- 
    disciplinaSemestre2(Curso, Disciplina),
    \+ disciplinaSemestre1(Curso, Disciplina),
    organizaDisciplinasAux(ListaDisciplinas, Curso, [Semestre1, Semestre2]).

organizaDisciplinasAux([Disciplina| ListaDisciplinas], Curso, [[Disciplina| Semestre1], [Disciplina | Semestre2]]) :- 
    disciplinaSemestre1(Curso, Disciplina),
    disciplinaSemestre2(Curso, Disciplina),
    organizaDisciplinasAux(ListaDisciplinas, Curso, [Semestre1, Semestre2]).
