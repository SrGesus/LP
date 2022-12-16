% ist1107030 Gabriel Ferreira
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- ['dados.pl'], ['keywords.pl']. % ficheiros a importar.

% Eventos Sem Salas
eventoSemSala(EventoID) :- evento(EventoID,_,_,_,semSala).
eventosSemSalas(ListaEventos) :- 
    setof(EventoID, eventoSemSala(EventoID), ListaEventos).

% Eventos Sem Salas num determinado dia da semana.
eventoSemSalaDiaSemana(DiaSemana, EventoID) :- evento(EventoID,_,_,_,semSala), horario(EventoID, DiaSemana,_,_,_,_).
eventosSemSalasDiaSemana(DiaSemana, ListaEventos) :- 
    setof(EventoID, eventoSemSalaDiaSemana(DiaSemana, EventoID), ListaEventos).

% contabilizar disciplinas semestrais nos seus dois periodos.
horario2(A,B,C,D,E,F) :- horario(A,B,C,D,E,F).
horario2(A,B,C,D,E,p1) :- horario(A,B,C,D,E,p1_2).
horario2(A,B,C,D,E,p2) :- horario(A,B,C,D,E,p1_2).
horario2(A,B,C,D,E,p3) :- horario(A,B,C,D,E,p3_4).
horario2(A,B,C,D,E,p4) :- horario(A,B,C,D,E,p3_4).

% Eventos Sem Sala num Período.
eventoPeriodo(Periodo, EventoID) :- horario2(EventoID,_,_,_,_,Periodo).
eventoSemSalaPeriodo(Periodo, EventoID) :- eventoSemSala(EventoID), eventoPeriodo(Periodo, EventoID).
eventoSemSalaListaPeriodo([Periodo | Resto], EventoID) :- eventoSemSalaPeriodo(Periodo, EventoID); eventoSemSalaListaPeriodo(Resto, EventoID).
eventosSemSalasPeriodo(ListaPeriodo, ListaEventos) :- 
    setof(EventoID, eventoSemSalaListaPeriodo(ListaPeriodo, EventoID), ListaEventos).

% Filtra Lista de Eventos por um Período.
eventoPeriodoNaLista([EventoID |_], Periodo, EventoID) :- eventoPeriodo(Periodo, EventoID).
eventoPeriodoNaLista([_| Resto], Periodo, EventoID) :- eventoPeriodoNaLista(Resto, Periodo, EventoID).
organizaEventos(ListaEventos, Periodo, EventosNoPeriodo) :- 
    setof(EventoID, eventoPeriodoNaLista(ListaEventos, Periodo, EventoID), EventosNoPeriodo).

% Eventos 
eventoDuracao(Duracao, EventoID) :- horario2(EventoID,_,_,_,Duracao,_).
eventoMenorQue(Duracao, EventoID) :- eventoDuracao(DuracaoEvento, EventoID), DuracaoEvento =< Duracao.
eventosMenoresQue(Duracao, ListaEventos) :- 
    setof(EventoID, eventoMenorQue(Duracao, EventoID), ListaEventos).

eventoMenorQueBool(EventoID, Duracao) :- eventoMenorQue(Duracao, EventoID).

% Procurar Disciplinas

cursoDisciplina(Curso, Disciplina) :- evento(EventoID, Disciplina,_,_,_), turno(EventoID, Curso,_,_).
procuraDisciplinas(Curso, ListaDisciplinas)
    :- setof(Disciplina, cursoDisciplina(Curso, Disciplina), ListaDisciplinas).

% Organiza Disciplinas
disciplinaSemestre1(Curso, Disciplina) :- evento(EventoID, Disciplina,_,_,_), turno(EventoID, Curso,_,_), (eventoPeriodo(p1, EventoID); eventoPeriodo(p2, EventoID)).
disciplinaSemestre2(Curso, Disciplina) :- evento(EventoID, Disciplina,_,_,_), turno(EventoID, Curso,_,_), (eventoPeriodo(p3, EventoID); eventoPeriodo(p4, EventoID)).

organizaDisciplinasAux([], _, [[],[]]).
organizaDisciplinasAux([Disciplina| ListaDisciplinas], Curso, [[Disciplina| Semestre1], Semestre2]) :- 
    disciplinaSemestre1(Curso, Disciplina),
    organizaDisciplinasAux(ListaDisciplinas, Curso, [Semestre1, Semestre2]).
organizaDisciplinasAux([Disciplina| ListaDisciplinas], Curso, [Semestre1, [Disciplina| Semestre2]]) :- 
    disciplinaSemestre2(Curso, Disciplina),
    organizaDisciplinasAux(ListaDisciplinas, Curso, [Semestre1, Semestre2]).
organizaDisciplinas(ListaDisciplinas, Curso, SemestresSorted) :- organizaDisciplinasAux(ListaDisciplinas, Curso, Semestres), sort(Semestres, SemestresSorted).

% Hora Curso
eventoCurso(EventoID, Curso, Ano) :- turno(EventoID, Curso, Ano, _).
listaEventosCurso(Curso, Ano, ListaEventos) :- setof(EventoID, eventoCurso(EventoID, Curso, Ano), ListaEventos).
% Se não encontrar duração, assumir com o0
eventoDuracaoPeriodo(EventoID, Curso, Periodo, Ano, Duracao) :- 
    setof(Duracao1, (horario2(EventoID, _, _, _, Duracao1, Periodo)), [Duracao|_]);
    (eventoCurso(EventoID, Curso, Ano),
    Duracao is 0,
    \+ (horario2(EventoID, _, _, _, _, Periodo))).
        
horasCursoAux([], _, _, _, 0).
horasCursoAux([EventoID| Resto], Curso, Periodo, Ano, TotalHoras) :-
    horasCursoAux(Resto, Curso, Periodo, Ano, Soma),
    eventoDuracaoPeriodo(EventoID, Curso, Periodo, Ano, Duracao),
    TotalHoras is Soma + Duracao, !.


horasCurso(Periodo, Curso, Ano, TotalHoras) :- listaEventosCurso(Curso, Ano, ListaEventos), horasCursoAux(ListaEventos, Curso, Periodo, Ano, TotalHoras).


% evolucaoHorasCurso
evolucaoHorasCursoAux(Curso, Ano, Periodo, TotalHoras) :- Periodo = p1,
    horasCurso(p1, Curso, Ano, TotalHoras).
evolucaoHorasCursoAux(Curso, Ano, Periodo, TotalHoras) :- Periodo = p2,
    horasCurso(p2, Curso, Ano, TotalHoras).
evolucaoHorasCursoAux(Curso, Ano, Periodo, TotalHoras) :- Periodo = p3,
    horasCurso(p3, Curso, Ano, TotalHoras).
evolucaoHorasCursoAux(Curso, Ano, Periodo, TotalHoras) :- Periodo = p4,
    horasCurso(p4, Curso, Ano, TotalHoras).
evolucaoHorasCurso(Curso, Evolucao) :- setof((Ano, Periodo, TotalHoras), evolucaoHorasCursoAux(Curso, Ano, Periodo, TotalHoras), Evolucao).