:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- ['dados.pl'], ['keywords.pl']. % ficheiros a importar.


    %%%%%%%%%%%%%%%%%%%
    % eventosSemSalas %
    %%%%%%%%%%%%%%%%%%%

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% eventoSemSala(EventoID)
% EventoID e um evento singular semSala
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
eventoSemSala(EventoID):- evento(EventoID, _, _, _, semSala).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% eventosSemSalas(EventosSemSala)
% EventosSemSala e uma lista, ordenada e
% sem elementos repetidos, de IDs de eventos sem sala.
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
eventosSemSalas(ListaEventosSorted):-
    findall(EventoID, eventoSemSala(EventoID), ListaEventos),
    sort(ListaEventos, ListaEventosSorted).


    %%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % eventosSemSalasDiaSemana %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% eventoSemSalaNoDiaSemana(DiaSemana, EventoID)
% EventoID e um evento semSala com horario no DiaSemana.
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
eventoSemSalaNoDiaSemana(DiaSemana, EventoID):-
    eventoSemSala(EventoID), horario(EventoID, DiaSemana, _, _, _, _).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% eventosSemSalasDiaSemana(DiaSemana, EventosSemSala)
% EventosSemSala e uma lista, ordenada e sem elementos repetidos, 
% de IDs de eventos sem sala, com horario em DiaSemana.
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
eventosSemSalasDiaSemana(DiaSemana, ListaEventosSorted):-
    findall(EventoID, eventoSemSalaNoDiaSemana(DiaSemana, EventoID), ListaEventos),
    sort(ListaEventos, ListaEventosSorted).


    %%%%%%%%%%%%%%%%%%%%%%%%%%
    % eventosSemSalasPeriodo %
    %%%%%%%%%%%%%%%%%%%%%%%%%%

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% horarioPeriodos(EventoID, DiaSemana, HoraInicio, HoraFim, 
% Duracao, Periodo)
% e verdade se horario para os mesmos argumentos tambem for
% mas tambem para Periodo = p1 ou p2 se o evento for de primeiro semestre
% e para Periodo = p3 ou p4 se o evento for de segundo semestre
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
horarioPeriodos(EventoID, DiaSemana, HoraInicio, HoraFim, Duracao, Periodo):- 
    horario(EventoID, DiaSemana, HoraInicio, HoraFim, Duracao, PeriodoAux),
    ((PeriodoAux = p1_2, (Periodo = p1; Periodo = p2));
    (PeriodoAux = p3_4, (Periodo = p3; Periodo = p4));
    PeriodoAux = Periodo).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% eventoPeriodo(EventoID, Periodo) 
% EventoID e um evento com horario em Periodo
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
eventoPeriodo(EventoID, Periodo):-
    horarioPeriodos(EventoID, _, _, _, _, Periodo).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% eventoSemSalaNoPeriodo(ListaPeriodos, EventoID)
% EventoID e um evento sem sala
% com horario num Periodo em ListaPeriodos
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
eventoSemSalaNoPeriodo(ListaPeriodos, EventoID):-
    member(Periodo, ListaPeriodos),
    eventoSemSala(EventoID),
    eventoPeriodo(EventoID, Periodo).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala)
% EventosSemSala e uma lista, ordenada e sem elementos repetidos,
% de IDs de eventos sem sala nos Periodos de ListaPeriodos.
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
eventosSemSalasPeriodo(ListaPeriodos, ListaEventosSorted):-
    findall(EventoID, eventoSemSalaNoPeriodo(ListaPeriodos, EventoID), ListaEventos),
    sort(ListaEventos, ListaEventosSorted).


    %%%%%%%%%%%%%%%%%%%
    % organizaEventos %
    %%%%%%%%%%%%%%%%%%%

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% filtraEventosPeriodo(ListaEventos, Periodo, EventosNoPeriodo)
% EventosNoPeriodo e uma lista dos EventoIDs pertencentes a
% ListaEventos que decorrem em Periodo.
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %

% caso terminal
filtraEventosPeriodo([], _, []).
% caso EventoID decorra no Periodo
filtraEventosPeriodo([EventoID | ListaEventos], Periodo, [EventoID | EventosNoPeriodo]):-
    eventoPeriodo(EventoID, Periodo),
    filtraEventosPeriodo(ListaEventos, Periodo, EventosNoPeriodo).
% caso EventoID nao decorra no Periodo
filtraEventosPeriodo([EventoID | ListaEventos], Periodo, EventosNoPeriodo):-
    \+ eventoPeriodo(EventoID, Periodo),
    filtraEventosPeriodo(ListaEventos, Periodo, EventosNoPeriodo).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% organizaEventos(ListaEventos, Periodo, EventosNoPeriodo)
% EventosNoPeriodo e uma lista ordenada sem repeticao 
% dos eventos da ListaEventos que ocorrem no Periodo
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
organizaEventos(ListaEventos, Periodo, EventosNoPeriodoSorted):-
    filtraEventosPeriodo(ListaEventos, Periodo, EventosNoPeriodo),
    sort(EventosNoPeriodo, EventosNoPeriodoSorted).


    %%%%%%%%%%%%%%%%%%%%%
    % eventosMenoresQue %
    %%%%%%%%%%%%%%%%%%%%%

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% eventoDuracao(Duracao, EventoID)
% Duracao e a Duracao do evento horario do EventoID
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
eventoDuracao(Duracao, EventoID):- horario(EventoID,_,_,_,Duracao,_).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% eventosMenoresQueBool(EventoID, Duracao)
% EventoID e um evento com duracao menor ou igual a Duracao
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
eventosMenoresQueBool(EventoID, Duracao):- 
    eventoDuracao(DuracaoEvento, EventoID), 
    DuracaoEvento =< Duracao.

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% ListaEventos e uma lista ordenada e sem 
% elementos repetidos dos EventoIDs que teem uma duracao maior 
% ou igual a Duracao
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
eventosMenoresQue(Duracao, ListaEventosSorted):-
    findall(EventoID, eventosMenoresQueBool(EventoID, Duracao), ListaEventos),
    sort(ListaEventos, ListaEventosSorted).


    %%%%%%%%%%%%%%%%%%%%%%
    % procuraDisciplinas %
    %%%%%%%%%%%%%%%%%%%%%%

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% disciplinaCurso(Disciplina, Curso)
% Disciplina tem um turno para Curso
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
disciplinaCurso(Disciplina, Curso):- 
    turno(EventoID, Curso, _, _),
    evento(EventoID, Disciplina, _, _, _).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% procuraDisciplinas(Curso, ListaDisciplinas)
% ListaDisciplinasSorted e a lista ordenada alfabeticamente
% do nome das disciplinas do Curso
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
procuraDisciplinas(Curso, ListaDisciplinasSorted):-
    findall(Disciplina, disciplinaCurso(Disciplina, Curso), ListaDisciplinas),
    sort(ListaDisciplinas, ListaDisciplinasSorted).


    %%%%%%%%%%%%%%%%%%%%%%%
    % organizaDisciplinas %
    %%%%%%%%%%%%%%%%%%%%%%%

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% disciplinaSemestre1(Curso, Disciplina) e
% disciplinaSemestre2(Curso, Disciplina)
% Disciplina tem um turno no Curso no respetivo semestre
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
disciplinaSemestre1(Curso, Disciplina):- 
    evento(EventoID, Disciplina, _, _, _), 
    turno(EventoID, Curso, _, _), 
    (eventoPeriodo(EventoID, p1); eventoPeriodo(EventoID, p2)).
disciplinaSemestre2(Curso, Disciplina):- 
    evento(EventoID, Disciplina, _, _, _), 
    turno(EventoID, Curso, _, _),
    (eventoPeriodo(EventoID, p3); eventoPeriodo(EventoID, p4)).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% organizaDisciplinasAux(ListaDisciplinas, Curso, Semestres)
% predicado auxiliar - faz o trabalho do organizaDisciplinas antes 
% da ordenacao alfabetica. Itera os elementos de ListaDisciplinas
% recursivamente e insere-os no respetivo semestre(s).
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %

% caso terminal
organizaDisciplinasAux([], _, [[],[]]).

% caso evento tenha Disciplina no Curso no semestre 1
organizaDisciplinasAux([Disciplina| ListaDisciplinas], Curso, [[Disciplina| Semestre1], Semestre2]):- 
    disciplinaSemestre1(Curso, Disciplina),
    organizaDisciplinasAux(ListaDisciplinas, Curso, [Semestre1, Semestre2]).

% caso evento tenha Disciplina no Curso no semestre 2
organizaDisciplinasAux([Disciplina| ListaDisciplinas], Curso, [Semestre1, [Disciplina| Semestre2]]):- 
    disciplinaSemestre2(Curso, Disciplina),
    \+ disciplinaSemestre1(Curso, Disciplina),
    organizaDisciplinasAux(ListaDisciplinas, Curso, [Semestre1, Semestre2]).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% organizaDisciplinas(ListaDisciplinas, Curso, Semestres)
% Recorre ao predicado auxiliar organizaDisciplinasAux 
% Semestres e uma lista com duas listas ordenadas, a primeira sao os 
% elementos de ListaDisciplinas que ocorrem no primeiro semestre, 
% ou primeiro e segundo semestre, e na segunda lista os que ocorrem 
% somente no segundo
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
organizaDisciplinas(ListaDisciplinas, Curso, [Semestre1Sorted, Semestre2Sorted]):-
    organizaDisciplinasAux(ListaDisciplinas, Curso, [Semestre1, Semestre2]), !,
    sort(Semestre1, Semestre1Sorted),
    sort(Semestre2, Semestre2Sorted).


    %%%%%%%%%%%%%%
    % horasCurso %
    %%%%%%%%%%%%%%

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% eventosCursoPeriodoAno(EventoID, Periodo, Curso, Ano)
% EventoID e o ID de um evento com horario no Periodo 
% e turno para Curso e Ano
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
eventosCursoPeriodoAno(EventoID, Periodo, Curso, Ano) :-
    turno(EventoID, Curso, Ano, _),
    eventoPeriodo(EventoID, Periodo).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% horasCursoAux(ListaEventos, SomaHoras)
% predicado auxiliar - itera os EventoIDs de ListaEventos 
% e soma recursivamente a duracao dos eventos
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
horasCursoAux([], 0).
horasCursoAux([EventoID | ListaEventos], SomaHoras):-
    horasCursoAux(ListaEventos, PrevHoras),
    eventoDuracao(Duracao, EventoID),
    SomaHoras is Duracao + PrevHoras.

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% horasCurso(ListaEventos, TotalHoras)
% TotalHoras e a soma das duracoes dos eventos na ListaEventos
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
horasCurso(Periodo, Curso, Ano, TotalHoras):-
    findall(EventoID, eventosCursoPeriodoAno(EventoID, Periodo, Curso, Ano), ListaEventos),
    sort(ListaEventos, ListaEventosSorted),
    horasCursoAux(ListaEventosSorted, TotalHoras).


    %%%%%%%%%%%%%%%%%%%%%%
    % evolucaoHorasCurso %
    %%%%%%%%%%%%%%%%%%%%%%

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% horasCursoAnoPeriodo(Curso, (Ano, Periodo, TotalHoras))
% Total Horas e a duracao total das cadeiras do Curso 
% no Ano e Periodo
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
horasCursoAnoPeriodo(Curso, (Ano, Periodo, TotalHoras)):-
    turno(_, _, Ano, _),
    (Periodo = p1; Periodo = p2; Periodo = p3; Periodo = p4),
    horasCurso(Periodo, Curso, Ano, TotalHoras).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% evolucaoHorasCurso(Curso, Evolucao)
% Evolucao e uma lista ordenado de (Ano, Periodo, TotalHoras)
% tal que TotalHoras e a duracao total das cadeiras do Curso 
% no Ano e Periodo
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
evolucaoHorasCurso(Curso, Evolucao) :- 
    setof(Tuplo, horasCursoAnoPeriodo(Curso, Tuplo), Evolucao).


    %%%%%%%%%%%%%
    % ocupaSlot %
    %%%%%%%%%%%%%

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% ocupaSlot(Inicio1, Fim1, Inicio2, Fim2, Overlap)
% Overlap e a interseccao entre a aula que comeca entre Inicio1
% e Fim1 e a aula que comeca entre Inicio2 e Fim2
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
ocupaSlot(Inicio1, Fim1, Inicio2, Fim2, Overlap) :- 
    InicioMaior is max(Inicio1, Inicio2),
    FimMenor is min(Fim1, Fim2),
    FimMenor > InicioMaior,
    Overlap is FimMenor-InicioMaior.

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% ocupaSlotOverlap(Inicio1, Fim1, Inicio2, Fim2, Overlap)
% como ocupaSlot mas inves de falhar, Overlap torna-se 0
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
ocupaSlotOverlap(Inicio1, Fim1, Inicio2, Fim2, Overlap) :- 
    ocupaSlot(Inicio1, Fim1, Inicio2, Fim2, Overlap).

ocupaSlotOverlap(Inicio1, Fim1, Inicio2, Fim2, Overlap) :- 
    \+ ocupaSlot(Inicio1, Fim1, Inicio2, Fim2, _),
    Overlap = 0.

    %%%%%%%%%%%%%%%%%%%%
    % numHorasOcupadas %
    %%%%%%%%%%%%%%%%%%%%

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% tipoSala(Tipo, Sala)
% Sala e uma sala do tipo Tipo
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
tipoSala(Tipo, Sala) :- 
    salas(Tipo, ListaSalas),
    member(Sala, ListaSalas).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% eventoPeriodoSalaDia(EventoID, Periodo, TipoSala, DiaSemana)
% EventoID e o ID de um evento que ocorre durante o Periodo, no
% DiaSemana, numa sala do TipoSala.
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
eventoPeriodoSalaDia(EventoID, Periodo, TipoSala, DiaSemana):-
    evento(EventoID, _, _, _, Sala),
    tipoSala(TipoSala, Sala),
    horarioPeriodos(EventoID, DiaSemana, _, _, _, Periodo).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% numHorasOcupadasAux(ListaEventos, InicioLim, FimLim, SomaHoras)
% predicado auxiliar - soma recursivamente o Overlap das horas de 
% cada EventoID da ListaEventos com o InicioLim e FimLim
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %

% caso terminal
numHorasOcupadasAux([], _, _, 0).

% caso lista tem 1 ou mais elementos
numHorasOcupadasAux([EventoID | ListaEventos], InicioLim, FimLim, SomaHoras):-
    horario(EventoID, _, Inicio, Fim, _, _),
    ocupaSlotOverlap(Inicio, Fim, InicioLim, FimLim, Overlap),
    numHorasOcupadasAux(ListaEventos, InicioLim, FimLim, PrevHoras),
    SomaHoras is PrevHoras + Overlap.

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, TotalHoras)
% TotalHoras e a soma do overlap dos eventos que ocorrem no Periodo, 
% DiaSemana, e salas do TipoSala com a HoraInicio e HoraFim
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, TotalHoras):-
    setof(EventoID, eventoPeriodoSalaDia(EventoID, Periodo, TipoSala, DiaSemana), ListaEventos),
    numHorasOcupadasAux(ListaEventos, HoraInicio, HoraFim, TotalHoras).


    %%%%%%%%%%%%%%%
    % ocupacaoMax %
    %%%%%%%%%%%%%%%

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max)
% Max e a duracao correspondente a HoraInicio HoraFim vezes
% o numero de salas do TipoSala
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max):-
    ocupaSlotOverlap(HoraInicio, HoraFim, HoraInicio, HoraFim, Overlap),
    salas(TipoSala, ListaSalas),
    length(ListaSalas, NoSalas),
    Max is Overlap * NoSalas.


    %%%%%%%%%%%%%%%
    % percentagem %
    %%%%%%%%%%%%%%%

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% percentagem(Numerador, Denominador, Percentagem)
% Percentagem e o Numerador/Denominador * 100
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
percentagem(Numerador, Denominador, Percentagem):-
    Percentagem is Numerador/Denominador * 100.


    %%%%%%%%%%%%%%%%%%%
    % ocupacaoCritica %
    %%%%%%%%%%%%%%%%%%%

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% ocupacaoCriticaAux(HoraInicio, HoraFim, Threshold, DiaSemana, TipoSala, RoundedPercent)
% RoundedPercent e a percentagem arredondada com ceiling de
% TotalHoras ocupada para salas do TipoSala no DiaSemana
% a dividir pela ocupacaoMax, se for maior que a Threshold
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
ocupacaoCriticaAux(HoraInicio, HoraFim, Threshold, DiaSemana, TipoSala, RoundedPercent):-
    (Periodo = p1; Periodo = p2; Periodo = p3; Periodo = p4),
    numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, TotalHoras),
    ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max),
    percentagem(TotalHoras, Max, Percentagem),
    Percentagem > Threshold,
    RoundedPercent is ceiling(Percentagem).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados)
% Resultados e uma lista ordenada dos tuplos 
% casoCriticos(DiaSemana, TipoSala, Percentagem) em que a percentagem
% excede a Threshold
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
ocupacaoCritica(HoraInicio, HoraFim, Threshold, ResultadosSorted):-
    findall(casosCriticos(DiaSemana, TipoSala, Percentagem), 
        ocupacaoCriticaAux(HoraInicio, HoraFim, Threshold, DiaSemana, TipoSala, Percentagem), 
        Resultados),
    sort(Resultados, ResultadosSorted).


    %%%%%%%%%%%%%%%%
    % ocupacaoMesa %
    %%%%%%%%%%%%%%%%

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% cab1(Pessoa, OcupacaoMesa)
% cab2(Pessoa, OcupacaoMesa)
% Na OcupacaoMesa a Pessoa esta na respetiva cabeceira da mesa.
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
cab1(X4, [[ _,  _, _ ],[X4,  _],[ _,  _,  _]]).
cab2(X5, [[ _,  _, _ ],[ _, X5],[ _,  _,  _]]).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% honra(Pessoa1, Pessoa2, OcupacaoMesa)
% Na OcupacaoMesa a Pessoa2 esta aa direita da Pessoa1, 
% que esta numa das cabeceiras.
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
honra(X4, X6, [[ _,  _, _ ],[X4,  _],[X6,  _,  _]]).
honra(X5, X3, [[ _,  _, X3],[ _, X5],[ _,  _,  _]]).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% lado(Pessoa1, Pessoa2, OcupacaoMesa)
% Na OcupacaoMesa, Pessoa1 e Pessoa2 estao sentadas lado a lado
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
lado(P1, P2,  [[X1, X2, X3],[ _,  _],[X6, X7, X8]]):-
    (P1 = X2, (P2 = X1; P2 = X3));
    (P1 = X7, (P2 = X6; P2 = X8)).
lado(P2, P1,  [[X1, X2, X3],[ _,  _],[X6, X7, X8]]):-
    (P1 = X2, (P2 = X1; P2 = X3));
    (P1 = X7, (P2 = X6; P2 = X8)).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% frente(Pessoa1, Pessoa2, OcupacaoMesa)
% Na OcupacaoMesa, Pessoa1 e Pessoa2 estao sentadas frente a frente
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
frente(X1, X6, [[X1,  _,  _],[ _,  _],[X6,  _,  _]]).
frente(X2, X7, [[ _, X2,  _],[ _,  _],[ _, X7,  _]]).
frente(X3, X8, [[ _,  _, X3],[ _,  _],[ _,  _, X8]]).
frente(X6, X1, [[X1,  _,  _],[ _,  _],[X6,  _,  _]]).
frente(X7, X2, [[ _, X2,  _],[ _,  _],[ _, X7,  _]]).
frente(X8, X3, [[ _,  _, X3],[ _,  _],[ _,  _, X8]]).

naoLado(Pessoa1, Pessoa2, OcupacaoMesa):-
    \+ lado(Pessoa1, Pessoa2, OcupacaoMesa).

naoFrente(Pessoa1, Pessoa2, OcupacaoMesa):-
    \+ frente(Pessoa1, Pessoa2, OcupacaoMesa).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% callRegrasMesa(ListasPessoas, Regras, ocupacaoMesa)
% predicado auxiliar - itera as regras e faz call delas,
% aplicando-as aa OcupacaoMesa.
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %

% caso terminal
callRegrasMesa(_, [], _).

% caso lista tem 1 ou mais elementos
callRegrasMesa(ListaPessoas, [Regra | Resto], OcupacaoMesa):-
    call(Regra, OcupacaoMesa),
    callRegrasMesa(ListaPessoas, Resto, OcupacaoMesa).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% ocupacaoValida(ListaPessoas, OcupacaoMesa)
% OcupacaoMesa e uma lista de listas [[X1, X2, X3],[X4, X5],[X6, X7, X8]]
% tal que todos os X sao diferentes e membros de ListaPessoas.
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
ocupacaoValida(ListaPessoas, [[X1, X2, X3],[X4, X5],[X6, X7, X8]]):-
    permutation(ListaPessoas, [X1, X2, X3, X4, X5, X6, X7, X8]).

% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
% ocupacaoMesa(ListaPessoas, Regras, OcupacaoMesa)
% OcupacaoMesa e uma lista de listas [[X1, X2, X3],[X4, X5],[X6, X7, X8]]
% tal que X1..X8 sao as pessoas sentadas de acordo com as Regras.
% - - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - - %
ocupacaoMesa(ListaPessoas, Regras, OcupacaoMesa):-
    ocupacaoValida(ListaPessoas, OcupacaoMesa),
    callRegrasMesa(ListaPessoas, Regras, OcupacaoMesa).
