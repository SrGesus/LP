% ist1107030 Gabriel Ferreira
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- ['dados.pl'], ['keywords.pl']. % ficheiros a importar.


    %%%%%%%%%%%%%%%%%%%
    % eventosSemSalas %
    %%%%%%%%%%%%%%%%%%%

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% eventoSemSala(EventoID)
% EventoID e um evento singular semSala
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
eventoSemSala(EventoID):- evento(EventoID, _, _, _, semSala).

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% eventosSemSalas(EventosSemSala)
% EventosSemSala e uma lista, ordenada e
% sem elementos repetidos, de IDs de eventos sem sala.
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
eventosSemSalas(ListaEventosSorted):-
    findall(EventoID, eventoSemSala(EventoID), ListaEventos),
    sort(ListaEventos, ListaEventosSorted).


    %%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % eventosSemSalasDiaSemana %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% eventosSemSalasDiaSemanaAux(DiaSemana, EventoID)
% EventoID e um evento semSala com horario no DiaSemana.
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
eventosSemSalasDiaSemanaAux(DiaSemana, EventoID):-
    eventoSemSala(EventoID), horario(EventoID, DiaSemana, _, _, _, _).

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% eventosSemSalasDiaSemana(DiaSemana, EventosSemSala)
% EventosSemSala e uma lista, ordenada e sem elementos repetidos, 
% de IDs de eventos sem sala, com horario em DiaSemana.
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
eventosSemSalasDiaSemana(DiaSemana, ListaEventosSorted):-
    findall(EventoID, eventosSemSalasDiaSemanaAux(DiaSemana, EventoID), ListaEventos),
    sort(ListaEventos, ListaEventosSorted).


    %%%%%%%%%%%%%%%%%%%%%%%%%%
    % eventosSemSalasPeriodo %
    %%%%%%%%%%%%%%%%%%%%%%%%%%

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% horarioPeriodos(EventoID, DiaSemana, HoraInicio, HoraFim, 
% Duracao, Periodo)
% e verdade se horario para os mesmos argumentos tambem for
% mas tambem para Periodo = p1 ou p2 se Periodo for p1_2
% e para Periodo = p3 ou p4 se Periodo for p3_4
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
horarioPeriodos(EventoID, DiaSemana, HoraInicio, HoraFim, Duracao, Periodo):- 
    horario(EventoID, DiaSemana, HoraInicio, HoraFim, Duracao, PeriodoAux),
    ((PeriodoAux = p1_2, (Periodo = p1; Periodo = p2));
    (PeriodoAux = p3_4, (Periodo = p3; Periodo = p4));
    PeriodoAux = Periodo).

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% eventoPeriodo(EventoID, Periodo) 
% EventoID e um evento com horario em Periodo
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
eventoPeriodo(EventoID, Periodo):-
    horarioPeriodos(EventoID, _, _, _, _, Periodo).

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% eventosSemSalasPeriodoAux(ListaPeriodos, EventoID)
% EventoID e um evento sem sala
% com horario num Periodo em ListaPeriodos
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
eventosSemSalasPeriodoAux(ListaPeriodos, EventoID):-
    member(Periodo, ListaPeriodos),
    eventoSemSala(EventoID),
    eventoPeriodo(EventoID, Periodo).

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala)
% EventosSemSala e uma lista, ordenada e sem elementos repetidos,
% de IDs de eventos sem sala nos Periodos de ListaPeriodos.
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
eventosSemSalasPeriodo(ListaPeriodos, ListaEventosSorted):-
    findall(EventoID, eventosSemSalasPeriodoAux(ListaPeriodos, EventoID), ListaEventos),
    sort(ListaEventos, ListaEventosSorted).


    %%%%%%%%%%%%%%%%%%%
    % organizaEventos %
    %%%%%%%%%%%%%%%%%%%

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% filtraEventosPeriodo(ListaEventos, Periodo, EventosNoPeriodo)
% EventosNoPeriodo e uma lista dos EventoIDs pertencentes a
% ListaEventos que decorrem em Periodo.
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %

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

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% organizaEventos(ListaEventos, Periodo, EventosNoPeriodo)
% EventosNoPeriodo e uma lista ordenada sem repeticao 
% dos eventos da ListaEventos que ocorrem no Periodo
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
organizaEventos(ListaEventos, Periodo, EventosNoPeriodoSorted):-
    filtraEventosPeriodo(ListaEventos, Periodo, EventosNoPeriodo),
    sort(EventosNoPeriodo, EventosNoPeriodoSorted).


    %%%%%%%%%%%%%%%%%%%%%
    % eventosMenoresQue %
    %%%%%%%%%%%%%%%%%%%%%

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% eventoDuracao(Duracao, EventoID)
% Duracao e a Duracao do evento horario do EventoID
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
eventoDuracao(Duracao, EventoID):- horario(EventoID,_,_,_,Duracao,_).

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% eventosMenoresQueBool(EventoID, Duracao)
% EventoID e um evento com duracao menor ou igual a Duracao
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
eventosMenoresQueBool(EventoID, Duracao):- 
    eventoDuracao(DuracaoEvento, EventoID), 
    DuracaoEvento =< Duracao.

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% ListaEventos e uma lista ordenada e sem 
% elementos repetidos dos EventoIDs que teem uma duracao maior 
% ou igual a Duracao
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
eventosMenoresQue(Duracao, ListaEventosSorted):-
    findall(EventoID, eventosMenoresQueBool(EventoID, Duracao), ListaEventos),
    sort(ListaEventos, ListaEventosSorted).


    %%%%%%%%%%%%%%%%%%%%%%
    % procuraDisciplinas %
    %%%%%%%%%%%%%%%%%%%%%%

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% disciplinaCurso(Disciplina, Curso)
% Disciplina tem um turno para Curso
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
disciplinaCurso(Disciplina, Curso):- 
    turno(EventoID, Curso, _, _),
    evento(EventoID, Disciplina, _, _, _).

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% procuraDisciplinas(Curso, ListaDisciplinas)
% ListaDisciplinasSorted e a lista ordenada alfabeticamente
% do nome das disciplinas do Curso
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
procuraDisciplinas(Curso, ListaDisciplinasSorted):-
    findall(Disciplina, disciplinaCurso(Disciplina, Curso), ListaDisciplinas),
    sort(ListaDisciplinas, ListaDisciplinasSorted).


    %%%%%%%%%%%%%%%%%%%%%%%
    % organizaDisciplinas %
    %%%%%%%%%%%%%%%%%%%%%%%

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% disciplinaSemestre1(Curso, Disciplina) e
% disciplinaSemestre2(Curso, Disciplina)
% Disciplina tem um turno no Curso no respetivo semestre
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
disciplinaSemestre1(Curso, Disciplina):- 
    evento(EventoID, Disciplina, _, _, _), 
    turno(EventoID, Curso, _, _), 
    (eventoPeriodo(EventoID, p1); eventoPeriodo(EventoID, p2)).
disciplinaSemestre2(Curso, Disciplina):- 
    evento(EventoID, Disciplina, _, _, _), 
    turno(EventoID, Curso, _, _),
    (eventoPeriodo(EventoID, p3); eventoPeriodo(EventoID, p4)).

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% organizaDisciplinasAux(ListaDisciplinas, Curso, Semestres)
% predicado auxiliar - como organizaDisciplinas mas antes 
% da ordenacao alfabetica. Itera os elementos de ListaDisciplinas
% recursivamente e insere-os no respetivo semestre(s).
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
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

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% organizaDisciplinas(ListaDisciplinas, Curso, Semestres)
% Recorre ao predicado auxiliar organizaDisciplinasAux 
% Semestres e uma lista com duas listas ordenadas, a primeira sao os 
% elementos de ListaDisciplinas que ocorrem no primeiro semestre, 
% ou primeiro e segundo semestre, e na segunda lista os que ocorrem 
% somente no segundo
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
organizaDisciplinas(ListaDisciplinas, Curso, [Semestre1Sorted, Semestre2Sorted]):-
    organizaDisciplinasAux(ListaDisciplinas, Curso, [Semestre1, Semestre2]), !,
    sort(Semestre1, Semestre1Sorted),
    sort(Semestre2, Semestre2Sorted).


    %%%%%%%%%%%%%%
    % horasCurso %
    %%%%%%%%%%%%%%

eventosCursoPeriodoAno(EventoID, Periodo, Curso, Ano) :-
    turno(EventoID, Curso, Ano, _),
    eventoPeriodo(EventoID, Periodo).

horasCursoAux([], 0).
horasCursoAux([EventoID | ListaEventos], SomaHoras):-
    horasCursoAux(ListaEventos, PrevHoras),
    eventoDuracao(Duracao, EventoID),
    SomaHoras is Duracao + PrevHoras.

horasCurso(Periodo, Curso, Ano, TotalHoras):-
    ground((Periodo, Curso, Ano)),
    findall(EventoID, eventosCursoPeriodoAno(EventoID, Periodo, Curso, Ano), ListaEventos),
    sort(ListaEventos, ListaEventosSorted),
    horasCursoAux(ListaEventosSorted, TotalHoras).


    %%%%%%%%%%%%%%%%%%%%%%
    % evolucaoHorasCurso %
    %%%%%%%%%%%%%%%%%%%%%%

evolucaoHorasCursoAux(Curso, (Ano, Periodo, TotalHoras)):-
    % ground the variables Ano e Periodo
    turno(_, _, Ano, _),
    (Periodo = p1; Periodo = p2; Periodo = p3; Periodo = p4),
    horasCurso(Periodo, Curso, Ano, TotalHoras).

evolucaoHorasCurso(Curso, Evolucao) :- 
    setof(Tuplo, evolucaoHorasCursoAux(Curso, Tuplo), Evolucao).


    %%%%%%%%%%%%%
    % ocupaSlot %
    %%%%%%%%%%%%%

ocupaSlot(Inicio1, Fim1, Inicio2, Fim2, Overlap) :- 
    InicioMaior is max(Inicio1, Inicio2),
    FimMenor is min(Fim1, Fim2),
    FimMenor > InicioMaior,
    Overlap is FimMenor-InicioMaior.


    %%%%%%%%%%%%%%%%%%%%
    % numHorasOcupadas %
    %%%%%%%%%%%%%%%%%%%%

tipoSala(Tipo, Sala) :- 
    salas(Tipo, ListaSalas),
    member(Sala, ListaSalas).

eventoPeriodoSalaDia(EventoID, Periodo, TipoSala, DiaSemana):-
    evento(EventoID, _, _, _, Sala),
    tipoSala(TipoSala, Sala),
    horarioPeriodos(EventoID, DiaSemana, _, _, _, Periodo).

numHorasOcupadasAux([], _, _, 0).
numHorasOcupadasAux([EventoID | ListaEventos], InicioLim, FimLim, TotalHoras):-
    horario(EventoID, _, Inicio, Fim, _, _),
    (ocupaSlot(Inicio, Fim, InicioLim, FimLim, Overlap);
    Overlap is 0, \+ ocupaSlot(Inicio, Fim, InicioLim, FimLim, _)),
    numHorasOcupadasAux(ListaEventos, InicioLim, FimLim, PrevHoras),
    TotalHoras is PrevHoras + Overlap.

numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, TotalHoras):-
    setof(EventoID, eventoPeriodoSalaDia(EventoID, Periodo, TipoSala, DiaSemana), ListaEventos),
    numHorasOcupadasAux(ListaEventos, HoraInicio, HoraFim, TotalHoras).


    %%%%%%%%%%%%%%%
    % ocupacaoMax %
    %%%%%%%%%%%%%%%

ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max):-
    (ocupaSlot(HoraInicio, HoraFim, HoraInicio, HoraFim, Overlap), !;
    Overlap is 0),
    salas(TipoSala, ListaSalas),
    length(ListaSalas, NoSalas),
    Max is Overlap * NoSalas.


    %%%%%%%%%%%%%%%
    % percentagem %
    %%%%%%%%%%%%%%%

percentagem(Numerador, Denominador, Percentagem):-
    Percentagem is Numerador/Denominador * 100.


    %%%%%%%%%%%%%%%%%%%
    % ocupacaoCritica %
    %%%%%%%%%%%%%%%%%%%

ocupacaoCriticaAux(HoraInicio, HoraFim, Threshold, DiaSemana, TipoSala, RoundedPercent):-
    (Periodo = p1; Periodo = p2; Periodo = p3; Periodo = p4),
    numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, TotalHoras),
    ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max),
    percentagem(TotalHoras, Max, Percentagem),
    Percentagem > Threshold,
    RoundedPercent is ceiling(Percentagem).

ocupacaoCritica(HoraInicio, HoraFim, Threshold, ResultadosSorted):-
    findall(casosCriticos(DiaSemana, TipoSala, Percentagem), 
        ocupacaoCriticaAux(HoraInicio, HoraFim, Threshold, DiaSemana, TipoSala, Percentagem), 
        Resultados),
    sort(Resultados, ResultadosSorted).

    %%%%%%%%%%%%%%%%
    % ocupacaoMesa %
    %%%%%%%%%%%%%%%%

aplicaRestricao(cab1(X4),       [[ _,  _, _ ],[X4,  _],[ _,  _,  _]]).
aplicaRestricao(cab2(X5),       [[ _,  _, _ ],[ _, X5],[ _,  _,  _]]).
aplicaRestricao(honra(X4, X6),  [[ _,  _, _ ],[X4,  _],[X6,  _,  _]]).
aplicaRestricao(honra(X5, X3),  [[ _,  _, X3],[ _, X5],[ _,  _,  _]]).

aplicaRestricao(lado(Pessoa1, Pessoa2),  [[X1, X2, X3],[ _,  _],[X6, X7, X8]]):-
    (Pessoa1 = X2, (Pessoa2 = X1; Pessoa2 = X3));
    (Pessoa1 = X7, (Pessoa2 = X6; Pessoa2 = X8)).

aplicaRestricao(lado(Pessoa2, Pessoa1),  [[X1, X2, X3],[ _,  _],[X6, X7, X8]]):-
    (Pessoa1 = X2, (Pessoa2 = X1; Pessoa2 = X3));
    (Pessoa1 = X7, (Pessoa2 = X6; Pessoa2 = X8)).

aplicaRestricao(naoLado(Pessoa1, Pessoa2), Mesa):-
    \+ aplicaRestricao(lado(Pessoa1, Pessoa2), Mesa).

aplicaRestricao(frente(X1, X6), [[X1,  _,  _],[ _,  _],[X6,  _,  _]]).
aplicaRestricao(frente(X2, X7), [[ _, X2,  _],[ _,  _],[ _, X7,  _]]).
aplicaRestricao(frente(X3, X8), [[ _,  _, X3],[ _,  _],[ _,  _, X8]]).
aplicaRestricao(frente(X6, X1), [[X1,  _,  _],[ _,  _],[X6,  _,  _]]).
aplicaRestricao(frente(X7, X2), [[ _, X2,  _],[ _,  _],[ _, X7,  _]]).
aplicaRestricao(frente(X8, X3), [[ _,  _, X3],[ _,  _],[ _,  _, X8]]).

aplicaRestricao(naoFrente(Pessoa1, Pessoa2), Mesa):-
    \+ aplicaRestricao(frente(Pessoa1, Pessoa2), Mesa).

ocupacaoMesaAux(_, [], _).
ocupacaoMesaAux(ListaPessoas, [Regra | Resto], OcupacaoMesa):-
    aplicaRestricao(Regra, OcupacaoMesa),
    ocupacaoMesaAux(ListaPessoas, Resto, OcupacaoMesa).

membroLista(Lista, El):- member(El, Lista).

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% ocupacaoValida(ListaPessoas, Mesa)
% Mesa e uma lista de listas [[X1, X2, X3],[X4, X5],[X6, X7, X8]]
% tal que todos os X sao diferentes e membros de ListaPessoas
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
ocupacaoValida(ListaPessoas, [[X1, X2, X3],[X4, X5],[X6, X7, X8]]):-
    maplist(membroLista(ListaPessoas), [X1, X2, X3, X4, X5, X6, X7, X8]),
    % is_set e verdade se a lista nao tem duplicados
    is_set([X1, X2, X3, X4, X5, X6, X7, X8]).

% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
% ocupacaoMesa(ListaPessoas, ListaRegras, OcupacaoMesa)
% OcupacaoMes
% - + - - - + - - - + - - - + - - - + - - - + - - - + - - - + - %
ocupacaoMesa(ListaPessoas, [Regra | Resto], OcupacaoMesa):-
    ocupacaoValida(ListaPessoas, OcupacaoMesa),
    aplicaRestricao(Regra, OcupacaoMesa),
    ocupacaoMesaAux(ListaPessoas, Resto, OcupacaoMesa).
