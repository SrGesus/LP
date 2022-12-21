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
eventosSemSalasDiaSemanaAux(DiaSemana, EventoID):-
    eventoSemSala(EventoID), horario(EventoID, DiaSemana, _, _, _, _).

% e verdade se ListaEventos e uma lista, 
% ordenada e sem elementos repetidos, de IDs de eventos sem sala,
% que decorrem em DiaSemana.
eventosSemSalasDiaSemana(DiaSemana, ListaEventosSorted):-
    findall(EventoID, eventosSemSalasDiaSemanaAux(DiaSemana, EventoID), ListaEventos),
    sort(ListaEventos, ListaEventosSorted).


    %%%%%%%%%%%%%%%%%%%%%%%%%%
    % eventosSemSalasPeriodo %
    %%%%%%%%%%%%%%%%%%%%%%%%%%

% horarioPeriodos para p1 e p2 e verdade se horario para p1_2 e verdade
% horarioPeriodos para p3 e p4 e verdade se horario para p3_4 e verdade
horarioPeriodos(A,B,C,D,E,F):- horario(A,B,C,D,E,F).
horarioPeriodos(A,B,C,D,E,p1):- horario(A,B,C,D,E,p1_2).
horarioPeriodos(A,B,C,D,E,p2):- horario(A,B,C,D,E,p1_2).
horarioPeriodos(A,B,C,D,E,p3):- horario(A,B,C,D,E,p3_4).
horarioPeriodos(A,B,C,D,E,p4):- horario(A,B,C,D,E,p3_4).

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
disciplinaSemestre1(Curso, Disciplina):- 
    evento(EventoID, Disciplina, _, _, _), 
    turno(EventoID, Curso, _, _), 
    (eventoPeriodo(EventoID, p1); eventoPeriodo(EventoID, p2)).
disciplinaSemestre2(Curso, Disciplina):- 
    evento(EventoID, Disciplina, _, _, _), 
    turno(EventoID, Curso, _, _),
    (eventoPeriodo(EventoID, p3); eventoPeriodo(EventoID, p4)).

% caso terminal
organizaDisciplinasAux([], _, [[],[]]).
% caso evento tenha Disciplina no Curso no semestre 1
organizaDisciplinasAux([Disciplina| ListaDisciplinas], Curso, [[Disciplina| Semestre1], Semestre2]):- 
    disciplinaSemestre1(Curso, Disciplina),
    % \+ disciplinaSemestre2(Curso, Disciplina),
    organizaDisciplinasAux(ListaDisciplinas, Curso, [Semestre1, Semestre2]).
% caso evento tenha Disciplina no Curso no semestre 2
organizaDisciplinasAux([Disciplina| ListaDisciplinas], Curso, [Semestre1, [Disciplina| Semestre2]]):- 
    disciplinaSemestre2(Curso, Disciplina),
    \+ disciplinaSemestre1(Curso, Disciplina),
    organizaDisciplinasAux(ListaDisciplinas, Curso, [Semestre1, Semestre2]).
% caso evento tenha Disciplina no Curso no semestre 1 e 2
% organizaDisciplinasAux([Disciplina| ListaDisciplinas], Curso, [[Disciplina| Semestre1], [Disciplina | Semestre2]]):- 
%     disciplinaSemestre1(Curso, Disciplina),
%     disciplinaSemestre2(Curso, Disciplina),
%     organizaDisciplinasAux(ListaDisciplinas, Curso, [Semestre1, Semestre2]).

organizaDisciplinas(ListaDisciplinas, Curso, [Semestre1Sorted, Semestre2Sorted]):-
    organizaDisciplinasAux(ListaDisciplinas, Curso, [Semestre1, Semestre2]), !,
    sort(Semestre1, Semestre1Sorted),
    sort(Semestre2, Semestre2Sorted).


    %%%%%%%%%%%%%%
    % horasCurso %
    %%%%%%%%%%%%%%

eventosCursoPeriodoAno(EventoID, Periodo, Curso, Ano) :-
    turno(EventoID, Curso, Ano, _),
    horarioPeriodos(EventoID, _, _, _, _, Periodo).

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
    % ground the variables Curso, Ano e Periodo
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
    (ocupaSlot(Inicio, Fim, InicioLim, FimLim, Overlap), !;
    Overlap is 0),
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
    findall(casosCriticos(DiaSemana, TipoSala, Percentagem), ocupacaoCriticaAux(HoraInicio, HoraFim, Threshold, DiaSemana, TipoSala, Percentagem), Resultados),
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

aplicaRestricao(naoLado(Pessoa1, Pessoa2), [[X1, X2, X3],[X4, X5],[X6, X7, X8]]):-
    % se Pessoa 1 estiver na cabeceira, Pessoa 2 pode estar em qualquer outro lugar
    (Pessoa1 = X4, 
        (Pessoa2 = X1; Pessoa2 = X2; Pessoa2 = X3; 
        Pessoa2 = X5; Pessoa2 = X6; Pessoa2 = X7; Pessoa2 = X8));
    (Pessoa1 = X5, 
        (Pessoa2 = X1; Pessoa2 = X2; Pessoa2 = X3; 
        Pessoa2 = X4; Pessoa2 = X6; Pessoa2 = X7; Pessoa2 = X8));
    % se estiver num lado, Pessoa2 pode estar no outro
    ((Pessoa1 = X1; Pessoa1 = X2; Pessoa1 = X3), (Pessoa2 = X6; Pessoa2 = X7; Pessoa2 = X8));
    % se estiver na ponta de uma lado, Pessoa2 pode estar na outra
    (Pessoa1 = X1, Pessoa2 = X3);
    (Pessoa1 = X6, Pessoa2 = X8).

% o mesmo trocando Pessoa1 e 2
aplicaRestricao(naoLado(Pessoa2, Pessoa1), [[X1, X2, X3],[X4, X5],[X6, X7, X8]]):-
    (Pessoa1 = X4, 
        (Pessoa2 = X1; Pessoa2 = X2; Pessoa2 = X3; 
        Pessoa2 = X5; Pessoa2 = X6; Pessoa2 = X7; Pessoa2 = X8));
    (Pessoa1 = X5, 
        (Pessoa2 = X1; Pessoa2 = X2; Pessoa2 = X3; 
        Pessoa2 = X4; Pessoa2 = X6; Pessoa2 = X7; Pessoa2 = X8));
    ((Pessoa1 = X1; Pessoa1 = X2; Pessoa1 = X3), (Pessoa2 = X6; Pessoa2 = X7; Pessoa2 = X8));
    (Pessoa1 = X1, Pessoa2 = X3);
    (Pessoa1 = X6, Pessoa2 = X8).

aplicaRestricao(frente(X1, X6), [[X1,  _,  _],[ _,  _],[X6,  _,  _]]).
aplicaRestricao(frente(X2, X7), [[ _, X2,  _],[ _,  _],[ _, X7,  _]]).
aplicaRestricao(frente(X3, X8), [[ _,  _, X3],[ _,  _],[ _,  _, X8]]).
aplicaRestricao(frente(X6, X1), [[X1,  _,  _],[ _,  _],[X6,  _,  _]]).
aplicaRestricao(frente(X7, X2), [[ _, X2,  _],[ _,  _],[ _, X7,  _]]).
aplicaRestricao(frente(X8, X3), [[ _,  _, X3],[ _,  _],[ _,  _, X8]]).

aplicaRestricao(naoFrente(Pessoa1, Pessoa2), [[X1, X2, X3],[X4, X5],[X6, X7, X8]]):-
    (Pessoa1 = X4; Pessoa1 = X5);
    % se estiver no mesmo lado nao esta a frente
    ((Pessoa1 = X1; Pessoa1 = X2; Pessoa1 = X3),
     (Pessoa2 = X1; Pessoa2 = X2; Pessoa2 = X3));
    ((Pessoa1 = X6; Pessoa1 = X7; Pessoa1 = X8),
      (Pessoa2 = X6; Pessoa2 = X7; Pessoa2 = X8));
    (Pessoa1 = X1, (Pessoa2 = X7; Pessoa2 = X8));
    (Pessoa1 = X2, (Pessoa2 = X6; Pessoa2 = X8));
    (Pessoa1 = X3, (Pessoa2 = X6; Pessoa2 = X7)).

aplicaRestricao(naoFrente(Pessoa2, Pessoa1), [[X1, X2, X3],[X4, X5],[X6, X7, X8]]):-
    (Pessoa1 = X4; Pessoa1 = X5);
    (Pessoa1 = X1, (Pessoa2 = X7; Pessoa2 = X8));
    (Pessoa1 = X2, (Pessoa2 = X6; Pessoa2 = X8));
    (Pessoa1 = X3, (Pessoa2 = X6; Pessoa2 = X7)).

ocupacaoMesaAux(_, [], _).
ocupacaoMesaAux(ListaPessoas, [Regra | Resto], OcupacaoMesa):-
    aplicaRestricao(Regra, OcupacaoMesa),
    ocupacaoMesaAux(ListaPessoas, Resto, OcupacaoMesa).

pessoasEmFalta([], _).
pessoasEmFalta([Pessoa|Resto], Mesa):-
    member(Pessoa, Mesa),
    pessoasEmFalta(Resto, Mesa).

ocupacaoValida(ListaPessoas, [[X1, X2, X3],[X4, X5],[X6, X7, X8]]):-
    sort([X1, X2, X3, X4, X5, X6, X7, X8], ListaSemRepetidos),
    length(ListaSemRepetidos, LenSemRepetidos),
    length([X1, X2, X3, X4, X5, X6, X7, X8], LenNormal),
    LenSemRepetidos = LenNormal,
    pessoasEmFalta(ListaPessoas, [X1, X2, X3, X4, X5, X6, X7, X8]).

ocupacaoMesa(ListaPessoas, [Regra | Resto], OcupacaoMesa):-
    aplicaRestricao(Regra, OcupacaoMesa),
    ocupacaoMesaAux(ListaPessoas, Resto, OcupacaoMesa),
    ocupacaoValida(ListaPessoas, OcupacaoMesa).