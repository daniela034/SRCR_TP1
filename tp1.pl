%---------------------------------------------------------------------------------------------
% Sistemas de Representação de Conhecimento MIEI 3 

%---------------------------------------------------------------------------------------------
% Base de Conhecimento no universo da área de vacinação da covid da população portuguesa 

%---------------------------------------------------------------------------------------------
% Definições iniciais 

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).
:- op( 900, xfy,'::' ).

% utente : idUtente, nº segurança social, nome, data_nasc, email, telefone, morada, profissão, [doencas_cron], centro_saude -> {V,F}
:-dynamic utente/10.

% centro_saude: idCentro, nome, morada, telefone, email -> {V,F}
:-dynamic centro_saude/5.

% staff: idStaff, idCentro, Nome, email -> {V,F}
:-dynamic staff/4.

% vacinação_covid: staff, utente, data, vacina, toma -> {V,F}
:-dynamic vacinacao_covid/5.


%---------------------------------------------------------------------------------------------
% Extensão do predicado insere : Termo -> {V,F}
insere(P) :- assert(P).
insere(P) :- retract(P), !, fail.

%---------------------------------------------------------------------------------------------
% Extensão do predicado remove : Termo -> {V,F}
remove(P) :- retract(P).
remove(P) :- assert(P), !, fail.

%---------------------------------------------------------------------------------------------
% Extensão do predicado teste : Termo -> {V,F}
teste([]).
teste([X|R]) :- X, teste(R).

%---------------------------------------------------------------------------------------------
% Extensão do predicado solucoes : Termo, Questao, Resultado -> {V,F}
solucoes(P,Q,R) :- findall(P,Q,R).

%---------------------------------------------------------------------------------------------
% Extensão do predicado solucoesSRep : Termo, Questao, Resultado -> {V,F}
solucoesSRep(X,Y,Z) :- setof(X,Y,Z).

%---------------------------------------------------------------------------------------------
% Extensão do predicado evolucao : Termo -> {V,F}
evolucao(T) :- solucoes(I, +T :: I, S), 
            insere(T),
            teste(S).

%---------------------------------------------------------------------------------------------
% Extensão do predicado involucao : Termo -> {V,F}
involucao(T) :- solucoes(I, -T::I, S), 
                remove(T),
                teste(S).

%---------------------------------------------------------------------------------------------
% Extensão do predicado comprimento : Lista,Resultado -> {V,F}
comprimento(S,N) :- length(S,N).


nao(P) :- call(P), !, fail.
nao(P).


%----------------------------------------------------------------------------------------------
% Extensão do predicado utente : idUtente, nº segurança social, nome, data_nasc, email, telefone, morada, profissão, [doencas_cron], centro_saude -> {V,F}
utente(1, 123477, eduardo, date(1988,01,30), 'eduardodf@gorj.pt', 932827487, braga, policia, [diabetes,hipertensao], centro_saude_gualtar).
utente(2, 623426, fernanda, date(1997,06,05), 'fernandinha@hotmail.com',918826356, braga, medico, [cancro], centro_saude_caranda).
utente(3, 632427, sofia, date(1972,10,02), 'sofsieh@gmail.com', 92937484, fafe, enfermeiro, [], centro_saude_fafe).
utente(4, 928323, marco,  date(2000,08,26), 'marcolol@outlook.pt', 918362758, vila_real, estudante, [], centro_saude_vilareal).
utente(5, 627848, alice, date(1968,12,26), 'alice1968@msn.pt', 912138366, vila_verde, professor, [diabetes], centro_saude_vilaverde ).
utente(6, 974837, carina, date(1980,10,04), 'carinaporto@hotmail.pt', 913738633, esposende, lojista, [coracao], centro_saude_esposende).
utente(7, 353347, jose, date(1977,04,29), 'josejosejose@outlook.pt', 917623527, vila_do_conde, gerente,[], centro_saude_viladoconde).

%----------------------------------------------------------------------------------------------
% Extensão do predicado centro_saude: idCentro, nome, morada, telefone, email -> {V,F}
centro_saude(1, centro_saude_gualtar, braga, 253764648, 'gualtar@csb.pt').
centro_saude(2, centro_saude_caranda, braga, 253847495, 'caranda@csb.pt').
centro_saude(3, centro_saude_fafe, fafe, 252746433, 'fafe@csf.pt').
centro_saude(4, centro_saude_vilareal, vila_real, 251763389, 'vilareal@csvr.pt').
centro_saude(5, centro_saude_esposende, esposende, 253713748, 'esposende@cse.pt').
centro_saude(6, centro_saude_viladoconde, vila_do_conde, 254635327, 'viladoconde@csvdc.pt').
centro_saude(7, centro_saude_vilaverde, vila_verde, 254635327, 'vilaverde@csvv.pt').

%----------------------------------------------------------------------------------------------
% Extensão do predicado staff: idStaff, idCentro, Nome, email -> {V,F}
staff(1,4, carla, 'carla_vilareal@csvr.pt').
staff(2,5, helena, 'helena_esposende@cse.pt').
staff(3,3, artur, 'artur_fafe@csf.pt' ).
staff(4,1, simao, 'simao_gualtar@csb.pt').
staff(5,2, elisabete, 'elisabete_caranda@csb.pt').
staff(6,7, carmo, 'carmo_vilaverde@csvv.pt').

%----------------------------------------------------------------------------------------------
% Extensão do predicado vacinação_covid: idstaff, idutente, data, vacina, toma -> {V,F}
vacinacao_covid(4, 1,  '10-03-2021', 'Pfizer', 1).
vacinacao_covid(5, 2, '28-12-2020', 'Moderna',2).
vacinacao_covid(3, 3, '05-04-2021', 'AstraZeneca',1).
vacinacao_covid(1, 4, '05-06-2021', 'Moderna', 0).
vacinacao_covid(6, 5, '10-05-2021', 'AstraZeneca',0).
vacinacao_covid(2, 6, '06-02-2021', 'Pfizer',2).

%----------------------------------------------------------------------------------------------
% Não permite que existam IDs iguais nos utentes 

+utente(ID,S,No,D,E,T,M,P,L,C) :: (solucoes((ID),(utente(ID,A,B,C,F,G,H,I,J,K)),S),
                                    comprimento(S,N), N =< 1).

% Não permite que existam IDs iguais nos centros de centro_saude

+centro_saude(ID,No,M,T,E) :: (solucoes((ID), (centro_saude(ID,X,Y,Z,W)),S)
                                comprimento(S,N), N =< 1).

% Não permite que existem IDs iguais no satff 

+staff(ID,I,No,E) :: (solucoes((ID), (staff(ID,X,Y,Z)), S),
                    comprimento(S,N), N =< 1).

% Não permite que remova utente se estiver na vacinacao_covid

-utente(ID,SS,N,D,E,T,M,P,L,C) :: (solucoes((ID), (vacinacao_covid(IDs, ID, DT, V, T)),S),
                                    comprimento(S,N), N==0).


% Não permite que remova staff se estiver na vacinacao_covid

-staff(ID,IDu,N,E) :: (solucoes((ID), (vacinacao_covid(ID, IDu, DT, V, T)),S),
                     comprimento(S,N), N==0).

% Não permite inserir utente em  vacinacao_covid se não estiver na base de conhecimento 

+vacinacao_covid(IDs,IDu,D,V,T) :: (solucoes((IDu), (utente(IDu,Ss,No,D,E,T,M,P,L,Cs)),S),
                                    comprimento(S,N), N == 1).

% Não permite inserir staff em vacinacao_covid se nao estiver na base de conhecimento

+vacinacao_covid(IDs,IDu,D,V,T) :: (solucoes((IDs), (staff(IDs,IDu,No,E)),S),
                                    comprimento(S,N), N == 1).

%----------------------------------------------------------------------------------------------
% Extensão do predicado fasesvacinacao : Criterio, Operador, Valor, Resultado -> {V,F}
% Lista de utentes que respeitam determinado critério 
% 1 - Data de Nascimento 
% 2 - Profissão 
% 3 - Doenças Cronicas
% 4 - Data Vacinação

% A data tem que ter o formato correto, para idosos com mais de 71 anos 
+date(A,M,D)::(date(A,M,D),
    A=<1950,
    D>=1, (
    (member(M, [1,3,5,7,8,10,12]), D=<31);
    (member(M, [2]), D=<28);
    (member(M, [2]), 0 is mod(A,4), D=<29);
    (member(M, [4,6,9,11]), D=<30))
).


% Extensão do predicado que identifica utentes pela sua data de Nascimento
fasesvacinacao(1,<,V,R) :- solucoes((ID,SS,NM,D,E,T,M,P,L,CS), (utente(ID,SS,NM,D,E,T,M,P,L,CS), D < V), R).


% Extensão do predicado que identifica utentes pela sua profissão

fasesvacinacao(2,=,V,R) :- solucoes((ID,SS,NM,D,E,T,M,P,L,CS), (utente(ID,SS,NM,D,E,T,M,P,L,CS)), R).

% Extensão do predicado que identifica utentes pelas doenças crónicas 

fasesvacinacao(3,=,V,R) :- solucoes((ID,SS,NM,D,E,T,M,P,L,CS), (utente(ID,SS,NM,D,E,T,M,P,L,CS)), R).

% Extensão da data de vacinação

fasesvacinacao(4,=,V,R) :-
	solucoesSRep((ID,SS,NM,D,E,T,M,P,L,CS), IDS^IDU^DT^V^T^(vacinacao_covid(IDS,IDU,DT,V,T), utente(ID,SS,NM,D,E,T,M,P,L,CS)), R).


%----------------------------------------------------------------------------------------------
% Lista os utentes que não foram vacinados 
% Extensão do predicado naovacinados :  IDutente, Resultado -> {V,F}

naovacinados(R) :- solucoes(utente(ID, SS, NM, D, E, TLF, M, P, L, CS), (utente(ID, SS, NM, D, E, TLF, M, P, L, CS), nao(vacinacao_covid(_, ID, _, _, _))), R).

%----------------------------------------------------------------------------------------------
% Lista os utentes que já foram vacinados  Toma = 1 e Toma = 2
% Extensão do predicado vacinadas : Toma, Resultado -> {V,F}

vacinadas(T, R) :- solucoes(utente(ID, SS, NM, D, E, TLF, M, P, L, CS), (utente(ID, SS, NM, D, E, TLF, M, P, L, CS), vacinacao_covid(_, ID, _, _, T)), R).

%----------------------------------------------------------------------------------------------
% Lista os utentes que foram vacinados indevidamente (ñ cumprem os critérios)
% Extensão do predicado vacinadasind : 
% 1 - Data de Nascimento 
% 2 - Profissão 
% 3 - Doenças Cronicas

profissaoRisco(medico).
profissaoRisco(policia).
profissaoRisco(enfermeiro).
profissaoRisco(bombeiro).
profissaoRisco(auxiliar_saude).

vacinadasInd(R) :- solucoes((ID,SS,NM,D,E,T,M,P,[],CS), (utente(ID,SS,NM,D,E,T,M,P,[],CS), vacinacao_covid(_, ID, _, _, _), nao(profissaoRisco(P))), R).

%----------------------------------------------------------------------------------------------
% Lista os utentes que ainda não foram vacinamos mas são candidatos, Toma = 0 ou Toma = 1
% Extensão do predicado candidatos : Toma, Resultado -> {V,F}

candidatos(N, R) :- solucoes(utente(ID,SS,NM,D,E,T,M,P,L,CS), (utente(ID,SS,NM,D,E,T,M,P,L,CS), vacinacao_covid(_, ID, _, _, N)), R).

%----------------------------------------------------------------------------------------------
% Lista os staff que deram a vacina 
% Extensão do predicado prestadores : Resultado -> {V,F}

prestadores(R) :- solucoesSRep(IDS, IDU^DT^V^T^vacinacao_covid(IDS,IDU,DT,V,T),R).


%----------------------------------------------------------------------------------------------
% Lista de vacinas dados por um determinado staff
% Extensão do predicado vacinasStaff : N, Resultado -> {V,F}

vacinasStaff(N,R) :- solucoes((N, IdU, D, V, T), vacinacao_covid(N, IdU, D, V, T), R).

%----------------------------------------------------------------------------------------------
% Lista de centros de saúde
% Extensão do predicado listaCentros : Resultado -> {V,F}

listaCentros(R) :- solucoesSRep((Id, N, M, T, E), centro_saude(Id, N, M, T, E), R).

%----------------------------------------------------------------------------------------------
% Lista de utentes
% Extensão do predicado listaUtentes : Resultado -> {V,F}

listaUtentes(R) :- solucoes(utente(ID,SS,NM,D,E,T,M,P,L,CS), utente(ID,SS,NM,D,E,T,M,P,L,CS), R).

%----------------------------------------------------------------------------------------------
% Lista de staff
% Extensão do predicado listaStaff : Resultado -> {V,F}

listaStaff(R) :- solucoes((IdS, IdC, N, E), staff(IdS, IdC, N, E), R).

%----------------------------------------------------------------------------------------------
% Lista de staff
% Extensão do predicado listaVac : Resultado -> {V,F}

listaVac(R) :- solucoes((IdS, IdU, D, Vac, T), vacinacao_covid(IdS, IdU, D, Vac, T), R).

%----------------------------------------------------------------------------------------------
% Extensão do predicado atualizaToma: IdU -> {V,F}
% Alteração da toma do utente

atualizaToma(IdU) :- 
    solucoes((IdS, IdU, Data, Vac, Toma), vacinacao_covid(IdS, IdU, Data, Vac, Toma), [(IdSR, IdUR, DataR, VacR, TomaR)|T]),
    remove(vacinacao_covid(IdSR, IdUR, DataR, VacR, TomaR)),
    insere(vacinacao_covid(IdSR, IdUR, DataR, VacR, 2)).

%----------------------------------------------------------------------------------------------
% Extensão do predicado atualizaMorada: IdU -> {V,F}
% Alteração da morada do utente

atualizaMorada(ID, Morada) :-
    solucoes((ID,SS,NM,D,E,Toma,M,P,L,CS), utente(ID,SS,NM,D,E,Toma,M,P,L,CS), [(IDR,SSR,NMR,DR,ER,TomaR,MR,PR,LR,CSR)|T]),
    remove(utente(IDR,SSR,NMR,DR,ER,TomaR,MR,PR,LR,CSR)),
    insere(utente(ID,SSR,NMR,DR,ER,TomaR,Morada,PR,LR,CSR)).

%----------------------------------------------------------------------------------------------
% Extensão do predicado atualizaCentro: IdU -> {V,F}
% Alteração do centro de saude do utente

atualizaCentro(ID, CentroS) :-
    solucoes((ID,SS,NM,D,E,Toma,M,P,L,CS), utente(ID,SS,NM,D,E,Toma,M,P,L,CS), [(IDR,SSR,NMR,DR,ER,TomaR,MR,PR,LR,CSR)|T]),
    remove(utente(IDR,SSR,NMR,DR,ER,TomaR,MR,PR,LR,CSR)),
    insere(utente(ID,SSR,NMR,DR,ER,TomaR,MR,PR,LR,CentroS)).
