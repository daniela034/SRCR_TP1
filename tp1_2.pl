%---------------------------------------------------------------------------------------------
% Sistemas de Representação de Conhecimento MIEI 3 

%---------------------------------------------------------------------------------------------
% Base de Conhecimento no universo da área de vacinação da covid da população portuguesa 

%---------------------------------------------------------------------------------------------
% SICStus PROLOG: declarações iniciais
:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%---------------------------------------------------------------------------------------------
% SICStus PROLOG: definições iniciais 

:- op( 900,xfy,'::' ).
:- op( 900,xfy,'e' ). 
:- op( 900,xfy,'ou' ). 
:- dynamic '-'/1.
:- dynamic excecao/1.
:- dynamic listing/1.
:-dynamic utente/10.
:-dynamic centro_saude/5.
:-dynamic staff/4.
:-dynamic vacinacao_covid/5.
:- dynamic (::)/2.




%---------------------------------------------------------------------------------------------
% Extensão do predicado que permite a inserção do conhecimento: Termo -> {V,F}

insere(P) :- assert(P). 
insere(P) :- retract(P), !, fail.

%---------------------------------------------------------------------------------------------
% Extensão do predicado que permite a remoção do conhecimento: Termo -> {V,F}

remove(P) :- retract(P).
remove(P) :- assert(P), !, fail.

%---------------------------------------------------------------------------------------------
% Extensão do predicado que permite a remoção do conhecimento numa lista: Lista -> {V,F}

removeL([H]) :- remove(H).
removeL([X|T]) :- remove(H), removeL(T).

%---------------------------------------------------------------------------------------------
% Extensão do predicado que permite a testar do conhecimento: Lista -> {V,F}

teste([]).
teste([X|R]) :- X, teste(R).

%---------------------------------------------------------------------------------------------
%Extensão do predicado soluções : Termo, Questao, Resultado -> {V,F}

solucoes(X,Y,Z) :- findall(X,Y,Z). 

%---------------------------------------------------------------------------------------------
%Extensão do predicado soluções repetidas : Termo, Questao, Resultado -> {V,F}

solucoesSRep(X,Y,Z) :- setof(X,Y,Z).

%---------------------------------------------------------------------------------------------
% Extensão do predicado que permite a evolução do conhecimento: Termo -> {V,F}

evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    atualiza( Termo ),
    teste( Lista ).

insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ),!,fail.

%---------------------------------------------------------------------------------------------
% Extensão do predicado que permite a involução do conhecimento: Termo -> {V,F}

involucao( Termo ) :-
    solucoes( Invariante,-Termo::Invariante,Lista ),
    remocao( Termo ),
    teste( Lista ).

remocao( Termo ) :-
    retract( Termo ).
remocao( Termo ) :-
    assert( Termo ),!,fail.

%---------------------------------------------------------------------------------------------
% Extensão do meta-predicado demo: Questao, Resposta -> {V,F}

demo(Questao,verdadeiro) :- Questao.
demo(Questao,falso) :- -Questao.
demo(Questao,desconhecido) :- nao(Questao), nao(-Questao).

%---------------------------------------------------------------------------------------------
% Extensão do meta-predicado demoC: CQuestao, Resposta -> {V,F}

demoC(Q1 e Q2, R) :- demo(Q1,R1), demoC(Q2,R2), conjuncao(R1,R2,R).
demoC(Q1 ou Q2, R) :- demo(Q1,R1), demoC(Q2,R2), disjuncao(R1,R2,R).
demoC(Q,R) :- demo(Q,R).

%---------------------------------------------------------------------------------------------
% Extensão do predicado conjuncao : Resposta1, Resposta2, Resultado -> {V,F}

conjuncao(verdadeiro,verdadeiro,verdadeiro).
conjuncao(verdadeiro,falso,falso).
conjuncao(falso,verdadeiro,falso).
conjuncao(falso,falso,falso).
conjuncao(desconhecido,desconhecido,desconhecido).
conjuncao(desconhecido,verdadeiro,desconhecido).
conjuncao(verdadeiro,desconhecido,desconhecido).
conjuncao(desconhecido,falso,falso).
conjuncao(falso,desconhecido,falso).

%---------------------------------------------------------------------------------------------
% Extensão do predicado disjuncao : Resposta1, Resposta2, Resultado -> {V,F}

disjuncao(verdadeiro,verdadeiro,verdadeiro).
disjuncao(verdadeiro,falso,verdadeiro).
disjuncao(falso,verdadeiro,verdadeiro).
disjuncao(falso,falso,falso).
disjuncao(desconhecido,desconhecido,desconhecido).
disjuncao(desconhecido,verdadeiro,verdadeiro).
disjuncao(verdadeiro,desconhecido,verdadeiro).
disjuncao(desconhecido,falso,desconhecido).
disjuncao(falso,desconhecido,desconhecido).

%---------------------------------------------------------------------------------------------
% Extensão do meta-predicado nao: Questao -> {V,F}

nao(Questao) :- Questao, !, fail.
nao(Questao).

%---------------------------------------------------------------------------------------------
% Extensao do predicado comprimento : Lista, Resultado -> {V,F}

comprimento(S,N) :- length(S,N).

%---------------------------------------------------------------------------------------------
% Extensão do meta-predicado atualiza: Termo -> {V,F}

% Conhecimento positivo -> Conhecimento positivo (utente)
atualiza(utente(ID,S,No,D,E,T,M,P,L,C)) :- 
                nao(utente(ID,S,No,D,E,T,M,P,L,C)), 
                nao(excecao(utente(ID,S,No,D,E,T,M,P,L,C))),
                solucoes((utente(ID,_,_,_,_,_,_,_,_,_)),
                        (utente(ID,_,_,_,_,_,_,_,_,_)), 
                        R),
                remove(R), 
                insere(utente(ID,S,No,D,E,T,M,P,L,C)).

% Conhecimento positivo -> Conhecimento positivo (centro_saude)
atualiza(centro_saude(ID,No,M,T,E)) :-
                nao(centro_saude(ID,No,M,T,E)),
                nao(excecao(centro_saude(ID,No,M,T,E))),
                solucoes((centro_saude(ID,_,_,_,_)),
                        (centro_saude(ID,_,_,_,_)),
                        R),
                remove(R),
                insere(centro_saude(ID,No,M,T,E)).

% Conhecimento positivo -> Conhecimento positivo (satff)
atualiza(staff(ID,I,No,E)) :- 
                nao(staff(ID,I,No,E)),
                nao(excecao(staff(ID,I,No,E))),
                solucoes((staff(ID,_,_,_)),
                        staff(ID,_,_,_),
                        R),
                remove(R),
                insere(staff(ID,I,No,E)).



% Conhecimento incerto/impreciso -> Conhecimento positivo
atualiza(Q) :- demo(Q,desconhecido),
            solucoes(C,
                    (cImperfeito(Q,C)),
                    R),
            remove(R),
            insere(Q).

% Conhecimento impreciso sem intervalo -> Conhecimento negativo 
atualiza(-Q) :- clause(excecao(Q),true),
                remove(excecao(Q)),
                insere(-Q).

% Conhecimento negativo -> Conhecimento positivo
atualiza(Q) :- clause(-Q,true),
            remove(-Q), 
            insere(Q).

% Conhecimento positivo -> Conhecimento negativo
atualiza(-Q) :- solucoes(Q, excecao(Q), S), 
                comprimento(S,N),
                N == 0, 
                clause(Q,true),
                remove(Q), 
                insere(-Q).


% Conhecimento positivo/negativo novo
atualiza(Q) :- insere(Q).

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
% Extensao da negação forte do predicado utente 

-utente(ID,S,No,D,E,T,M,P,L,C) :- nao(utente(ID,S,No,D,E,T,M,P,L,C)),
                                nao(excecao(utente(ID,S,No,D,E,T,M,P,L,C))).


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
% Extensao da negação forte do predicado centro_saude

-centro_saude(ID,No,M,T,E) :- nao(centro_saude(ID,No,M,T,E)), 
                            nao(excecao(centro_saude(ID,No,M,T,E))).


%----------------------------------------------------------------------------------------------
% Extensão do predicado staff: idStaff, idCentro, Nome, email -> {V,F}
staff(1,4, carla, 'carla_vilareal@csvr.pt').
staff(2,5, helena, 'helena_esposende@cse.pt').
staff(3,3, artur, 'artur_fafe@csf.pt' ).
staff(4,1, simao, 'simao_gualtar@csb.pt').
staff(5,2, elisabete, 'elisabete_caranda@csb.pt').
staff(6,7, carmo, 'carmo_vilaverde@csvv.pt').

%----------------------------------------------------------------------------------------------
% Extensao da negação forte do predicado staff

-staff(ID,I,No,E) :- nao(staff(ID,I,No,E)),
                    nao(excecao(staff(ID,I,No,E))).

%----------------------------------------------------------------------------------------------
% Extensão do predicado vacinação_covid: idstaff, idutente, data, vacina, toma -> {V,F}
vacinacao_covid(4, 1,  '10-03-2021', 'Pfizer', 1).
vacinacao_covid(5, 2, '28-12-2020', 'Moderna',2).
vacinacao_covid(3, 3, '05-04-2021', 'AstraZeneca',1).
vacinacao_covid(1, 4, '05-06-2021', 'Moderna', 0).
vacinacao_covid(6, 5, '10-05-2021', 'AstraZeneca',0).
vacinacao_covid(2, 6, '06-02-2021', 'Pfizer',2).

%---------------------------------------------------------------------------------------------
% Extensao da negação forte do predicado vacinacao_covid

-vacinacao_covid(IDs,IDu,D,V,T) :- nao(vacinacao_covid(IDs,IDu,D,V,T)),
                                   nao(excecao(vacinacao_covid(IDs,IDu,D,V,T))).


%---------------------------------------------------------------------------------------------
% Extensão do conhecimento incerto utente

% utente id - x001
utente(x001, 152635, josue, date(1976,04,19), 'josueeee@ahgs.pt', 927643769, porto, bombeiro, [], centro_saude_porto).
excecao(utente(ID,S,No,D,E,T,M,P,L,C)) :- utente(x001,S,No,D,E,T,M,P,L,C).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(x001,S,No,D,E,T,M,P,L,C), 
                                            R = (utente(x001,S,No,D,E,T,M,P,L,C)).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(x001,S,No,D,E,T,M,P,L,C) ,
                                            R = (excecao(utente(x001,S,No,D,E,T,M,P,L,C)) :- utente(x001,S,No,D,E,T,M,P,L,C)).                                            

% utente nº segurança social - x002 

utente(8, x002, bruna, date(1997,03,01), 'bruna@hotmail.pt', 972564524, lisboa, estudante, [], centro_saude_lisboa).
excecao(utente(ID,S,No,D,E,T,M,P,L,C)) :- utente(ID,x002,No,D,E,T,M,P,L,C).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(ID,x002,No,D,E,T,M,P,L,C), 
                                            R = (utente(ID,x002,No,D,E,T,M,P,L,C)).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(ID,x002,No,D,E,T,M,P,L,C) ,
                                            R = (excecao(utente(ID,x002,No,D,E,T,M,P,L,C)) :- utente(ID,x002,No,D,E,T,M,P,L,C)). 


% utente nome - x003
utente(9, 152635, x003, date(1976,04,19), 'ariana@gmail.pt', 982327543, famalicao, secretaria, [], centro_saude_famalicao).
excecao(utente(ID,S,No,D,E,T,M,P,L,C)) :- utente(ID,S,x003,D,E,T,M,P,L,C).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(ID,S,x003,D,E,T,M,P,L,C), 
                                            R = (utente(ID,S,x003,D,E,T,M,P,L,C)).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(ID,S,x003,D,E,T,M,P,L,C) ,
                                            R = (excecao(utente(ID,S,x003,D,E,T,M,P,L,C)) :- utente(ID,S,x003,D,E,T,M,P,L,C)).

% utente Data_nasc - x004 
utente(9, 152635, rofolfo, x004, 'rod@gmail.pt', 93743547, barcelos, empilhador, [], centro_saude_barcelos).
excecao(utente(ID,S,No,D,E,T,M,P,L,C)) :- utente(ID,S,No,x004,E,T,M,P,L,C).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(ID,S,No,x004,E,T,M,P,L,C), 
                                            R = (utente(ID,S,No,x004,E,T,M,P,L,C)).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(ID,S,No,x004,E,T,M,P,L,C) ,
                                            R = (excecao(utente(ID,S,No,x004,E,T,M,P,L,C)) :- utente(ID,S,No,x004,E,T,M,P,L,C)).

% utente email - x005 
utente(10, 152635, alberto, date(1960,08,09), x005, 937365434, povoa, empilhador, [], centro_saude_povoa).
excecao(utente(ID,S,No,D,E,T,M,P,L,C)) :- utente(ID,S,No,D,x005,T,M,P,L,C).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(ID,S,No,D,x005,T,M,P,L,C), 
                                            R = (utente(ID,S,No,D,x005,T,M,P,L,C)).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(ID,S,No,D,x005,T,M,P,L,C) ,
                                            R = (excecao(utente(ID,S,No,D,x005,T,M,P,L,C)) :- utente(ID,S,No,D,x005,T,M,P,L,C)).


% utente telefone - x006
utente(10, 152635, susana, date(1950,07,03), 'susy@hebd.pt', x006, covilha, pedreiro, [], centro_saude_covilha).
excecao(utente(ID,S,No,D,E,T,M,P,L,C)) :- utente(ID,S,No,D,E,x006,M,P,L,C).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(ID,S,No,D,E,x006,M,P,L,C), 
                                            R = (utente(ID,S,No,D,E,x006,M,P,L,C)).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(ID,S,No,D,E,x006,M,P,L,C) ,
                                            R = (excecao(utente(ID,S,No,D,E,x006,M,P,L,C)) :- utente(ID,S,No,D,E,x006,M,P,L,C)).


% utente morada - x007
utente(11, 151432, bruno, date(1977,11,11), 'bruno@outlook.pt', 919723768, x007, arquiteto, [], centro_saude_c).
excecao(utente(ID,S,No,D,E,T,M,P,L,C)) :- utente(ID,S,No,D,E,T,x007,P,L,C).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(ID,S,No,D,E,T,x007,P,L,C), 
                                            R = (utente(ID,S,No,D,E,T,x007,P,L,C)).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(ID,S,No,D,E,T,x007,P,L,C) ,
                                            R = (excecao(utente(ID,S,No,D,E,T,x007,P,L,C)) :- utente(ID,S,No,D,E,T,x007,P,L,C)).


% utente profissao - x008
utente(12, 151154, manuel, date(1990,09,21), 'nel@outlook.pt', 913242472, portalegre, x008, [], centro_saude_portalegre).
excecao(utente(ID,S,No,D,E,T,M,P,L,C)) :- utente(ID,S,No,D,E,T,M,x008,L,C).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(ID,S,No,D,E,T,M,x008,L,C), 
                                            R = (utente(ID,S,No,D,E,T,M,x008,L,C)).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(ID,S,No,D,E,T,M,x008,L,C) ,
                                            R = (excecao(utente(ID,S,No,D,E,T,M,x008,L,C)) :- utente(ID,S,No,D,E,T,M,x008,L,C)).


% utente doencas_cron - x009
utente(13, 153654, rosa, date(1985,06,25), 'rosinha@outlook.pt', 936523738, faro, hoteleira, x009, centro_saude_faro).
excecao(utente(ID,S,No,D,E,T,M,P,L,C)) :- utente(ID,S,No,D,E,T,M,P,x009,C).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(ID,S,No,D,E,T,M,P,x009,C), 
                                            R = (utente(ID,S,No,D,E,T,M,P,x009,C)).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(ID,S,No,D,E,T,M,P,x009,C) ,
                                            R = (excecao(utente(ID,S,No,D,E,T,M,P,x009,C)) :- utente(ID,S,No,D,E,T,M,P,x009,C)).

% utente centro_saude - x010
utente(13, 153654, americo, date(1960,04,02), 'americo@outlook.pt', 936524768, arcosdevaldevez, agricultor, [diabetes], x010).
excecao(utente(ID,S,No,D,E,T,M,P,L,C)) :- utente(ID,S,No,D,E,T,M,P,L,x010).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(ID,S,No,D,E,T,M,P,L,x010), 
                                            R = (utente(ID,S,No,D,E,T,M,P,L,x010)).

cImperfeito(utente(ID,S,No,D,E,T,M,P,L,C),R) :- utente(ID,S,No,D,E,T,M,P,L,x010) ,
                                            R = (excecao(utente(ID,S,No,D,E,T,M,P,L,x010)) :- utente(ID,S,No,D,E,T,M,P,L,x010)).



%---------------------------------------------------------------------------------------------
% Extensão do conhecimento incerto centro_saude 

% centro_saude id - x011 
centro_saude(x011, centro_saude_guarda, guarda, 253424267, 'guarda@csg.pt').
excecao(centro_saude(ID,No,M,T,E)) :- centro_saude(x011,No,M,T,E).

cImperfeito(centro_saude(ID,No,M,T,E)) :- centro_saude(x011,No,M,T,E),
                                        R = (centro_saude(x011,No,M,T,E)).

cImperfeito(centro_saude(ID,No,M,T,E)) :- centro_saude(x011,No,M,T,E),
                                        R = (excecao(centro_saude(x011,No,M,T,E)) :- centro_saude(x011,No,M,T,E)).


% centro_saude nome - x012
centro_saude(8, x012, beja, 253424267, 'beja@csbj.pt').
excecao(centro_saude(ID,No,M,T,E)) :- centro_saude(ID,x012,M,T,E).

cImperfeito(centro_saude(ID,No,M,T,E)) :- centro_saude(ID,x012,M,T,E),
                                        R = (centro_saude(ID,x012,M,T,E)).

cImperfeito(centro_saude(ID,No,M,T,E)) :- centro_saude(ID,x012,M,T,E),
                                        R = (excecao(centro_saude(ID,x012,M,T,E)) :- centro_saude(ID,x012,M,T,E)).


% centro_saude morada - x013
centro_saude(9, centro_saude_beja, x013, 253424267, 'beja@csbj.pt').
excecao(centro_saude(ID,No,M,T,E)) :- centro_saude(ID,No,x013,T,E).

cImperfeito(centro_saude(ID,No,M,T,E)) :- centro_saude(ID,No,x013,T,E),
                                        R = (centro_saude(ID,No,x013,T,E)).

cImperfeito(centro_saude(ID,No,M,T,E)) :- centro_saude(ID,No,x013,T,E),
                                        R = (excecao(centro_saude(ID,No,x013,T,E)) :- centro_saude(ID,No,x013,T,E)).

% centro_saude telefone - x014 
centro_saude(10, centro_saude_arcos, arcosdevaldevez , x014, 'arcos@csa.pt').
excecao(centro_saude(ID,No,M,T,E)) :- centro_saude(ID,No,M,x014,E).

cImperfeito(centro_saude(ID,No,M,T,E)) :- centro_saude(ID,No,M,x014,E),
                                        R = (centro_saude(ID,No,M,x014,E)).

cImperfeito(centro_saude(ID,No,M,T,E)) :- centro_saude(ID,No,M,x014,E),
                                        R = (excecao(centro_saude(ID,No,M,x014,E)) :- centro_saude(ID,No,M,x014,E)).

% centro_saude email - x015 
centro_saude(11, centro_saude_viana, viana , 253427723, x015).
excecao(centro_saude(ID,No,M,T,E)) :- centro_saude(ID,No,M,T,x015).

cImperfeito(centro_saude(ID,No,M,T,E)) :- centro_saude(ID,No,M,T,x015),
                                        R = (centro_saude(ID,No,M,T,x015)).

cImperfeito(centro_saude(ID,No,M,T,E)) :- centro_saude(ID,No,M,T,x015),
                                        R = (excecao(centro_saude(ID,No,M,T,x015)) :- centro_saude(ID,No,M,T,x015)).


%---------------------------------------------------------------------------------------------
% Extensão do conhecimento incerto staff 

% staff id - x016 
staff(x016,4, sandra, 'sandra_vilareal@csvr.pt').
excecao(staff(ID,I,No,E)) :- staff(x016,I,No,E).

cImperfeito(staff(ID,I,No,E)) :- staff(x016,I,No,E),
                                        R = (staff(x016,I,No,E)).

cImperfeito(staff(ID,I,No,E)) :- staff(x016,I,No,E),
                                        R = (excecao(staff(x016,I,No,E)) :- staff(x016,I,No,E)).

% staff idCentro - x017
staff(7,x017, bernardo, 'bernardo_guarda@csg.pt').
excecao(staff(ID,I,No,E)) :- staff(ID,x017,No,E).

cImperfeito(staff(ID,I,No,E)) :- staff(ID,x017,No,E),
                                        R = (staff(ID,x017,No,E)).

cImperfeito(staff(ID,I,No,E)) :- staff(ID,x017,No,E),
                                        R = (excecao(staff(ID,x017,No,E)) :- staff(ID,x017,No,E)).


% staff nome - x018 
staff(8,2, x018, 'nome_caranda@csb.pt').
excecao(staff(ID,I,No,E)) :- staff(ID,I,x018,E).

cImperfeito(staff(ID,I,No,E)) :- staff(ID,I,x018,E),
                                        R = (staff(ID,I,x018,E)).

cImperfeito(staff(ID,I,No,E)) :- staff(ID,I,x018,E),
                                        R = (excecao(staff(ID,I,x018,E)) :- staff(ID,I,x018,E)).

% staff email - x019
staff(9,4, elias, x019).
excecao(staff(ID,I,No,E)) :- staff(ID,I,No,x019).

cImperfeito(staff(ID,I,No,E)) :- staff(ID,I,No,x019),
                                        R = (staff(ID,I,No,x019)).

cImperfeito(staff(ID,I,No,E)) :- staff(ID,I,No,x019),
                                        R = (excecao(staff(ID,I,No,x019)) :- staff(ID,I,No,x019)).


%---------------------------------------------------------------------------------------------
% Extensao do conhecimento incerto de vacinacao_covid

% vacinacao_covid idStaff - x020 
vacinacao_covid(x020, 3,  '20-03-2020', 'Pfizer', 1).
excecao(vacinacao_covid(Ids,Idu,D,V,T)) :- vacinacao_covid(x020,Idu,D,V,T).

cImperfeito(vacinacao_covid(Ids,Idu,D,V,T)) :- vacinacao_covid(x020,Idu,D,V,T),
                                        R = (vacinacao_covid(x020,Idu,D,V,T)).

cImperfeito(vacinacao_covid(Ids,Idu,D,V,T)) :- vacinacao_covid(x020,Idu,D,V,T),
                                        R = (excecao(vacinacao_covid(x020,Idu,D,V,T)) :- vacinacao_covid(x020,Idu,D,V,T)).



% vacinacao_covid idUtente - x021 
vacinacao_covid(5, x021,  '19-12-2020', 'Moderna', 2).
excecao(vacinacao_covid(Ids,Idu,D,V,T)) :- vacinacao_covid(Ids,x021,D,V,T).

cImperfeito(vacinacao_covid(Ids,Idu,D,V,T)) :- vacinacao_covid(Ids,x021,D,V,T),
                                        R = (vacinacao_covid(Ids,x021,D,V,T)).

cImperfeito(vacinacao_covid(Ids,Idu,D,V,T)) :- vacinacao_covid(Ids,x021,D,V,T),
                                        R = (excecao(vacinacao_covid(Ids,x021,D,V,T)) :- vacinacao_covid(Ids,x021,D,V,T)).


% vacinacao_covid data - x022 
vacinacao_covid(5, 3,  x022, 'Astrazeneca', 2).
excecao(vacinacao_covid(Ids,Idu,D,V,T)) :- vacinacao_covid(Ids,Idu,x022,V,T).

cImperfeito(vacinacao_covid(Ids,Idu,D,V,T)) :- vacinacao_covid(Ids,Idu,x022,V,T),
                                        R = (vacinacao_covid(Ids,Idu,x022,V,T)).

cImperfeito(vacinacao_covid(Ids,Idu,D,V,T)) :- vacinacao_covid(Ids,Idu,x022,V,T),
                                        R = (excecao(vacinacao_covid(Ids,Idu,x022,V,T)) :- vacinacao_covid(Ids,Idu,x022,V,T)).


% vacinacao_covid vacina - x023 
vacinacao_covid(1, 5, '10-01-2021', x023, 2).
excecao(vacinacao_covid(Ids,Idu,D,V,T)) :- vacinacao_covid(Ids,Idu,D,x023,T).

cImperfeito(vacinacao_covid(Ids,Idu,D,V,T)) :- vacinacao_covid(Ids,Idu,D,x023,T),
                                        R = (vacinacao_covid(Ids,Idu,D,x023,T)).

cImperfeito(vacinacao_covid(Ids,Idu,D,V,T)) :- vacinacao_covid(Ids,Idu,D,x023,T),
                                        R = (excecao(vacinacao_covid(Ids,Idu,D,x023,T)) :- vacinacao_covid(Ids,Idu,D,x023,T)).



% vacinacao_covid toma - x024 
vacinacao_covid(1, 5, '10-01-2021', 'PFizer', x024).
excecao(vacinacao_covid(Ids,Idu,D,V,T)) :- vacinacao_covid(Ids,Idu,D,V,x024).

cImperfeito(vacinacao_covid(Ids,Idu,D,V,T)) :- vacinacao_covid(Ids,Idu,D,V,x024),
                                        R = (vacinacao_covid(Ids,Idu,D,V,x024)).

cImperfeito(vacinacao_covid(Ids,Idu,D,V,T)) :- vacinacao_covid(Ids,Idu,D,V,x024),
                                        R = (excecao(vacinacao_covid(Ids,Idu,D,V,x024)) :- vacinacao_covid(Ids,Idu,D,V,x024)).

%---------------------------------------------------------------------------------------------
% Extensão do conhecimento impreciso utente 

% utente id 
excecao(utente(8, 365337, mafalda, date(1998,05,29), 'mafalda@outlook.pt', 917236372, cascais, marketing,[], centro_saude_cascais)).
excecao(utente(9, 365337, mafalda, date(1998,05,29), 'mafalda@outlook.pt', 917236372, cascais, marketing,[], centro_saude_cascais)).

cImperfeito(utente(8, 365337, mafalda, date(1998,05,29), 'mafalda@outlook.pt', 917236372, cascais, marketing,[], centro_saude_cascais),R) :-
                        R = excecao(utente(8, 365337, mafalda, date(1998,05,29), 'mafalda@outlook.pt', 917236372, cascais, marketing,[], centro_saude_cascais)).

cImperfeito(utente(8, 365337, mafalda, date(1998,05,29), 'mafalda@outlook.pt', 917236372, cascais, marketing,[], centro_saude_cascais),R) :-
                        R = excecao(utente(8, 365337, mafalda, date(1998,05,29), 'mafalda@outlook.pt', 917236372, cascais, marketing,[], centro_saude_cascais)).

% utente nº segurança social 
excecao(utente(8, 365337, mafalda, date(1998,05,29), 'mafalda@outlook.pt', 917236372, cascais, marketing,[], centro_saude_cascais)).
excecao(utente(8, 365339, mafalda, date(1998,05,29), 'mafalda@outlook.pt', 917236372, cascais, marketing,[], centro_saude_cascais)).

cImperfeito(utente(8, 365337, mafalda, date(1998,05,29), 'mafalda@outlook.pt', 917236372, cascais, marketing,[], centro_saude_cascais),R) :-
                        R = excecao(utente(8, 365337, mafalda, date(1998,05,29), 'mafalda@outlook.pt', 917236372, cascais, marketing,[], centro_saude_cascais)).

cImperfeito(utente(8, 365337, mafalda, date(1998,05,29), 'mafalda@outlook.pt', 917236372, cascais, marketing,[], centro_saude_cascais),R) :-
                        R = excecao(utente(8, 365337, mafalda, date(1998,05,29), 'mafalda@outlook.pt', 917236372, cascais, marketing,[], centro_saude_cascais)).

% utente data de Nascimento
excecao(utente(8, 326532, humberto, date(1977,10,10), 'humberto@outlook.pt', 913643483, portimao, ceo,[], centro_saude_portimao)).
excecao(utente(8, 326532, humberto, date(1977,10,11), 'humberto@outlook.pt', 913643483, portimao, ceo,[], centro_saude_portimao)).

cImperfeito(utente(8, 326532, humberto, date(1977,10,10), 'humberto@outlook.pt', 913643483, portimao, ceo,[], centro_saude_portimao),R) :-
                        R = excecao(utente(8, 326532, humberto, date(1977,10,10), 'humberto@outlook.pt', 913643483, portimao, ceo,[], centro_saude_portimao)).

cImperfeito(utente(8, 326532, humberto, date(1977,10,11), 'humberto@outlook.pt', 913643483, portimao, ceo,[], centro_saude_portimao),R) :-
                        R = excecao(utente(8, 326532, humberto, date(1977,10,11), 'humberto@outlook.pt', 913643483, portimao, ceo,[], centro_saude_portimao)).



%---------------------------------------------------------------------------------------------
% Extensão do conhecimento impreciso centro de saude 

% centro de saude id 
excecao(centro_saude(I, centro_saude_guarda, guarda, 253764648, 'guarda@csg.pt')):- I >= 9, I=<15.
cImperfeito(centro_saude(I, centro_saude_guarda, guarda, 253764648, 'guarda@csg.pt'), R) :-
    R = (excecao(centro_saude(I, centro_saude_guarda, guarda, 253764648, 'guarda@csg.pt')) :-
    I >= 9,
    I =< 15).

% centro de saude nome 
excecao(centro_saude(8, centro_saude_guarda, guarda, 253764648, 'guarda@csg.pt')).
excecao(centro_saude(8, centro_saude_manteigas, guarda, 253764648, 'guarda@csg.pt')).

cImperfeito(centro_saude(8, centro_saude_guarda, guarda, 253764648, 'guarda@csg.pt'),R) :-
                        R = excecao(centro_saude(8, centro_saude_guarda, guarda, 253764648, 'guarda@csg.pt')).

cImperfeito(centro_saude(8, centro_saude_guarda, guarda, 253764648, 'guarda@csg.pt'),R) :-
                        R = excecao(centro_saude(8, centro_saude_guarda, guarda, 253764648, 'guarda@csg.pt')).


% centro de saude morada 
excecao(centro_saude(9, centro_saude_guarda, serra_da_estrela, 253764648, 'guarda@csg.pt')).
excecao(centro_saude(9, centro_saude_guarda, guarda, 253764648, 'guarda@csg.pt')).

cImperfeito(centro_saude(9, centro_saude_guarda, serra_da_estrela, 253764648, 'guarda@csg.pt'),R) :-
                        R = excecao(centro_saude(9, centro_saude_guarda, serra_da_estrela, 253764648, 'guarda@csg.pt')).

cImperfeito(centro_saude(9, centro_saude_guarda, guarda, 253764648, 'guarda@csg.pt'),R) :-
                        R = excecao(centro_saude(9, centro_saude_guarda, guarda, 253764648, 'guarda@csg.pt')).

% centro de saude telefone 
excecao(centro_saude(9, centro_saude_guarda, guarda, 253764649, 'guarda@csg.pt')).
excecao(centro_saude(9, centro_saude_guarda, guarda, 253764647, 'guarda@csg.pt')).

cImperfeito(centro_saude(9, centro_saude_guarda, guarda, 253764649, 'guarda@csg.pt'),R) :-
                        R = excecao(centro_saude(9, centro_saude_guarda, guarda, 253764649, 'guarda@csg.pt')).

cImperfeito(centro_saude(9, centro_saude_guarda, guarda, 253764647, 'guarda@csg.pt'),R) :-
                        R = excecao(centro_saude(9, centro_saude_guarda, guarda, 253764647, 'guarda@csg.pt')).

% centro de saude email

excecao(centro_saude(9, centro_saude_guarda, guarda, 253764648, 'serraestrela@csg.pt')).
excecao(centro_saude(9, centro_saude_guarda, guarda, 253764648, 'guarda@csg.pt')).

cImperfeito(centro_saude(9, centro_saude_guarda, guarda, 253764648, 'serraestrela@csg.pt'),R) :-
                        R = excecao(centro_saude(9, centro_saude_guarda, gaurda, 253764648, 'guarda@csg.pt')).

cImperfeito(centro_saude(9, centro_saude_guarda, guarda, 253764648, 'guarda@csg.pt'),R) :-
                        R = excecao(centro_saude(9, centro_saude_guarda, guarda, 253764648, 'guarda@csg.pt')).


%---------------------------------------------------------------------------------------------
% Extensão do conhecimento impreciso staff 

% staff id
excecao(staff(6,7, carmo, 'carmo_vilaverde@csvv.pt')).
excecao(staff(5,7, carmo, 'carmo_vilaverde@csvv.pt')).

cImperfeito(staff(6,7, carmo, 'carmo_vilaverde@csvv.pt'),R) :-
                        R = excecao(staff(6,7, carmo, 'carmo_vilaverde@csvv.pt')).

cImperfeito(staff(5,7, carmo, 'carmo_vilaverde@csvv.pt'),R) :-
                        R = excecao(staff(5,7, carmo, 'carmo_vilaverde@csvv.pt')).

% staff idCentro 
excecao(staff(6,I, carmo, 'carmo_vilaverde@csvv.pt')):- I >= 1, I=<8.
cImperfeito(staff(6,I, carmo, 'carmo_vilaverde@csvv.pt'), R) :-
    R = (excecao(staff(6,I, carmo, 'carmo_vilaverde@csvv.pt')) :-
    I >= 1, I=<8).

% staff nome 
excecao(staff(8,5, norberto, 'norberto@esposende.pt')).
excecao(staff(8,5, nuno, 'norberto@esposende.pt')).

cImperfeito(staff(8,5, norberto, 'norberto@esposende.pt'),R) :-
                        R = excecao(staff(8,5, norberto, 'norberto@esposende.pt')).

cImperfeito(staff(8,5, nuno, 'norberto@esposende.pt'),R) :-
                        R = excecao(staff(8,5, nuno, 'norberto@esposende.pt')).


% staff email 
excecao(staff(9,3, nuna, 'nuna@fafe.pt')).
excecao(staff(9,3, nuna, 'nuna@santotirso.pt')).

cImperfeito(staff(9,3, nuna, 'nuna@fafe.pt'),R) :-
                        R = excecao(staff(9,3, nuna, 'nuna@fafe.pt')).

cImperfeito(staff(9,3, nuna, 'nuna@santotirso.pt'),R) :-
                        R = excecao(staff(9,3, nuna, 'nuna@santotirso.pt')).



%---------------------------------------------------------------------------------------------
% Extensão do conhecimento impreciso vacinacao_covid

% vacinacao_covid - idStaff 
excecao(vacinacao_covid(2, 6, '06-02-2021', 'Pfizer',2)).
excecao(vacinacao_covid(4, 6, '06-02-2021', 'Pfizer',2)).

cImperfeito(vacinacao_covid(2, 6, '06-02-2021', 'Pfizer',2),R) :-
                        R = excecao(vacinacao_covid(2, 6, '06-02-2021', 'Pfizer',2)).

cImperfeito(vacinacao_covid(4, 6, '06-02-2021', 'Pfizer',2),R) :-
                        R = excecao(vacinacao_covid(4, 6, '06-02-2021', 'Pfizer',2)).


% vacinacao_covid - idUtente 
excecao(vacinacao_covid(2, U, '06-02-2021', 'Pfizer',1)):- U >= 1, U=<20.
cImperfeito(vacinacao_covid(2, U, '06-02-2021', 'Pfizer',1), R) :-
    R = (excecao(vacinacao_covid(2, U, '06-02-2021', 'Pfizer',1)) :-
    U >= 1, U=<20).

% vacinacao_covid - data 
excecao(vacinacao_covid(2, 3, D, 'Pfizer',1)):- D >= '06-02-2021', D=< '15-02-2021'.
cImperfeito(vacinacao_covid(2, 3,D, 'Pfizer',1), R) :-
    R = (excecao(vacinacao_covid(2, 3,D, 'Pfizer',1)) :-
    D >= '06-02-2021', D=< '15-02-2021').


% vacinacao_covid - vacina 
excecao(vacinacao_covid(7, 2, '02-05-2021', 'Moderna',1)).
excecao(vacinacao_covid(7, 2, '02-05-2021', 'Pfizer',1)).

cImperfeito(vacinacao_covid(7, 2, '02-05-2021', 'Moderna',1),R) :-
                        R = excecao(vacinacao_covid(7, 2, '02-05-2021', 'Moderna',1)).

cImperfeito(vacinacao_covid(7, 2, '02-05-2021', 'Pfizer',1),R) :-
                        R = excecao(vacinacao_covid(7, 2, '02-05-2021', 'Pfizer',1)).



% vacinacao_covid - toma
excecao(vacinacao_covid(4, 5, '06-06-2021', 'Astrazeneca',T)):- T >= 0, T=< 2.
cImperfeito(vacinacao_covid(4, 5, '06-06-2021', 'Astrazeneca',T), R) :-
    R = (excecao(vacinacao_covid(4, 5, '06-06-2021', 'Astrazeneca',T)) :-
    T >= 0, T=< 2).


%---------------------------------------------------------------------------------------------
% Extensão do conhecimento interdito utente 

% utente id - np1
utente(8, 9261376, luis,  date(1997,08,18), 'luis@outlook.pt', 91272635, geres, np1, [], centro_saude_geres).
excecao(utente(ID,S,No,D,E,T,M,P,L,C)) :- utente(ID,S,No,D,E,T,M,np1,L,C).
nulo(np1).
+utente(ID,S,No,D,E,T,M,P,L,C) :: (solucoes(Profissao,(utente(8, 9261376, luis,  date(1997,08,18), 'luis@outlook.pt', 91272635, geres, Profissao, [], centro_saude_geres), nao(nulo(Profissao))),L),
                comprimento(L,N), N==0).



listaUtentes(R) :- solucoes(utente(ID,SS,NM,D,E,T,M,P,L,CS), utente(ID,SS,NM,D,E,T,M,P,L,CS), R).

%---------------------------------------------------------------------------------------------
% Extensão do conhecimento interdito centro_saude

% centro_saude id - np11 
centro_saude(8, centro_saude_paredes, np11, 25262537, 'paredes@csvpc.pt').
excecao(centro_saude(ID,No,M,T,E)) :- centro_saude(ID,No, np11,T,E).
nulo(np11).
+centro_saude(ID,No,M,T,E) :: (solucoes(Morada,(centro_saude(8, centro_saude_paredes, Morada, 25262537, 'paredes@csvpc.pt'), nao(nulo(Morada))),L),
                comprimento(L,N), N==0).

listaCentros(R) :- solucoesSRep((Id, N, M, T, E), centro_saude(Id, N, M, T, E), R).

%---------------------------------------------------------------------------------------------
% Extensão do conhecimento interdito staff 

% staff id - np16 
staff(np16,5, idalete, 'idalete_esposende@csve.pt').
excecao(staff(ID,I,No,E)) :- staff(np16,I,No,E).
nulo(np16).
+staff(ID,I,No,E) :: (solucoes(IDs,(staff(IDs,5, idalete, 'idalete_esposende@csve.pt'), nao(nulo(IDs))),L),
                comprimento(L,N), N==0).




listaStaff(R) :- solucoes((IdS, IdC, N, E), staff(IdS, IdC, N, E), R).


%---------------------------------------------------------------------------------------------
% Extensão do conhecimento interdito vacinacao_covid

% vacinacao_covid vacina - np23 
vacinacao_covid(3, 7, '18-02-2021', np23,1).
excecao(vacinacao_covid(Ids,Idu,D,V,T)) :- vacinacao_covid(Ids,Idu,D,np23,T).
nulo(np23).
+vacinacao_covid(Ids,Idu,D,V,T) :: (solucoes(Vacina,(vacinacao_covid(3, 7, '18-02-2021', Vacina,1), nao(nulo(Vacina))),L),
                comprimento(L,N), N==0).


listaVac(R) :- solucoes((IdS, IdU, D, Vac, T), vacinacao_covid(IdS, IdU, D, Vac, T), R).

%---------------------------------------------------------------------------------------------
% Invariante não permite que haja conhecimento negativo repetido

+(-Q) :: (solucoes(Q, clause(-Q, true), S),
        comprimento(S,N), 
        N =< 1).



% Invariante nao permite que haja o mesmo conhecimento negativo e positivo ao mesmo tempo 
+Q :: nao(-Q).
+(-Q) :: nao(Q).

% Invariante nao permite que haja o mesmo conhecimento negativo e desconhecido
+(-Q) :: (solucoes(Q,clause(excecao(Q), true),S),
                  comprimento(S,N), 
                  N == 0).

% Não permite que existam IDs iguais nos utentes 

+utente(ID,S,No,D,E,T,M,P,L,C) :: (solucoes((ID),(utente(ID,A,B,C,F,G,H,I,J,K)),S),
                                    comprimento(S,N), N =< 1).

% Não permite que existam IDs iguais nos centros de centro_saude

+centro_saude(ID,No,M,T,E) :: (solucoes((ID), (centro_saude(ID,X,Y,Z,W)),S),
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



%---------------------------------------------------------------------------------------------
% INCERTO

%UTENTE
% nº seguran social UTENTE 
evolucaoIncSSutente(utente(ID,S,No,D,E,T,M,P,Do,C)) :-
                    demo(utente(ID,S,No,D,E,T,M,P,Do,C),desconhecido),
                    solucoes(excecao(utente(ID,S,No,D,E,T,M,P,Do,C)) :- utente(ID,X,No,D,E,T,M,P,Do,C)), 
                                                                (utente(ID,X,No,D,E,T,M,P,Do,C),nao(nulo(X)),L),
                    removeL(L),
                    remove(utente(ID,X,No,D,E,T,M,P,Do,C)),
                    evolucao(utente(ID,S,No,D,E,T,M,P,Do,C)).


% nome UTENTE
evolucaoIncNutente(utente(ID,S,No,D,E,T,M,P,Do,C)) :-
                    demo(utente(ID,S,No,D,E,T,M,P,Do,C),desconhecido),
                    solucoes(excecao(utente(ID,S,No,D,E,T,M,P,Do,C)) :- utente(ID,S,X,D,E,T,M,P,Do,C)), 
                                                                (utente(ID,S,X,D,E,T,M,P,Do,C),nao(nulo(X)),L),
                    removeL(L),
                    remove(utente(ID,S,X,D,E,T,M,P,Do,C)),
                    evolucao(utente(ID,S,No,D,E,T,M,P,Do,C)).


% data nascimento UTENTE
evolucaoIncDutente(utente(ID,S,No,D,E,T,M,P,Do,C)) :-
                    demo(utente(ID,S,No,D,E,T,M,P,Do,C),desconhecido),
                    solucoes(excecao(utente(ID,S,No,D,E,T,M,P,Do,C)) :- utente(ID,S,No,X,E,T,M,P,Do,C)), 
                                                                (utente(ID,S,No,X,E,T,M,P,Do,C),nao(nulo(X)),L),
                    removeL(L),
                    remove(utente(ID,S,No,X,E,T,M,P,Do,C)),
                    evolucao(utente(ID,S,No,D,E,T,M,P,Do,C)).

% email UTENTE 
evolucaoIncEutente(utente(ID,S,No,D,E,T,M,P,Do,C)) :-
                    demo(utente(ID,S,No,D,E,T,M,P,Do,C),desconhecido),
                    solucoes(excecao(utente(ID,S,No,D,E,T,M,P,Do,C)) :- utente(ID,S,No,D,X,T,M,P,Do,C)), 
                                                                (utente(ID,S,No,D,X,T,M,P,Do,C),nao(nulo(X)),L),
                    removeL(L),
                    remove(utente(ID,S,No,D,X,T,M,P,Do,C)),
                    evolucao(utente(ID,S,No,D,E,T,M,P,Do,C)).

% telefone UTENTE
evolucaoIncTutente(utente(ID,S,No,D,E,T,M,P,Do,C)) :-
                    demo(utente(ID,S,No,D,E,T,M,P,Do,C),desconhecido),
                    solucoes(excecao(utente(ID,S,No,D,E,T,M,P,Do,C)) :- utente(ID,S,No,D,E,X,M,P,Do,C)), 
                                                                (utente(ID,S,No,D,E,X,M,P,Do,C),nao(nulo(X)),L),
                    removeL(L),
                    remove(utente(ID,S,No,D,E,X,M,P,Do,C)),
                    evolucao(utente(ID,S,No,D,E,T,M,P,Do,C)).

% morada UTENTE 
evolucaoIncMutente(utente(ID,S,No,D,E,T,M,P,Do,C)) :-
                    demo(utente(ID,S,No,D,E,T,M,P,Do,C),desconhecido),
                    solucoes(excecao(utente(ID,S,No,D,E,T,M,P,Do,C)) :- utente(ID,S,No,D,E,T,X,P,Do,C)), 
                                                                (utente(ID,S,No,D,E,T,X,P,Do,C),nao(nulo(X)),L),
                    removeL(L),
                    remove(utente(ID,S,No,D,E,T,X,P,Do,C)),
                    evolucao(utente(ID,S,No,D,E,T,M,P,Do,C)).

% profissão UTENTE 
evolucaoIncPutente(utente(ID,S,No,D,E,T,M,P,Do,C)) :-
                    demo(utente(ID,S,No,D,E,T,M,P,Do,C),desconhecido),
                    solucoes(excecao(utente(ID,S,No,D,E,T,M,P,Do,C)) :- utente(ID,S,No,D,E,T,M,X,Do,C)), 
                                                                (utente(ID,S,No,D,E,T,M,X,Do,C),nao(nulo(X)),L),
                    removeL(L),
                    remove(utente(ID,S,No,D,E,T,M,X,Do,C)),
                    evolucao(utente(ID,S,No,D,E,T,M,P,Do,C)).


% doencas_cron UTENTE 
evolucaoIncDoutente(utente(ID,S,No,D,E,T,M,P,Do,C)) :-
                    demo(utente(ID,S,No,D,E,T,M,P,Do,C),desconhecido),
                    solucoes(excecao(utente(ID,S,No,D,E,T,M,P,Do,C)) :- utente(ID,S,No,D,E,T,M,P,X,C)), 
                                                                (utente(ID,S,No,D,E,T,D,P,X,C),nao(nulo(X)),L),
                    removeL(L),
                    remove(utente(ID,S,No,D,E,T,M,P,X,C)),
                    evolucao(utente(ID,S,No,D,E,T,M,P,Do,C)).


% centro de saude UTENTE 
evolucaoIncCutente(utente(ID,S,No,D,E,T,M,P,Do,C)) :-
                    demo(utente(ID,S,No,D,E,T,M,P,Do,C),desconhecido),
                    solucoes(excecao(utente(ID,S,No,D,E,T,M,P,Do,C)) :- utente(ID,S,No,D,E,T,M,P,Do,X)), 
                                                                (utente(ID,S,No,D,E,T,D,P,Do,X),nao(nulo(X)),L),
                    removeL(L),
                    remove(utente(ID,S,No,D,E,T,M,P,Do,X)),
                    evolucao(utente(ID,S,No,D,E,T,M,P,Do,C)).

%CENTRO_SAUDE 
% nome CENTRO_SAUDE 
evolucaoIncNCS(centro_saude(ID,No,M,T,E)) :-
                    demo(centro_saude(ID,No,M,T,E),desconhecido),
                    solucoes(excecao(centro_saude(ID,No,M,T,E)) :- centro_saude(ID,X,M,T,E)), 
                                                                (centro_saude(ID,X,M,T,E),nao(nulo(X)),L),
                    removeL(L),
                    remove(centro_saude(ID,X,M,T,E)),
                    evolucao(centro_saude(ID,No,M,T,E)).

% morada CENTRO_SAUDE
evolucaoIncMCS(centro_saude(ID,No,M,T,E)) :-
                    demo(centro_saude(ID,No,M,T,E),desconhecido),
                    solucoes(excecao(centro_saude(ID,No,M,T,E)) :- centro_saude(ID,No,X,T,E)), 
                                                                (centro_saude(ID,No,X,T,E),nao(nulo(X)),L),
                    removeL(L),
                    remove(centro_saude(ID,No,X,T,E)),
                    evolucao(centro_saude(ID,No,M,T,E)).

% telefone CENTRO_SAUDE
evolucaoIncTCS(centro_saude(ID,No,M,T,E)) :-
                    demo(centro_saude(ID,No,M,T,E),desconhecido),
                    solucoes(excecao(centro_saude(ID,No,M,T,E)) :- centro_saude(ID,No,M,X,E)), 
                                                                (centro_saude(ID,No,M,X,E),nao(nulo(X)),L),
                    removeL(L),
                    remove(centro_saude(ID,No,M,X,E)),
                    evolucao(centro_saude(ID,No,M,T,E)).


% email CENTRO_SAUDE 
evolucaoIncECS(centro_saude(ID,No,M,T,E)) :-
                    demo(centro_saude(ID,No,M,T,E),desconhecido),
                    solucoes(excecao(centro_saude(ID,No,M,T,E)) :- centro_saude(ID,No,M,T,X)), 
                                                                (centro_saude(ID,No,M,T,X),nao(nulo(X)),L),
                    removeL(L),
                    remove(centro_saude(ID,No,M,T,X)),
                    evolucao(centro_saude(ID,No,M,T,E)).

%STAFF
% id centro STAFF 
evolucaoIncICS(staff(ID,IDu,N,E)) :-
                    demo(staff(ID,IDu,N,E),desconhecido),
                    solucoes(excecao(staff(ID,IDu,N,E)) :- staff(ID,X,N,E)), 
                                                                (staff(ID,X,N,E),nao(nulo(X)),L),
                    removeL(L),
                    remove(staff(ID,X,N,E)),
                    evolucao(staff(ID,IDu,N,E)).

% nome STAFF 
evolucaoIncNS(staff(ID,IDu,N,E)) :-
                    demo(staff(ID,IDu,N,E),desconhecido),
                    solucoes(excecao(staff(ID,IDu,N,E)) :- staff(ID,IDu,X,E)), 
                                                                (staff(ID,IDu,X,E),nao(nulo(X)),L),
                    removeL(L),
                    remove(staff(ID,IDu,X,E)),
                    evolucao(staff(ID,IDu,N,E)).

% email STAFF 
evolucaoIncES(staff(ID,IDu,N,E)) :-
                    demo(staff(ID,IDu,N,E),desconhecido),
                    solucoes(excecao(staff(ID,IDu,N,E)) :- staff(ID,IDu,N,X)), 
                                                                (staff(ID,IDu,N,X),nao(nulo(X)),L),
                    removeL(L),
                    remove(staff(ID,IDu,N,X)),
                    evolucao(staff(ID,IDu,N,E)).

%VACINACAO_COVID
% id staff VACINACAO_COVID
evolucaoIncIsV(vacinacao_covid(IDs,IDu,D,V,T)) :-
                    demo(vacinacao_covid(IDs,IDu,D,V,T),desconhecido),
                    solucoes(excecao(vacinacao_covid(IDs,IDu,D,V,T)) :- vacinacao_covid(X,IDu,D,V,T)), 
                                                                (vacinacao_covid(X,IDu,D,V,T),nao(nulo(X)),L),
                    removeL(L),
                    remove(vacinacao_covid(X,IDu,D,V,T)),
                    evolucao(vacinacao_covid(IDs,IDu,D,V,T)).

% id utente VACINACAO_COVID
evolucaoIncIuV(vacinacao_covid(IDs,IDu,D,V,T)) :-
                    demo(vacinacao_covid(IDs,IDu,D,V,T),desconhecido),
                    solucoes(excecao(vacinacao_covid(IDs,IDu,D,V,T)) :- vacinacao_covid(IDs,X,D,V,T)), 
                                                                (vacinacao_covid(IDs,X,D,V,T),nao(nulo(X)),L),
                    removeL(L),
                    remove(vacinacao_covid(IDs,X,D,V,T)),
                    evolucao(vacinacao_covid(IDs,IDu,D,V,T)).

% data VACINACAO_COVID
evolucaoIncDV(vacinacao_covid(IDs,IDu,D,V,T)) :-
                    demo(vacinacao_covid(IDs,IDu,D,V,T),desconhecido),
                    solucoes(excecao(vacinacao_covid(IDs,IDu,D,V,T)) :- vacinacao_covid(IDs,IDu,X,V,T)), 
                                                                (vacinacao_covid(IDs,IDu,X,V,T),nao(nulo(X)),L),
                    removeL(L),
                    remove(vacinacao_covid(IDs,IDu,X,V,T)),
                    evolucao(vacinacao_covid(IDs,IDu,D,V,T)).

% vacina VACINACAO_COVID
evolucaoIncVV(vacinacao_covid(IDs,IDu,D,V,T)) :-
                    demo(vacinacao_covid(IDs,IDu,D,V,T),desconhecido),
                    solucoes(excecao(vacinacao_covid(IDs,IDu,D,V,T)) :- vacinacao_covid(IDs,IDu,D,X,T)), 
                                                                (vacinacao_covid(IDs,IDu,D,X,T),nao(nulo(X)),L),
                    removeL(L),
                    remove(vacinacao_covid(IDs,IDu,D,X,T)),
                    evolucao(vacinacao_covid(IDs,IDu,D,V,T)).

% toma VACINACAO_COVID 
evolucaoIncTV(vacinacao_covid(IDs,IDu,D,V,T)) :-
                    demo(vacinacao_covid(IDs,IDu,D,V,T),desconhecido),
                    solucoes(excecao(vacinacao_covid(IDs,IDu,D,V,T)) :- vacinacao_covid(IDs,IDu,D,V,X)), 
                                                                (vacinacao_covid(IDs,IDu,D,V,X),nao(nulo(X)),L),
                    removeL(L),
                    remove(vacinacao_covid(IDs,IDu,D,V,X)),
                    evolucao(vacinacao_covid(IDs,IDu,D,V,T)).

%---------------------------------------------------------------------------------------------
% IMPRECISO

%evolucao do conhecimento impreciso UTENTE
evolucaoUtImpreciso(utente(ID,Ss,Nome,Data,Email,Telefone,Morada,Profissao,Doenca,Centro)):-
		demo(utente(ID,Ss,Nome,Data,Email,Telefone,Morada,Profissao,Doenca,Centro),desconhecido),
		solucoes(excecao(utente(ID,S,No,D,E,T,M,P,Do,C)), excecao(utente(ID,S,No,D,E,T,M,P,Do,C)),L),
		removeL(L),
		evolucao(utente(ID,Ss,Nome,Data,Email,Telefone,Morada,Profissao,Doenca,Centro)).

%evolucao do conhecimento impreciso CENTRO_SAUDE
evolucaoCSImpreciso(centro_saude(ID,Nome,Morada,Telefone,Email)):-
		demo(centro_saude(ID,Nome,Morada,Telefone,Email),desconhecido),
		solucoes(excecao(centro_saude(ID,No,M,T,E)), excecao(centro_saude(ID,No,M,T,E)),L),
		removeL(L),
		evolucao(centro_saude(ID,Nome,Morada,Telefone,Email)).


%evolucao do conhecimento impreciso STAFF
evolucaoSImpreciso(staff(ID,IDutente,Nome,Email)):-
		demo(staff(ID,IDutente,Nome,Email),desconhecido),
		solucoes(excecao(staff(ID,IDu,N,E)), excecao(staff(ID,IDu,N,E)),L),
		removeL(L),
		evolucao(staff(ID,IDutente,Nome,Email)).

%evolucao do conhecimento impreciso VACINACAO_COVID
evolucaoVImpreciso(vacinacao_covid(IDstaff,IDutente,Data,Vacina,Toma)):-
		demo(vacinacao_covid(IDstaff,IDutente,Data,Vacina,Toma),desconhecido),
		solucoes(excecao(vacinacao_covid(IDs,IDu,D,V,T)), excecao(vacinacao_covid(IDs,IDu,D,V,T)),L),
		removeL(L),
		evolucao(vacinacao_covid(IDstaff,IDutente,Data,Vacina,Toma)).


%Inserir UTENTE impreciso 
% para o nº segurança social 
inserirUtImpreciso(utente(ID,[],No,D,E,T,M,P,Do,C)).
inserirUtImpreciso(utente(ID,[X|Y],No,D,E,T,M,P,Do,C)) :-
	    evolucao( (excecao(utente(ID,X,No,D,E,T,M,P,Do,C)))),
	    inserirUtImpreciso(utente(ID,Y,No,D,E,T,M,P,Do,C)).


% para o nome

inserirUtImpreciso(utente(ID,S,[],D,E,T,M,P,Do,C)).
inserirUtImpreciso(utente(ID,S,[X|Y],D,E,T,M,P,Do,C)) :-
	    evolucao( (excecao(utente(ID,S,X,D,E,T,M,P,Do,C)))),
	    inserirUtImpreciso(utente(ID,S,Y,D,E,T,M,P,Do,C)).


% para a data nascimento
inserirUtImpreciso(utente(ID,S,No,[],E,T,M,P,Do,C)).
inserirUtImpreciso(utente(ID,S,No,[X|Y],E,T,M,P,Do,C)) :-
	    evolucao( (excecao(utente(ID,S,No,X,E,T,M,P,Do,C)))),
	    inserirUtImpreciso(utente(ID,S,No,Y,E,T,M,P,Do,C)).

% para o email 
inserirUtImpreciso(utente(ID,S,No,D,[],T,M,P,Do,C)).
inserirUtImpreciso(utente(ID,S,No,D,[X|Y],T,M,P,Do,C)) :-
	    evolucao( (excecao(utente(ID,S,No,D,X,T,M,P,Do,C)))),
	    inserirUtImpreciso(utente(ID,S,No,D,Y,T,M,P,Do,C)).


% para o telefone
inserirUtImpreciso(utente(ID,S,No,D,E,[],M,P,Do,C)).
inserirUtImpreciso(utente(ID,S,No,D,E,[X|Y],M,P,Do,C)) :-
	    evolucao( (excecao(utente(ID,S,No,D,E,X,M,P,Do,C)))),
	    inserirUtImpreciso(utente(ID,S,No,D,E,Y,M,P,Do,C)).


% para a morada
inserirUtImpreciso(utente(ID,S,No,D,E,T,[],P,Do,C)).
inserirUtImpreciso(utente(ID,S,No,D,E,T,[X|Y],P,Do,C)) :-
	    evolucao( (excecao(utente(ID,S,No,D,E,T,X,P,Do,C)))),
	    inserirUtImpreciso(utente(ID,S,No,D,E,T,Y,P,Do,C)).

% para a profissao
inserirUtImpreciso(utente(ID,S,No,D,E,T,M,[],Do,C)).
inserirUtImpreciso(utente(ID,S,No,D,E,T,M,[X|Y],Do,C)) :-
	    evolucao( (excecao(utente(ID,S,No,D,E,T,M,X,Do,C)))),
	    inserirUtImpreciso(utente(ID,S,No,D,E,T,M,Y,Do,C)).


% para as doencas_cron
inserirUtImpreciso(utente(ID,S,No,D,E,T,M,P,[],C)).
inserirUtImpreciso(utente(ID,S,No,D,E,T,M,P,[X|Y],C)) :-
	    evolucao( (excecao(utente(ID,S,No,D,E,T,M,P,X,C)))),
	    inserirUtImpreciso(utente(ID,S,No,D,E,T,M,P,Y,C)).


% para o centro_saude
inserirUtImpreciso(utente(ID,S,No,D,E,T,M,P,Do,[])).
inserirUtImpreciso(utente(ID,S,No,D,E,T,M,P,Do,[X|Y])) :-
	    evolucao( (excecao(utente(ID,S,No,D,E,T,M,P,Do,X)))),
	    inserirUtImpreciso(utente(ID,S,No,D,E,T,M,P,Do,Y)).

%Inserir CENTRO DE SAUDE impreciso
% para o nome 
inserirCSImpreciso(centro_saude(ID,[],M,T,E)).
inserirCSImpreciso(centro_saude(ID,[X|Y],M,T,E)) :-
	    evolucao( (excecao(centro_saude(ID,X,M,T,E)))),
	    inserirUtImpreciso(centro_saude(ID,Y,M,T,E)).


% para a morada 
inserirCSImpreciso(centro_saude(ID,No,[],T,E)).
inserirCSImpreciso(centro_saude(ID,No,[X|Y],T,E)) :-
	    evolucao( (excecao(centro_saude(ID,No,X,T,E)))),
	    inserirUtImpreciso(centro_saude(ID,No,Y,T,E)).


% para o Telefone
inserirCSImpreciso(centro_saude(ID,No,M,[],E)).
inserirCSImpreciso(centro_saude(ID,No,M,[X|Y],E)) :-
	    evolucao( (excecao(centro_saude(ID,No,M,X,E)))),
	    inserirUtImpreciso(centro_saude(ID,No,M,Y,E)).

% para o email 
inserirCSImpreciso(centro_saude(ID,No,M,T,[])).
inserirCSImpreciso(centro_saude(ID,No,M,T,[X|Y])) :-
	    evolucao( (excecao(centro_saude(ID,No,M,T,X)))),
	    inserirUtImpreciso(centro_saude(ID,No,M,T,Y)).

%Inserir STAFF impreciso 
% para o idCentro 
inserirSImpreciso(staff(ID,[],N,E)).
inserirSImpreciso(staff(ID,[X|Y],N,E)) :-
	    evolucao( (excecao(staff(ID,X,N,E)))),
	    inserirUtImpreciso(staff(ID,Y,N,E)).

% para o nome 
inserirSImpreciso(staff(ID,IDu,[],E)).
inserirSImpreciso(staff(ID,IDu,[X|Y],E)) :-
	    evolucao( (excecao(staff(ID,IDu,X,E)))),
	    inserirUtImpreciso(staff(ID,IDu,Y,E)).


% para o email 
inserirSImpreciso(staff(ID,IDu,N,[])).
inserirSImpreciso(staff(ID,IDu,N,[X|Y])) :-
	    evolucao( (excecao(staff(ID,IDu,N,X)))),
	    inserirUtImpreciso(staff(ID,IDu,N,Y)).


%Inserir VACINACAO_COVID impreciso - vacinacao_covid(IDs,IDu,D,V,T)
% para o idStaff
inserirVImpreciso(vacinacao_covid([],IDu,D,V,T)).
inserirVImpreciso(vacinacao_covid([X|Y],IDu,D,V,T)) :-
	    evolucao( (excecao(vacinacao_covid(X,IDu,D,V,T)))),
	    inserirUtImpreciso(vacinacao_covid(Y,IDu,D,V,T)).


% para o id Utente 
inserirVImpreciso(vacinacao_covid(IDs,[],D,V,T)).
inserirVImpreciso(vacinacao_covid(IDs,[X|Y],D,V,T)) :-
	    evolucao( (excecao(vacinacao_covid(IDs,X,D,V,T)))),
	    inserirUtImpreciso(vacinacao_covid(IDs,Y,D,V,T)).


% para a data 
inserirVImpreciso(vacinacao_covid(IDs,IDu,[],V,T)).
inserirVImpreciso(vacinacao_covid(IDs,IDu,[X|Y],V,T)) :-
	    evolucao( (excecao(vacinacao_covid(IDs,IDu,X,V,T)))),
	    inserirUtImpreciso(vacinacao_covid(IDs,IDu,Y,V,T)).

% para a vacina 
inserirVImpreciso(vacinacao_covid(IDs,IDu,D,[],T)).
inserirVImpreciso(vacinacao_covid(IDs,IDu,D,[X|Y],T)) :-
	    evolucao( (excecao(vacinacao_covid(IDs,IDu,D,X,T)))),
	    inserirUtImpreciso(vacinacao_covid(IDs,IDu,D,Y,T)).


% para a toma 
inserirVImpreciso(vacinacao_covid(IDs,IDu,D,V,[])).
inserirVImpreciso(vacinacao_covid(IDs,IDu,D,V,[X|Y])) :-
	    evolucao( (excecao(vacinacao_covid(IDs,IDu,D,V,X)))),
	    inserirUtImpreciso(vacinacao_covid(IDs,IDu,D,V,Y)).
