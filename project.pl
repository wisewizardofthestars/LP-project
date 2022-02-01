% ist1102507    %
% Sara Pinheiro %
% LP 2021/2022  %

% Solucionador de Puzzles Hashi (Parte 1)


% 2.1 extrai_ilhas_linha(N_L, Linha, Ilhas) : 

extrai_ilhas_linha(N_L, Linha, Ilhas) :- extrai_ilhas_linha(N_L, Linha, 1, Ilhas).
extrai_ilhas_linha(_,[],_,[]).
extrai_ilhas_linha(N_L,[A|B],Cont,[ilha(A,(N_L,Cont))|Res]) :-
    A\==0,
    %Se for diferente de 0 entao e porque e uma ilha, entao adiciona-se
    Cont1 is Cont + 1,
    extrai_ilhas_linha(N_L,B,Cont1,Res).

extrai_ilhas_linha(N_L,[A|B],Cont,Res) :-
     A==0,
     %se for igual a zero entao nao e uma ilha, 
     %mas o contador tem de aumentar a mesma
     Cont1 is Cont + 1,
     extrai_ilhas_linha(N_L,B,Cont1,Res).

% 2.2 ilhas(Puz, Ilhas) : 

ilhas(Puz, IlhasF) :- ilhas(Puz,0,Ilhas),
    append(Ilhas,IlhasF).
ilhas([],_,[]).
ilhas([A|B], Cont_linhas, [Y|X]):-
    Cont_linhas1 is Cont_linhas + 1,
    %contar as linhas 
    extrai_ilhas_linha(Cont_linhas1, A, Y),
    ilhas(B, Cont_linhas1,X).

% 2.3 vizinhas(Ilhas, Ilha, Vizinhas): 

vizinhas(Ilhas,Ilha,Vizinhas) :- 
    Ilha = ilha(_,(Linha_og,Coluna_og)),
    findall(ilha(ND,(Linha_og, Coluna)),(member(ilha(ND,(Linha_og, Coluna)),Ilhas),Coluna>Coluna_og),Colunas_Maiores),
    %Mesma Linha Colunas maiores ou seja tera de ser o min da diferenca
    findall(ilha(NE,(Linha_og, Coluna)),(member(ilha(NE,(Linha_og, Coluna)),Ilhas),Coluna<Coluna_og),Colunas_Menores),
    %Mesma Linha Colunas menores ou seja  tera de ser o max de diferenca
    findall(ilha(NB,(Linha, Coluna_og)),(member(ilha(NB,(Linha, Coluna_og)),Ilhas),Linha>Linha_og),Linhas_Maiores),
    %Mesma Coluna, Linhas maiores ou seja tera de ser o min de diferenca
    findall(ilha(NC,(Linha, Coluna_og)),(member(ilha(NC,(Linha, Coluna_og)),Ilhas),Linha<Linha_og),Linhas_Menores),
    %Mesma Coluna , Linhas Menores ou seja tera de ser o maior de diferenca
    
    ((length(Colunas_Maiores,0), C_M = []) ; (Colunas_Maiores = [C_M1 | _], C_M = [C_M1])),
    %usar o primeiro da lista, que sera o que esta mais perto
    ((length(Colunas_Menores,0), C_m = []) ; (last(Colunas_Menores, C_m1), C_m = [C_m1])),
    %usar o ultimo da lista que sera o que esta mais perto
    ((length(Linhas_Maiores,0), L_M = []) ; (Linhas_Maiores = [L_M1 | _], L_M = [L_M1])),
    ((length(Linhas_Menores,0), L_m = []) ; (last(Linhas_Menores, L_m1), L_m = [L_m1])),

    append([L_m,C_m,C_M,L_M],Vizinhas).


% 2.4 estado(Ilhas, Estado):  

estado(Ilhas,Estado) :- estado(Ilhas,Estado,Ilhas).
estado([Ilha|B],[[Ilha,Vizinhas,[]]|Res],Ilhas) :-
    vizinhas(Ilhas, Ilha,Vizinhas),
    estado(B,Res,Ilhas).
estado([],[],_).


% 2.5 posicoes_entre(Pos1, Pos2, Posicoes): 
posicoes_entre((X1,Y1),(X1,Y2),Posicoes) :- 
    Max is max(Y1,Y2) - 1,
    Min is min(Y1,Y2) + 1,
    findall((X1,Y),between(Min,Max,Y),Posicoes),!.
posicoes_entre((X1,Y1),(X2,Y1),Posicoes) :- 
    Max is max(X1,X2) - 1,
    Min is min(X1,X2) + 1,
    findall((X,Y1),between(Min,Max,X),Posicoes).


% 2.6 cria_ponte(Pos1, Pos2, Ponte):  
cria_ponte((X1,Y1),(X1,Y2),Ponte) :-
    Y1 > Y2,
    Ponte = ponte((X1,Y2),(X1,Y1)),!;
    Y1 < Y2,
    Ponte = ponte((X1,Y1),(X1,Y2)),!.
cria_ponte((X1,Y1),(X2,Y1),Ponte) :-
    X1 > X2,
    Ponte = ponte((X2,Y1),(X1,Y1)),!;
    X1 < X2,
    Ponte = ponte((X1,Y1),(X2,Y1)).




% 2.7 caminho_livre(Pos1, Pos2, Posicoes, I, Vz):
caminho_livre(Pos1,Pos2,_,ilha(_,Pos1),ilha(_,Pos2)) :- !.
caminho_livre(Pos1,Pos2,_,ilha(_,Pos2),ilha(_,Pos1)) :- !.
    %se pos1 e pos2 forem as posicoes das ilhas a serem avaliadas entao 
    %nao vao deixar de ser vizinhas
caminho_livre(_,_,Posicoes,ilha(_,(X1,Y1)),ilha(_,(X2,Y2))) :-
    posicoes_entre((X1,Y1),(X2,Y2),Pos_new),
    %Lista de posicoes entre a ilha e a sua vizinha
    %ponte que tem posicao final posicao inicial
    %lista de posicoes que a ponte ocupa
    %se a lista de posicoes entre I e Viz intersetar com a 
    %lista de posicoes da ponte entao viz e I passam a nao ser vizinhas
    findall((A,B),(member((A,B),Pos_new), member((A,B),Posicoes)),Lst),
    length(Lst,0).

% 2.8 actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada,Nova_Entrada): (1)
actualiza_vizinhas_entrada(Pos1,Pos2,Posicoes,[Ilha,Vizinhas|B],[Ilha,Aux|B]) :-
    posicoes_entre(Pos1,Pos2,Posicoes),
    include(caminho_livre(Pos1,Pos2,Posicoes,Ilha),Vizinhas,Aux).
    

% 2.9 actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado): (0.5) Done mooshak
actualiza_vizinhas_apos_pontes(Estado,Pos1,Pos2,Novo_estado) :-
    posicoes_entre(Pos1,Pos2,Posicoes),
    maplist(actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes),Estado,Novo_estado).

% 2.10 ilhas_terminadas(Estado, Ilhas_term): (1) DONE Mooshak approved
ilhas_terminadas([],[]) :- !.
ilhas_terminadas([A|B],[Ilha|Res]) :- 
    ilhas_terminadas_aux(A,Ilha),
    ilhas_terminadas(B,Res),!.

ilhas_terminadas([_|B],Res) :-
    ilhas_terminadas(B,Res).

ilhas_terminadas_aux([ilha(N,Pos),_,Res],ilha(N,Pos)) :- 
    integer(N),
    length(Res,Len),
    Len =:= N.


% 2.11 tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada): (1) doen mooshak
tira_ilhas_terminadas_entrada(Ilhas_term, [Ilha,Viz_og,Pontes], [Ilha,Viz_new,Pontes]):-
    findall(Ilhas, (member(Ilhas,Viz_og),\+(member(Ilhas,Ilhas_term))),Viz_new).

% 2.12 tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado): (1) done mooshak
tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    maplist(tira_ilhas_terminadas_entrada(Ilhas_term),Estado,Novo_estado).

% 2.13 marca_ilhas_terminadas_entrada(Ilhas_term, Entrada,Nova_entrada):(1) done mooshak
marca_ilhas_terminadas_entrada(Ilhas_term, [ilha(N_L,A),Vizinhas,Pontes],[ilha('X',A),Vizinhas,Pontes]) :-
    member(ilha(N_L,A),Ilhas_term),!.
marca_ilhas_terminadas_entrada(_,Entrada,Entrada).


% 2.14 marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado):(1) Done mooshak
marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    maplist(marca_ilhas_terminadas_entrada(Ilhas_term),Estado,Novo_estado).

% 2.15 trata_ilhas_terminadas(Estado, Novo_estado):(1)
trata_ilhas_terminadas(Estado, Novo_estado) :-
    ilhas_terminadas(Estado,Ilhas_term),
    tira_ilhas_terminadas(Estado,Ilhas_term,Aux),
    marca_ilhas_terminadas(Aux,Ilhas_term,Novo_estado).

% 2.16 junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado):(1)
junta_pontes(Novo,0,ilha(_,(X1,Y1)), ilha(_,(X2,Y2)),Novo_estado) :- junta_pontes_aux(Novo,(X1,Y1),(X2,Y2),Novo_estado).
junta_pontes(Estado, Num_pontes, ilha(_,(X1,Y1)), ilha(_,(X2,Y2)), Novo_estado) :-
    Num_pontes_new is Num_pontes - 1,
    junta_pontes_pontes(Estado,ilha(_,(X1,Y1)),ilha(_,(X2,Y2)),Aux),
    junta_pontes(Aux, Num_pontes_new, ilha(_,(X1,Y1)),ilha(_,(X2,Y2)),Novo_estado).

junta_pontes_aux(Old,(X1,Y1),(X2,Y2),New) :-
    actualiza_vizinhas_apos_pontes(Old,(X1,Y1),(X2,Y2),Aux),
    trata_ilhas_terminadas(Aux,New).


junta_pontes_pontes(Estado,ilha(_,(X1,Y1)),ilha(_,(X2,Y2)),Novo_estado) :-
    cria_ponte((X1,Y1),(X2,Y2),Ponte),
    junta_pontes_estado(Estado,ilha(_,(X1,Y1)),ilha(_,(X2,Y2)),Novo_estado,Ponte).

junta_pontes_estado([],_,_,[],_) :- !. 

junta_pontes_estado([[Ilha,Vizinhas,Pontes_og]|B],Ilha,Ilha2,[[Ilha,Vizinhas,Pontes]|Res],Ponte) :-
    append(Pontes_og,[Ponte],Pontes),
    junta_pontes_estado(B,Ilha,Ilha2,Res,Ponte).

junta_pontes_estado([[Ilha,Vizinhas,Pontes_og]|B],Ilha1,Ilha,[[Ilha,Vizinhas,Pontes]|Res],Ponte) :-
    append(Pontes_og,[Ponte],Pontes),
    junta_pontes_estado(B,Ilha1,Ilha,Res,Ponte).

junta_pontes_estado([[Ilha,Vizinhas,Pontes]|B],Ilha1,Ilha2,[[Ilha,Vizinhas,Pontes]|Res],Ponte) :-
    Ilha \= Ilha1,
    Ilha \= Ilha2,
    junta_pontes_estado(B,Ilha1,Ilha2,Res,Ponte).
