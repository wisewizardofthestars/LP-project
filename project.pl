% LP 2021/2022  %

% Solucionador de Puzzles Hashi (Parte 1)


% 2.1 extrai_ilhas_linha(N_L, Linha, Ilhas) : 
/*N_L -> inteiro positivo
Linha -> e uma lista correspondente a uma linha de um puzzle
Ilhas -> lista ordenada (esquerda para a direita) de ilhas da linha Linha*/

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
/*Puz -> e um puzzle 
Ilhas -> lista organizada (esquerda para direita, cima para baixo),
cujos elementos sao ilhas de Puz*/

ilhas(Puz, IlhasF) :- ilhas(Puz,0,Ilhas),
    append(Ilhas,IlhasF).
ilhas([],_,[]).
ilhas([A|B], Cont_linhas, [Y|X]):-
    Cont_linhas1 is Cont_linhas + 1,
    %contar as linhas 
    extrai_ilhas_linha(Cont_linhas1, A, Y),
    ilhas(B, Cont_linhas1,X).

% 2.3 vizinhas(Ilhas, Ilha, Vizinhas): 
/*Ilhas -> lista de ilhas de um puzzle 
Ilha -> uma das ilhas de um puzzle contida na lista Ilhas
Vizinhas -> lista ordenada cujos elementos sao as ilhas vizinhas de Ilha*/

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
/*Entrada : elemento 1 e uma ilha,
    elemento 2 e uma lista de vizinhas dessa ilha,
    elemento 3 e a lista de pontes da ilha (vazia no inicio)
Ilhas -> lista de ilhas de um puzzle
Estado -> lista ordenada cujos elementos sao entradas referentes a cada
uma das ilhas de Ilhas*/

estado(Ilhas,Estado) :- estado(Ilhas,Estado,Ilhas).
estado([Ilha|B],[[Ilha,Vizinhas,[]]|Res],Ilhas) :-
    vizinhas(Ilhas, Ilha,Vizinhas),
    estado(B,Res,Ilhas).
estado([],[],_).


% 2.5 posicoes_entre(Pos1, Pos2, Posicoes): 
/*Pos1 e Pos2 -> posicoes
Posicoes -> e a lista ordenada de posicoes entre Pos1 e Pos2, excluindo
Se Pos1 e Pos2 nao pertencerem a mesma linha ou coluna deve devolver falso*/

posicoes_entre((X1,Y1),(X1,Y2),Posicoes) :- 
    Max is max(Y1,Y2) - 1,
    Min is min(Y1,Y2) + 1,
    findall((X1,Y),between(Min,Max,Y),Posicoes),!.
posicoes_entre((X1,Y1),(X2,Y1),Posicoes) :- 
    Max is max(X1,X2) - 1,
    Min is min(X1,X2) + 1,
    findall((X,Y1),between(Min,Max,X),Posicoes).


% 2.6 cria_ponte(Pos1, Pos2, Ponte):  
/*Pos1 e Pos2 -> posicoes
Ponte -> ponte entre essas duas posicoes*/

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
/*Pos1 e Pos2 -> posicoes
Posicoes -> lista ordenada de posicoes entre Pos1 e Pos 2
I -> e uma ilha
Vz -> e uma das vizinhas dessa ilha, ou seja, a adicao da ponte
entre Pos1 e Pos2 nao faz com que I e Vz deixem de ser Vizinhas*/

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

% 2.8 actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada,Nova_Entrada): 
/*Pos1 e Pos2 -> posicoes entre as quais vai ser adicionada uma ponte
Posicoes -> lista ordenada de posicoes entre Pos1 e Pos2
Entrada -> e uma entrada do estilo: Entrada : elemento 1 e uma ilha,
    elemento 2 e uma lista de vizinhas dessa ilha,
    elemento 3 e a lista de pontes da ilha (vazia no inicio)
Nova_Entrada -> e igual a Entrada exepto na lista de ilhas vizinhas,
esta deve ser atualizada, removendo as ilhas que deixam de ser vizinhas apos
a adicao da ponte*/

actualiza_vizinhas_entrada(Pos1,Pos2,Posicoes,[Ilha,Vizinhas|B],[Ilha,Aux|B]) :-
    posicoes_entre(Pos1,Pos2,Posicoes),
    include(caminho_livre(Pos1,Pos2,Posicoes,Ilha),Vizinhas,Aux).
    

% 2.9 actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado): 
/*Estado -> e um estado
Pos1 e Pos2 -> sao posicoes entre as quais foi adicionada uma ponte
Novo_estado e o estado que se obtem de Estado apos a atualizacao das ilhas vizinhas*/

actualiza_vizinhas_apos_pontes(Estado,Pos1,Pos2,Novo_estado) :-
    posicoes_entre(Pos1,Pos2,Posicoes),
    maplist(actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes),Estado,Novo_estado).

% 2.10 ilhas_terminadas(Estado, Ilhas_term): 
/*Estado -> e um estado
Ilhas_ter e a lista de ilhas que ja tem todas as ponets associadas, ilhas terminadas
uma ilha esta terminada se o Nr de pontes for diferente de 'X' e o comprimento
da lista de pontes (elemento 3 do estado) for igual ao Nr de pontes
*/

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


% 2.11 tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada): 
/*Estado -> e um estado
Ilhas_term -> e a lista de ilhas terminadas
Nova_entrada -> e a entrada resultante de remover as ilhas de Ilhas_term
da lista de ilhas vizinhas de entrada*/

tira_ilhas_terminadas_entrada(Ilhas_term, [Ilha,Viz_og,Pontes], [Ilha,Viz_new,Pontes]):-
    findall(Ilhas, (member(Ilhas,Viz_og),\+(member(Ilhas,Ilhas_term))),Viz_new).

% 2.12 tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado): 
/*Estado -> e um estado
Ilhas_term -> e uma lista de ilhas terminadas
Novo_estado -> e o estdo resulatnte de aplicar o predicado 2.11 a cada uma
das entradas de Estado*/

tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    maplist(tira_ilhas_terminadas_entrada(Ilhas_term),Estado,Novo_estado).

% 2.13 marca_ilhas_terminadas_entrada(Ilhas_term, Entrada,Nova_entrada):
/*Ilhas_term -> lista de ilhas terminadas
Entrada -> e uma entrada
Nova_entrada -> entrada obtida de Entrada: se a ilha de Entrada pertencer
a Ilhas_term o nr de pontes desta e substituido por 'X', caso contrario
Nova_entrada e igual a Entrada*/

marca_ilhas_terminadas_entrada(Ilhas_term, [ilha(N_L,A),Vizinhas,Pontes],[ilha('X',A),Vizinhas,Pontes]) :-
    member(ilha(N_L,A),Ilhas_term),!.
marca_ilhas_terminadas_entrada(_,Entrada,Entrada).


% 2.14 marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado):
/*Estado -> e um estado
Ilahs_term -> lista de ilhas terminadas
Novo_estado -> estado resultante de aplicar o predicado 2.13 a 
cada uma das entradas de Estado*/

marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    maplist(marca_ilhas_terminadas_entrada(Ilhas_term),Estado,Novo_estado).

% 2.15 trata_ilhas_terminadas(Estado, Novo_estado):
/*Estado -> e um estado
Novo_estado -> estado resultante de se aplicar os predicados 2.14 e 2.12
a Estado*/

trata_ilhas_terminadas(Estado, Novo_estado) :-
    ilhas_terminadas(Estado,Ilhas_term),
    tira_ilhas_terminadas(Estado,Ilhas_term,Aux),
    marca_ilhas_terminadas(Aux,Ilhas_term,Novo_estado).

% 2.16 junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado):
/*Estado -> e um estado
Ilha1 e Ilha2 -> sao 2 ilhas 
Novo_estado -> e o estado que se obtem de Estado por adicao do
Num_pontes entre Ilha1 e Ilha2*/

junta_pontes(Novo,0,ilha(_,(X1,Y1)), ilha(_,(X2,Y2)),Novo_estado) :- junta_pontes_aux(Novo,(X1,Y1),(X2,Y2),Novo_estado).
    %caso terminal que ira levar a parte final de atualizar e tratar ilhas terminadas

junta_pontes(Estado, Num_pontes, ilha(_,(X1,Y1)), ilha(_,(X2,Y2)), Novo_estado) :-
    %main function que ira servir como contadora para o nr de pontes a ser criado
    Num_pontes_new is Num_pontes - 1,
    junta_pontes_pontes(Estado,ilha(_,(X1,Y1)),ilha(_,(X2,Y2)),Aux),
    junta_pontes(Aux, Num_pontes_new, ilha(_,(X1,Y1)),ilha(_,(X2,Y2)),Novo_estado).

junta_pontes_aux(Old,(X1,Y1),(X2,Y2),New) :-
    %ultima parte, atualizar vizinhas e tartar ilhas terminadas
    actualiza_vizinhas_apos_pontes(Old,(X1,Y1),(X2,Y2),Aux),
    trata_ilhas_terminadas(Aux,New).


junta_pontes_pontes(Estado,ilha(_,(X1,Y1)),ilha(_,(X2,Y2)),Novo_estado) :-
    %funcao que cria as pontes e que ira direcionar para a funcao de as juntar ao estado
    cria_ponte((X1,Y1),(X2,Y2),Ponte),
    junta_pontes_estado(Estado,ilha(_,(X1,Y1)),ilha(_,(X2,Y2)),Novo_estado,Ponte).

junta_pontes_estado([],_,_,[],_) :- !.
%caso terminal da junta_pontes_estado

junta_pontes_estado([[Ilha,Vizinhas,Pontes_og]|B],Ilha,Ilha2,[[Ilha,Vizinhas,Pontes]|Res],Ponte) :-
    %caso a ilha a ser avaliada seja a que se tem de adicionar as pontes, isso e feito
    append(Pontes_og,[Ponte],Pontes),
    junta_pontes_estado(B,Ilha,Ilha2,Res,Ponte).

junta_pontes_estado([[Ilha,Vizinhas,Pontes_og]|B],Ilha1,Ilha,[[Ilha,Vizinhas,Pontes]|Res],Ponte) :-
    %segundo caso da ilha a ser adicionaads pontes ser esta
    append(Pontes_og,[Ponte],Pontes),
    junta_pontes_estado(B,Ilha1,Ilha,Res,Ponte).

junta_pontes_estado([[Ilha,Vizinhas,Pontes]|B],Ilha1,Ilha2,[[Ilha,Vizinhas,Pontes]|Res],Ponte) :-
    %caso seja qualquer outra ilha deve se junta ao novo estado sem alterar
    Ilha \= Ilha1,
    Ilha \= Ilha2,
    junta_pontes_estado(B,Ilha1,Ilha2,Res,Ponte).
