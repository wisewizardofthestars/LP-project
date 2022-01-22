%:- [codigo_comum, puzzles_publicos].


% 2.1 extrai_ilhas_linha(N_L, Linha, Ilhas) : DONE, mooshak approved

extrai_ilhas_linha(N_L, Linha, Ilhas) :- extrai_ilhas_linha(N_L, Linha, 1, Ilhas).
extrai_ilhas_linha(_,[],_,[]).
extrai_ilhas_linha(N_L,[A|B],Cont,[ilha(A,(N_L,Cont))|Res]) :-
    A\==0,
    Cont1 is Cont + 1,
    extrai_ilhas_linha(N_L,B,Cont1,Res).

extrai_ilhas_linha(N_L,[A|B],Cont,Res) :-
     A==0,
     Cont1 is Cont + 1,
     extrai_ilhas_linha(N_L,B,Cont1,Res).

% 2.2 ilhas(Puz, Ilhas) : DONE, mooshak approved

ilhas(Puz, IlhasF) :- ilhas(Puz,0,Ilhas),
    append(Ilhas,IlhasF).
ilhas([],_,[]).
ilhas([A|B], Cont_linhas, [Y|X]):-
    Cont_linhas1 is Cont_linhas + 1,
    extrai_ilhas_linha(Cont_linhas1, A, Y),
    ilhas(B, Cont_linhas1,X).

% 2.3 vizinhas(Ilhas, Ilha, Vizinhas):
% vizinhas(Ilhas,Ilha,Vizinhas) :- vizinhas(Ilhas,Ilha,Vizinhas,Aux_coluna,Aux_linha).
% vizinhas([],_,[],_).
% vizinhas ([ilha(_,(Linha_o,Coluna_o))|B],ilha(_,(Linha,Coluna)),_,_,[[ilha(_,(Linha_o,Coluna_o))|X]) :-
%     Linha_o=:=Linha,
%     vizinhas(B,ilha(_,(Linha,Coluna)),_,_,X).
% vizinhas ([ilha(_,(Linha_o,Coluna_o))|B],ilha(_,(Linha,Coluna)),_,[[ilha(_,(Linha_o,Coluna_o))|X],_) :-
%     Coluna_o=:=Coluna,
%     vizinhas(B,ilha(_,(Linha,Coluna)),_,X,_).

%new try with built in
vizinhas(Ilhas,Ilha,Vizinhas) :- 
    % findall(Ilha,(Ilha=ilha(_,(Linha, Coluna)),member(Ilha,Ilhas),Linha=:=Linha_og,Coluna>Coluna_og,),Colunas_Maiores),
    % %Mesma Linha Colunas maiores ou seja terá de ser o mini da diferença
    % findall(Ilha,(Ilha=ilha(_,(Linha, Coluna)),member(Ilha,Ilhas),Linha=:=Linha_og,Coluna<Coluna_og,),Colunas_Menores),
    % %Mesma Linha Colunas menores ou seja  terá de ser o máx de diferença
    % findall(Ilha,(Ilha=ilha(_,(Linha, Coluna)),member(Ilha,Ilhas),Coluna=:=Coluna_og,Linha>Linha_og),Linhas_Maiores),
    % %Mesma Coluna, Linhas maiores ou seja terá de ser o min de diferença
    % findall(Ilha,(Ilha=ilha(_,(Linha, Coluna)),member(Ilha,Ilhas),Coluna=:=Coluna_og,Linha<Linha_og),Linhas_Menores),
    %Mesma Coluna , Linhas Menores ou seja terá de ser o maior de diferneça
    Ilha = ilha(N_L,(Linha_og,Coluna_og)),
    writeln(Linha_og),
    writeln(Coluna_og),
    findall(ilha(N_L,(Linha, Coluna)),(member(ilha(N_L,(Linha, Coluna)),Ilhas),Linha=:=Linha_og,Coluna>Coluna_og),Colunas_Maiores),
    %Mesma Linha Colunas maiores ou seja terá de ser o mini da diferença
    findall(ilha(N_L,(Linha, Coluna)),(member(ilha(N_L,(Linha, Coluna)),Ilhas),Linha=:=Linha_og,Coluna<Coluna_og),Colunas_Menores),
    %Mesma Linha Colunas menores ou seja  terá de ser o máx de diferença
    findall(ilha(N_L,(Linha, Coluna)),(member(ilha(N_L,(Linha, Coluna)),Ilhas),Coluna=:=Coluna_og,Linha>Linha_og),Linhas_Maiores),
    %Mesma Coluna, Linhas maiores ou seja terá de ser o min de diferença
    findall(ilha(N_L,(Linha, Coluna)),(member(ilha(N_L,(Linha, Coluna)),Ilhas),Coluna=:=Coluna_og,Linha<Linha_og),Linhas_Menores),
    %Mesma Coluna , Linhas Menores ou seja terá de ser o maior de diferença
    
    
    findall(ilha(N_L,(X,Y)),(member(ilha(N_L,(X,Y)),Colunas_Maiores),max(Y-Coluna_og)),C_M),
    findall(ilha(N_L,(X,Y)),(member(ilha(N_L,(X,Y)),Colunas_Menores),min(Y-Coluna_og)),C_m),
    findall(ilha(N_L,(X,Y)),(member(ilha(N_L,(X,Y)),Linhas_Maiores),max(X-Linha_og)),L_M),
    findall(ilha(N_L,(X,Y)),(member(ilha(N_L,(X,Y)),Linhas_Menores),min(X-Linha_og)),L_m),

    append([C_M,C_m,L_m,L_M],Vizinhas),
    writeln(Vizinhas).
    




    

    
    
    


% vizinhas([A|B], Ilha, [A|Res]) :- 
    

% 2.4 estado(Ilhas, Estado): cant test bc vizinhas isnt defined
estado(Ilhas,Estado) :- 
    Ilhas = [Ilha | _],
    findall([X], member(X,Ilhas), Estado),
    estado(Ilhas, Ilha, Estado).
estado([],_,[]).
estado(Ilhas,[A|B],[[A,Y|[]]|Res]) :-
    vizinhas(Ilhas,A,Y),
    estado(Ilhas,B,Res).



% 2.5 posicoes_entre(Pos1, Pos2, Posicoes):

% 2.6 cria_ponte(Pos1, Pos2, Ponte):
% 2.7 caminho_livre(Pos1, Pos2, Posicoes, I, Vz):
% 2.8 actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada,Nova_Entrada):
% 2.9 actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado):
% 2.10 ilhas_terminadas(Estado, Ilhas_term):
% 2.11 tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada):
% 2.12 tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado):
% 2.13 marca_ilhas_terminadas_entrada(Ilhas_term, Entrada,Nova_entrada):
% 2.14 marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado):
% 2.15 trata_ilhas_terminadas(Estado, Novo_estado):
% 2.16 junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado):

