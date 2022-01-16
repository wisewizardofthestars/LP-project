%:- [codigo_comum, puzzles_publicos].

% 2.1 extrai_ilhas_linha(N_L, Linha, Ilhas) : DONE

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

% 2.2 ilhas(Puz, Ilhas) :

ilhas(Puz, Ilhas) :- ilhas(Puz,0,Ilhas).
ilhas([A|B], Cont_linhas, [Y|X]):-
    Cont_linhas1 is Cont_linhas + 1,
    extrai_ilhas(Cont_linhas1, A, Y),
    ilhas(B, Cont_linhas1,X).

% 2.3 vizinhas(Ilhas, Ilha, Vizinhas):

vizinhas([A|B], Ilha, [A|Res]) :- 
    

% 2.4 estado(Ilhas, Estado):
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

