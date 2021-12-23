{- |
Module      : Tarefa3_2021li1g082
Description : Representação textual do jogo
Copyright   : Diogo Marques <a100897@alunos.uminho.pt>;
            : Luís de Castro Rodrigues Caetano <a100893@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g082 where

import LI12122

instance Show Jogo where
  show (Jogo l j) = funcaoFinal l j

-- | Funções relativas á constroução do mapa

-- | A função 'dameLinhaMapa' é capaz de transformar uma lista de Peca numa Sting

dameLinhaMapa :: [Peca] -> String
dameLinhaMapa [] = []
dameLinhaMapa (x:xs) | x == Vazio = ' ' : dameLinhaMapa xs
                     | x == Bloco = 'X' : dameLinhaMapa xs
                     | x == Porta = 'P' : dameLinhaMapa xs
                     | x == Caixa = 'C' : dameLinhaMapa xs

{- | Com o auxilio da função 'dameLinhaMapa', a função 'dameMapaTodo' retorna uma Sting, String na qual a divisão entre linhas é representada por '\n':

== Exemplos de utilização:
>>> dameMapaTodo [[Vazio,Bloco],[Porta,Bloco],[Caixa,Vazio]]
" X \nPX \nC  "
-}

dameMapaTodo :: Mapa -> String
dameMapaTodo [] = []
dameMapaTodo [x] = dameLinhaMapa x
dameMapaTodo ((x:xs):y) = dameLinhaMapa (x:xs) ++ "\n" ++  dameMapaTodo y

-- | FUNÇÕES RELATIVAS AO JOGADOR

-- | A função 'trocaLetra' é capaz de inserir o Jogador corretamente numa String

trocaLetra :: Coordenadas -> Jogador -> String -> String
trocaLetra (x,y) (Jogador (a,b) d f) [] = []
trocaLetra (x,y) (Jogador (a,b) d f) (z:zs) | x == a && y == b && d == Este = '>' : zs
                                            | x == a && y == b && d == Oeste = '<' : zs
                                            | z == '\n' = z : trocaLetra (0,y+1) (Jogador (a,b) d f) zs
                                            | otherwise = z : trocaLetra (x+1,y) (Jogador (a,b) d f) zs


-- | Função que retorna todas as coordenadas de uma String

dameCoordenadas :: Coordenadas -> String -> [Coordenadas]
dameCoordenadas (x,y) [] = []
dameCoordenadas (x,y) (z:zs) | z == ' ' = dameCoordenadas (x+1,y) zs
                             | z == '\n' = dameCoordenadas (0,y+1) zs
                             | otherwise = (x,y) : dameCoordenadas (x+1,y) zs

-- | Esta função verifica se o Jogador está apoiado numa em alguma Peca 

analisaBase :: Coordenadas -> [Coordenadas] -> Bool
analisaBase (x,y) [] = False
analisaBase (x,y) ((a,b):c) | x == a && y == b-1 = True
                            | otherwise = analisaBase (x,y) c

-- | A função 'dameColuna' retorna todas as coordenadas com a mesma abcissa da coordenada do Jogador

dameColuna :: Coordenadas -> [Coordenadas] -> [Coordenadas]
dameColuna (x,y) [] = []
dameColuna (x,y) ((a,b):c) | x == a = (a,b) : dameColuna (x,y) c
                           | otherwise = dameColuna (x,y) c

-- | A função 'retirasuperior' retorna uma lista de coordenadas com todas as coordenadas abaixo do Jogador 

retirasuperior :: Coordenadas -> [Coordenadas] -> [Coordenadas] 
retirasuperior (x,y) [] = []
retirasuperior (x,y) ((a,b):c) | y >= b = retirasuperior (x,y) c
                               | otherwise = (a,b) : retirasuperior (x,y) c

-- | Dada uma lista de Coordenadas a função 'terraAlta' retorna a coordenada com menor ordenada  

terraAlta :: [Coordenadas] -> Coordenadas
terraAlta [(x,y)] = (x,y)
terraAlta ((x,y):(a,b):c) | y < b = terraAlta ((x,y):c)  
                          | otherwise = terraAlta ((a,b):c)


-- | A funçao 'gravidade' impede que o Jogador fique enterrado entre Blocos 

gravidade :: Coordenadas -> Bool -> Jogador -> Jogador
gravidade (x,y) r (Jogador (a,b) d f) | r == True = (Jogador (a,b) d f)
                                      | otherwise = (Jogador (a,y-1) d f)

-- | Esta função foi criada com o objetivo de nunca deixa o jogador a flutuar, ou seja, colocar o Jogador sempre no chão 

gravidadeFinal :: String -> Jogador -> Jogador
gravidadeFinal l (Jogador (a,b) d f) = gravidade (terraAlta (retirasuperior (a,b) (dameColuna (a,b) (dameCoordenadas (0,0) l)))) (analisaBase (a,b) (dameCoordenadas (0,0) l)) (Jogador (a,b) d f)


-- | A 'funcaoFinal' retorna um String na qual o Mapa está devidamente representado e o Jogador nunca está a flutuar

funcaoFinal :: Mapa -> Jogador -> String
funcaoFinal l j = trocaLetra (0,0) (gravidadeFinal (dameMapaTodo l) j) (dameMapaTodo l)