module Tarefa2_2021li1g082_Spec where

import Data.List (sort)
import Test.HUnit
import LI12122
import Tarefa2_2021li1g082
import Fixtures

testsT2 =
  test
    [ "Tarefa 2 - Teste Construir Mapa m1" ~: m1r ~=? constroiMapa m1
    , "Tarefa 2 - Teste Construir Mapa vazio" ~: [] ~=? constroiMapa []
    , "Tarefa 2 - Teste Desconstruir Mapa m1" ~: sort m1 ~=?  sort (desconstroiMapa m1r)
    , "Tarefa 2 - Teste Desconstruir Mapa vazio" ~: [] ~=? desconstroiMapa []
    , "Tarefa 2 - Teste Identidade m1" ~: sort m1 ~=?  sort (desconstroiMapa (constroiMapa m1))
    , "Tarefa 2 - Teste Identidade m1r" ~: m1r ~=?  constroiMapa (desconstroiMapa m1r)
    , "Tarefa 2 - Teste Construir Sobrepor Peças" ~: constroiMapa [(Porta, (7, 4))] ~=?  constroiMapa [(Porta, (7, 4)), (Porta, (7, 4))]
    , "Tarefa 2 - Teste Constroi Mapa complexo" ~: constroiMapa [(Bloco,(0,3)),(Bloco,(1,4)),(Bloco,(2,4)),(Bloco,(3,3)),(Bloco,(4,4)),(Caixa,(4,3)),(Bloco,(5,4)),(Porta, (0,2)),(Bloco,(3,2)),(Bloco,(6,3)),(Bloco,(5,2)),(Caixa,(4,2)),(Bloco,(2,1)),(Bloco,(3,0)),(Caixa,(2,0))] ~=? [[Vazio,Vazio,Caixa,Bloco,Vazio,Vazio,Vazio],[Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio],[Porta,Vazio,Vazio,Bloco,Caixa,Bloco,Vazio],[Bloco,Vazio,Vazio,Bloco,Caixa,Vazio,Bloco],[Vazio,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio]]
    , "Tarefa 2 - Teste Desconstroi Mapa complexo" ~: desconstroiMapa [[Vazio,Vazio,Caixa,Bloco,Vazio,Vazio,Vazio],[Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio],[Porta,Vazio,Vazio,Bloco,Caixa,Bloco,Vazio],[Bloco,Vazio,Vazio,Bloco,Caixa,Vazio,Bloco],[Vazio,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio]] ~=? [(Caixa,(2,0)),(Caixa,(4,2)),(Caixa,(4,3)),(Bloco,(3,0)),(Bloco,(2,1)),(Bloco,(3,2)),(Bloco,(5,2)),(Bloco,(0,3)),(Bloco,(3,3)),(Bloco,(6,3)),(Bloco,(1,4)),(Bloco,(2,4)),(Bloco,(4,4)),(Bloco,(5,4)),(Porta,(0,2))]
    , "Tarefa 2 - Teste face ao ponto mais abaixo e à direita do mapa, devolve uma lista de vazios" ~: dameMapaVazios (2,2) ~=? [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio]]
    , "Tarefa 2 - Teste que dado um Mapa conta quantas vezes aparece uma dada Peça neste mesmo" ~: auxcontaRepete2 Bloco [[Bloco,Porta,Bloco,Vazio,Bloco],[Bloco,Vazio,Bloco,Vazio,Bloco],[Bloco,Caixa,Vazio,Vazio,Bloco]] ~=? 8
    , "Tarefa 2 - Teste que conta todas as Peças de um Mapa" ~: contaRepete [Porta,Bloco,Caixa] [[Bloco,Porta,Bloco,Vazio,Bloco],[Caixa,Vazio,Bloco,Vazio,Bloco],[Bloco,Caixa,Vazio,Vazio,Bloco]] ~=? [1,7,2]
    , "Tarefa 2 - Teste que conta todas as coordenadas de Peças num Mapa" ~: contaTodasCoordenadas [Bloco,Porta] [[Bloco,Porta,Bloco,Vazio,Bloco]] ~=? [(0,0),(2,0),(4,0),(1,0)]
    , "Tarefa 2 - Teste que face a uma Mapa, ignora todas as Peças 'Vazio'" ~: ignoraVaziosMapa [[Vazio,Vazio,Caixa,Bloco,Vazio,Vazio,Vazio],[Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio],[Porta,Vazio,Vazio,Bloco,Caixa,Bloco,Vazio],[Bloco,Vazio,Vazio,Bloco,Caixa,Vazio,Bloco],[Vazio,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio]] ~=? [[Caixa,Bloco],[Bloco],[Porta,Bloco,Caixa,Bloco],[Bloco,Bloco,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco]]
    , "Tarefa 2 - Teste que dado um Mapa de Vazios e uma lista de pares de Peças e Coordenadas, as substitui" ~: trocaTodasPeca [(Bloco,(0,1)),(Bloco,(4,1)),(Caixa,(0,0))] [[Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio]] ~=? [[Caixa,Vazio,Vazio,Vazio],[Bloco,Vazio,Vazio,Vazio]]
    , "Tarefa 2 - Teste que usa a funcao 'multiplica'" ~: multiplica [6,2] [Bloco,Caixa] ~=? [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Caixa,Caixa]
    ]