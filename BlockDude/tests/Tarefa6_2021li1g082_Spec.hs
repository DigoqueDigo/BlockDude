module Tarefa6_2021li1g082_Spec where

import Data.List (sort)
import Test.HUnit
import LI12122
import Tarefa6_2021li1g082
import Fixtures

testsT6 =
  test
    [ "Tarefa 6 - Teste apenas com movimentos para a direita" ~: movebot2 jogo1 ~=? [AndarDireita,AndarDireita,AndarDireita,AndarDireita]
    , "Tarefa 6 - Teste apenas com movimentos para a esquerda" ~: movebot2 jogo2 ~=? [AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda]
    , "Tarefa 6 - Teste com o movimento Trepar (direita)" ~: movebot2 jogo3 ~=? [AndarDireita,Trepar,AndarDireita,AndarDireita]
    , "Tarefa 6 - Teste com o movimento Trepar (esquerda)" ~: movebot2 jogo4 ~=? [AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda] 
    , "Tarefa 6 - Teste com o Jogador a passar por baixo de um Bloco" ~: movebot2 jogo5 ~=? [AndarDireita,AndarDireita,AndarDireita,AndarDireita]
    , "Tarefa 6 - Teste com o Jogador a não conseguir passar por baixo de um Bloco" ~: movebot2 jogo6 ~=? [AndarDireita]
    , "Tarefa 6 - Teste num mapa bastante complexo com o Jogador sem Caixa" ~: movebot2 jogo7 ~=? [AndarDireita,Trepar,Trepar,Trepar,AndarDireita,AndarDireita,AndarDireita,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda]
    , "Tarefa 6 - Teste num mapa bastante complexo com o Jogador com Caixa" ~: movebot2 jogo8 ~=? [AndarDireita,Trepar,Trepar,Trepar,AndarDireita,AndarDireita,AndarDireita,AndarEsquerda]
    , "Tarefa 6 - Teste com o Jogador a cair ao lado da Porta com Caixa" ~: movebot2 jogo9 ~=? [AndarDireita,Trepar,Trepar,AndarDireita]
    , "Tarefa 6 - Teste com o Jogador a cair ao lado da Porta sem Caixa" ~: movebot2 jogo10 ~=? [AndarDireita,Trepar,Trepar,AndarDireita,AndarEsquerda]
    , "Tarefa 6 - Teste num mapa simples com um número de movimentos reduzidos" ~: resolveJogo 2 jogo1 ~=? Nothing
    , "Tarefa 6 - Teste num mapa simples com um numero de movimentos suficientes" ~: resolveJogo 4 jogo1 ~=? Just [AndarDireita,AndarDireita,AndarDireita,AndarDireita]
    , "Tarefa 6 - Teste com movimentos para a esquerda e número de movimentos suficientes" ~: resolveJogo 10 jogo2 ~=? Just [AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda]
    , "Tarefa 6 - Teste com movimentos para a esquerda e número de movimentos insuficientes" ~: resolveJogo 1 jogo2 ~=? Nothing
    , "Tarefa 6 - Teste no jogo3 com movimentos suficientes" ~: resolveJogo 10 jogo3 ~=? Just [AndarDireita,Trepar,AndarDireita,AndarDireita]
    , "Tarefa 6 - Teste no jogo3 com movimentos insuficientes" ~: resolveJogo 1 jogo3 ~=? Nothing
    , "Tarefa 6 - Teste no jogo4 com movimentos suficientes" ~: resolveJogo 10 jogo4 ~=? Just [AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda]
    , "Tarefa 6 - Teste no jogo4 com movimentos insuficientes" ~: resolveJogo 1 jogo4 ~=? Nothing
    , "Tarefa 6 - Teste no jogo5 com movimentos suficientes" ~: resolveJogo 10 jogo5 ~=? Just [AndarDireita,AndarDireita,AndarDireita,AndarDireita]
    , "Tarefa 6 - Teste no jogo5 com movimentos insuficientes" ~: resolveJogo 1 jogo5 ~=? Nothing
    , "Tarefa 6 - Teste num jogo impossivel como movimentos suficientes" ~: resolveJogo 10 jogo6 ~=? Nothing
    , "Tarefa 6 - Teste num jogo impossivel como movimentos insuficientes" ~: resolveJogo 1 jogo6 ~=? Nothing
    , "Tarefa 6 - Teste num mapa complexo como movimentos suficientes" ~: resolveJogo 11 jogo7~=? Just [AndarDireita,Trepar,Trepar,Trepar,AndarDireita,AndarDireita,AndarDireita,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda]
    , "Tarefa 6 - Teste num mapa complexo como movimentos insuficientes" ~: resolveJogo 1 jogo7 ~=? Nothing
    , "Tarefa 6 - Teste num mapa complexo impossivel com movimentos suficientes" ~: resolveJogo 10 jogo8 ~=? Nothing
    , "Tarefa 6 - Teste num mapa complexo impossivel com movimentos insuficientes" ~: resolveJogo 1 jogo8 ~=? Nothing
    , "Tarefa 6 - Teste no jogo9 com movimentos suficientes" ~: resolveJogo 10 jogo9 ~=? Nothing
    , "Tarefa 6 - Teste no jogo9 com movimentos insuficientes" ~: resolveJogo 1 jogo9 ~=? Nothing
    , "Tarefa 6 - Teste no jogo10 com movimentos suficientes" ~: resolveJogo 10 jogo10 ~=? Just [AndarDireita,Trepar,Trepar,AndarDireita,AndarEsquerda]
    , "Tarefa 6 - Teste no jogo10 com movimentos insuficientes" ~: resolveJogo 1 jogo10 ~=? Nothing 
    ]