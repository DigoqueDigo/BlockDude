module Tarefa4_2021li1g082_Spec where

import Test.HUnit
import LI12122
import Tarefa3_2021li1g082
import Tarefa4_2021li1g082
import Fixtures

testsT4 =
  test
    [ "Tarefa 4 - Teste Move m1e1 Oeste" ~: Jogo m1r (Jogador (5, 3) Oeste False) ~=?  moveJogador m1e1 AndarEsquerda
    , "Tarefa 4 - Teste Move m1e1 Este" ~: Jogo m1r (Jogador (6, 0) Este False) ~=?  moveJogador m1e1 AndarDireita
    , "Tarefa 4 - Teste Move m1e1 Trepar" ~: m1e1 ~=? moveJogador m1e1 Trepar
    , "Tarefa 4 - Teste Move m1e1 InterageCaixa" ~: m1e1 ~=?  moveJogador m1e1 InterageCaixa
    , "Tarefa 4 - Teste movimentos m1e1" ~: m1e2 ~=?  correrMovimentos m1e1 [AndarEsquerda, Trepar, AndarEsquerda, AndarEsquerda]
    , "Tarefa 4 - Teste movimentos m1e2 Caixa1" ~: Jogo
        [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (3, 3) Este False) ~=?  correrMovimentos m1e2 [AndarDireita, InterageCaixa]
    , "Tarefa 4 - Teste movimentos m1e2 Caixa2" ~:
      Jogo
        [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Porta, Caixa, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (2, 3) Oeste False) ~=?  correrMovimentos m1e2 [AndarDireita, InterageCaixa, AndarEsquerda, InterageCaixa]
    , "Tarefa 4 - Teste Move malto InterageCaixa" ~: maltos ~=? moveJogador maltor InterageCaixa 
    , "Tarefa 4 - Teste Move Jogador sem Caixa por entre dois bloco" ~: mrandom3 ~=? moveJogador mrandom4 AndarEsquerda
    , "Tarefa 4 - Teste Move Jogador cercado por Blocos" ~: mrodar ~=? moveJogador mrodar AndarDireita
    , "Tarefa 4 - Teste movimentos aleatorios" ~: mapalinearj ~=? correrMovimentos mapalinearg [AndarEsquerda,AndarEsquerda,Trepar]
    , "Tarefa 4 - Teste movimentos mais aleatorios" ~: mrandom8 ~=? correrMovimentos mrandom7 [AndarEsquerda,InterageCaixa,AndarEsquerda,InterageCaixa,Trepar,Trepar]
    , "Tarefa 4 - Teste movimentos aleatorios simetrico" ~: mapalinearss ~=? correrMovimentos mapalinears [AndarDireita,AndarDireita,Trepar]
    , "Tarefa 4 - Teste que testa os movimentos de um Jogador com uma Caixa" ~: mcaixaj ~=? correrMovimentos mcaixag [AndarDireita,AndarDireita,AndarDireita,InterageCaixa]
    ]