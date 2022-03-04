module Tarefa1_2021li1g082_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g082
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: validaPotencialMapa m1 ~=? True
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com 2 Portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))] ~=?  False
    , "Tarefa 1 - Teste Valida Mapa com coordenadas iguais" ~: validaPotencialMapa [(Bloco,(0,0)),(Porta,(2,0)),(Bloco,(0,0)),(Bloco,(0,3)),(Bloco, (1,3)),(Bloco, (2,3))] ~=? False  
    , "Tarefa 1 - Teste Valida Mapa com uma Caixa flutuante" ~: validaPotencialMapa [(Bloco, (0,0)),(Porta,(2,0)),(Bloco,(0,3)),(Bloco, (1,3)),(Bloco,(2,3)),(Caixa,(1,1))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com um Bloco por baixo de uma base" ~: validaPotencialMapa [(Bloco,(0,3)),(Bloco,(2,0)),(Bloco,(1,4)),(Bloco,(2,3)),(Bloco, (0,0)),(Bloco,(0,3))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com uma Porta flutuante" ~: validaPotencialMapa [(Bloco,(0,3)),(Bloco,(2,0)),(Bloco,(1,4)),(Bloco,(2,3)),(Bloco, (0,0)),(Porta,(0,3))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com uma Caixa a servir de base" ~: validaPotencialMapa [(Bloco,(0,3)),(Bloco,(2,0)),(Bloco,(1,4)),(Bloco,(2,3)),(Bloco,(0,0)),(Caixa,(3,3))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com uma Caixa apoiada num bloco" ~: validaPotencialMapa [(Bloco,(0,4)),(Bloco,(2,0)),(Bloco,(1,4)),(Bloco,(2,4)),(Bloco, (0,0)),(Caixa, (1,3)),(Porta,(0,3))] ~=? True
    , "Tarefa 1 - Teste Valida Mapa com uma Porta a servir de base" ~: validaPotencialMapa [(Bloco,(0,3)),(Bloco,(2,0)),(Bloco,(2,3)),(Bloco,(2,4)),(Bloco, (0,0)),(Porta, (1,3))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa sem Blocos" ~: validaPotencialMapa [(Porta, (0,2)), (Caixa, (3,1))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com uma Caixa por de baixo da base " ~: validaPotencialMapa [(Porta,(0,2)),(Bloco,(0,3)),(Bloco,(2,0)),(Bloco,(1,4)),(Bloco,(2,4)),(Bloco, (0,0)),(Caixa, (0,4))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com uma Caixa por de baixo de um Bloco" ~: validaPotencialMapa [(Bloco,(0,4)),(Bloco,(2,0)),(Bloco,(1,4)),(Bloco,(2,4)),(Bloco, (0,0)),(Caixa, (3,0))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com uma Caixa rodeado por Blocos" ~: validaPotencialMapa [(Bloco,(1,4)),(Bloco,(0,3)),(Bloco,(1,2)),(Bloco,(2,3)),(Caixa,(1,3)), (Porta,(0,2))] ~=? True
    , "Tarefa 1 - Teste Valida Mapa cheio de Blocos e uma Porta" ~: validaPotencialMapa [(Porta,(0,0)),(Bloco,(0,1)),(Bloco,(0,2)),(Bloco,(1,0)),(Bloco,(1,2)),(Bloco,(1,3)),(Bloco,(0,3))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa sem Porta" ~: validaPotencialMapa [(Bloco,(0,3)),(Bloco,(2,0)),(Bloco,(1,4)),(Bloco,(2,4)),(Bloco, (0,0)),(Caixa, (0,4))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com Caixa flutuante e Caixa apoiada" ~: validaPotencialMapa [(Bloco,(0,3)),(Bloco,(1,4)),(Porta,(0,2)),(Bloco,(2,4)),(Bloco,(3,3)),(Caixa,(3,2)),(Caixa,(1,0))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com Caixa apoiada em Caixa" ~: validaPotencialMapa [(Bloco,(0,3)),(Bloco,(1,4)),(Porta,(0,2)),(Bloco,(2,4)),(Bloco,(3,3)),(Caixa,(3,2)),(Caixa,(3,1))] ~=? True
    , "Tarefa 1 - Teste Valida Mapa com base só com Caixas" ~: validaPotencialMapa [(Caixa, (0,3)),(Bloco, (0,1)),(Caixa, (1,3)),(Caixa, (2,4)),(Caixa, (3,3)),(Porta, (0,0))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com mapa na diagonal com caixa apoiada fora do mapa e porta dentro" ~: validaPotencialMapa [(Bloco, (0,4)),(Bloco, (1,3)),(Bloco, (2,2)),(Bloco, (3,1)),(Bloco, (4,0)),(Bloco, (4,3)), (Caixa,(4,2)), (Porta, (2,1))] ~=? False
    ]
-- | PRIMEIROS TESTES REALIZADOS

lp01 :: [(Peca, Coordenadas)]
lp01 = [(Bloco, (0,1)), (Bloco, (1,1)), (Porta, (0,0))]

lp02 :: [(Peca, Coordenadas)]
lp02 = [(Bloco, (0,1)), (Bloco, (1,1)), (Porta, (0,0)), (Caixa, (1, 0))]

lp03 :: [(Peca, Coordenadas)]
lp03 = [(Bloco, (0,2)), (Bloco, (1,2)), (Porta, (0,1)), (Caixa, (1, 0))]

lp04 :: [(Peca, Coordenadas)]
lp04 = [(Bloco, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)), (Caixa, (5, 0)), (Porta, (0,1))]

lp05 :: [(Peca, Coordenadas)]
lp05 = [(Bloco, (0,0)), (Bloco, (1,1)), (Bloco, (2,1)), (Bloco, (3,2)), (Bloco, (4,3)), (Bloco, (5,2)), (Bloco, (6,1)), (Caixa, (1,0)), (Porta, (3,1))]

lp06 :: [(Peca, Coordenadas)]
lp06 = [(Bloco, (0,0)), (Bloco, (1,1)), (Bloco, (2,1)), (Bloco, (3,2)), (Bloco, (4,3)), (Bloco, (5,2)), (Bloco, (6,1)), (Caixa, (1,0)), (Porta, (3,1)), (Bloco, (0,3)), (Caixa, (6,1)), (Caixa, (6,3))]

lp07 :: [(Peca, Coordenadas)]
lp07 = [(Bloco, (0,0)), (Bloco, (1,1)), (Bloco, (2,1)), (Bloco, (3,2)), (Bloco, (4,3)), (Bloco, (5,2)), (Bloco, (6,1)), (Caixa, (1,0)), (Bloco, (1,0)), (Porta, (3,1))]

lp08 :: [(Peca, Coordenadas)]
lp08 = [(Porta, (0,0)), (Bloco, (1,1)), (Bloco, (2,1)), (Bloco, (3,2)), (Bloco, (4,3)), (Bloco, (5,2)), (Bloco, (6,1)), (Caixa, (1,0)), (Porta, (3,1))]