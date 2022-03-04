module Tarefa3_2021li1g082_Spec where

import Test.HUnit
import Tarefa3_2021li1g082
import Fixtures

testsT3 =
  test
    [ "Tarefa 3 - Teste Imprime Jogo m1e1" ~: "      <\n      X\n      X\nP   C X\nXXXXXXX" ~=?  show m1e1
    , "Tarefa 3 - Teste Imprime Jogo m1e2" ~: "       \n      X\n      X\nP < C X\nXXXXXXX" ~=?  show m1e2
    , "Tarefa 3 - Teste que dado um Jogo com um Jogador no ar, o mete no chão e apresenta uma String " ~: "       \n      X\n      X\nP  XX<X\nXXXXXXX" ~=? show m3r1
    , "Tarefa 3 - Teste que Imprime Jogo com um Jogador virado para Oeste" ~: "       \n      X\n      X\nP  XX<X\nXXXXXXX" ~=? show m2r1
    , "Tarefa 3 - Teste que face a um Mapa devolve uma String " ~: dameMapaTodo m2r  ~=? "       \n      X\n      X\nP  XX X\nXXXXXXX"
    , "Tarefa 3 - Teste que verifica se o jogador esta apoiado em alguma peca" ~: analisaBase (1,0) [(0,1),(1,1),(2,1)] ~=? True
    , "Tarefa 3 - Teste que dado um Jogo, devolve uma String " ~: show m4r1 ~=? "   XXX\n   XXX\nP >XXX\nXXXXXX"
    , "Tarefa 3 - Teste que dado uma String e um jogador a flutuar, o posiciona no chão" ~: gravidadeFinal "       \n      X\n      X\nP  XX X\nXXXXXXX" jogador1 ~=? jogador1r
    , "Tarefa 3 - Teste que dado as coordenadas do Jogador, devolve as coordenadas do Mapa que estão na mesma coluna que o Jogador" ~: dameColuna (1,1) [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1),(0,2),(1,2),(2,2)] ~=? [(1,0),(1,1),(1,2)]
    , "Tarefa 3 - Teste Imprime Jogo complexo" ~: "  CX  \n  X   \nP  XCX\nX  XC<\n XX XX" ~=? show mcomplexor
    , "Tarefa 3 - Teste dado um Jogo complexo retorna a sua String correspondente" ~: show mcomplexor ~=? "  CX  \n  X   \nP  XCX\nX  XC<\n XX XX"
    , "Tarefa 3 - Teste à funcao auxiliar retirasuperior" ~: retirasuperior (1,1) [(1,0),(1,1),(1,2)] ~=? [(1,2)]
    ]