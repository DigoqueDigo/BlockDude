module Fixtures where

import LI12122

m1 :: [(Peca, Coordenadas)]
m1 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 3)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1))
  ]

m1r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (6, 0) Oeste False)

m1e2 :: Jogo
m1e2 = Jogo m1r (Jogador (2, 3) Oeste False)

m2r :: Mapa 
m2r = [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Bloco, Bloco, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

m2r1 :: Jogo 
m2r1 = Jogo m2r (Jogador (5,2) Oeste False)

m3r :: Mapa
m3r = [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Bloco, Bloco, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

m3r1 :: Jogo 
m3r1 = Jogo m3r (Jogador (5,0) Oeste False)

m4r :: Mapa
m4r = [[Vazio,Vazio,Vazio,Bloco,Bloco,Bloco],[Vazio,Vazio,Vazio,Bloco,Bloco,Bloco],[Porta,Vazio,Caixa,Bloco,Bloco,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]

m4r1 :: Jogo 
m4r1 = Jogo m4r (Jogador (2,2) Este False)

jogador1 :: Jogador
jogador1 = (Jogador (2,0) Oeste False)

jogador1r :: Jogador
jogador1r = (Jogador (2,3) Oeste False)

mcomplexo :: Mapa 
mcomplexo = [[Vazio,Vazio,Caixa,Bloco,Vazio,Vazio],[Vazio,Vazio,Bloco,Vazio,Vazio,Vazio],[Porta,Vazio,Vazio,Bloco,Caixa,Bloco],[Bloco,Vazio,Vazio,Bloco,Caixa,Vazio],[Vazio,Bloco,Bloco,Vazio,Bloco,Bloco]]

mcomplexor :: Jogo 
mcomplexor = Jogo mcomplexo (Jogador (5,2) Oeste False)

malto :: Mapa
malto = [[Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Bloco,Vazio],[Vazio,Vazio,Bloco,Vazio],[Bloco,Bloco,Bloco,Bloco]]

maltocai :: Mapa 
maltocai = [[Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Bloco,Vazio],[Vazio,Caixa,Bloco,Vazio],[Bloco,Bloco,Bloco,Bloco]]

maltor :: Jogo 
maltor = Jogo malto (Jogador (2,1) Oeste True)

maltos :: Jogo 
maltos = Jogo maltocai (Jogador (2,1) Oeste False)

mroda :: Mapa
mroda = [[Vazio,Vazio,Vazio,Vazio],[Bloco,Vazio,Bloco,Vazio],[Bloco,Bloco,Bloco,Bloco]]

mrodar :: Jogo 
mrodar = Jogo mroda (Jogador (1,1) Este False)

mapalinear :: Mapa
mapalinear = [[Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Bloco,Vazio,Vazio,Vazio],[Bloco,Bloco,Bloco,Bloco,Bloco]]

mapalinearg :: Jogo 
mapalinearg = Jogo mapalinear (Jogador (3,1) Oeste False)

mapalinearj :: Jogo 
mapalinearj = Jogo mapalinear (Jogador (1,0) Oeste False)

mrandom :: Mapa 
mrandom = [[Vazio,Bloco,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio],[Bloco,Bloco,Bloco,Bloco,Bloco]]

mrandomdois :: Mapa 
mrandomdois = [[Vazio,Bloco,Vazio,Vazio,Vazio],[Vazio,Vazio,Caixa,Vazio,Vazio],[Bloco,Bloco,Bloco,Bloco,Bloco]]

mrandom2 :: Jogo 
mrandom2 = Jogo mrandomdois (Jogador (2,1) Oeste True)

mrandomdoisdois :: Jogo 
mrandomdoisdois = Jogo mrandom (Jogador (2,1) Oeste True)

mrandom3 :: Jogo 
mrandom3 = Jogo mrandom (Jogador (1,1) Oeste False)

mrandom4 :: Jogo 
mrandom4 = Jogo mrandom (Jogador (2,1) Oeste False)

mrandom5 :: Mapa 
mrandom5 = [[Vazio,Vazio,Vazio,Vazio],[Bloco,Vazio,Vazio,Vazio],[Vazio,Vazio,Caixa,Vazio],[Bloco,Bloco,Bloco,Bloco]]

mrandom6 :: Mapa 
mrandom6 = [[Vazio,Vazio,Vazio,Vazio],[Bloco,Vazio,Vazio,Vazio],[Vazio,Caixa,Vazio,Vazio],[Bloco,Bloco,Bloco,Bloco]]

mrandom7 :: Jogo 
mrandom7 = Jogo mrandom5 (Jogador (3,2) Oeste False)

mrandom8 :: Jogo 
mrandom8 = Jogo mrandom6 (Jogador (0,0) Oeste False)

mrandom9 :: Mapa 
mrandom9 = [[Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Bloco],[Vazio,Caixa,Vazio,Vazio],[Bloco,Bloco,Bloco,Bloco]]

mrandom10 :: Mapa 
mrandom10 = [[Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Bloco],[Vazio,Vazio,Bloco,Vazio],[Bloco,Bloco,Bloco,Bloco]]

mrandoms :: Jogo 
mrandoms = Jogo mrandom9 (Jogador (0,1) Este False)

mrandomss :: Jogo 
mrandomss = Jogo mrandom10 (Jogador (4,0) Este False)

mapalinear2 :: Mapa 
mapalinear2 = [[Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Bloco,Vazio],[Bloco,Bloco,Bloco,Bloco,Bloco]]

mapalinears :: Jogo 
mapalinears = Jogo mapalinear2 (Jogador (1,1) Este False)

mapalinearss :: Jogo 
mapalinearss = Jogo mapalinear2 (Jogador (3,0) Este False)

mcaixa :: Mapa 
mcaixa = [[Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio],[Bloco,Bloco,Bloco,Bloco,Bloco]]

mcaixax :: Mapa
mcaixax = [[Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Caixa],[Bloco,Bloco,Bloco,Bloco,Bloco]]

mcaixag :: Jogo 
mcaixag = Jogo mcaixa (Jogador (0,1) Este True) 

mcaixaj :: Jogo 
mcaixaj = Jogo mcaixax (Jogador (3,1) Este False)

jogo1 :: Jogo
jogo1 = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Porta],[Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (0,1) Este False)

jogo2 :: Jogo
jogo2 = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio],[Porta,Vazio,Vazio,Vazio,Vazio],[Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (4,1) Este False)

jogo3 :: Jogo
jogo3 = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Bloco,Vazio,Porta],[Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (0,1) Este False)

jogo4 :: Jogo
jogo4 = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio],[Porta,Vazio,Bloco,Vazio,Vazio],[Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (4,1) Este False)

jogo5 :: Jogo
jogo5 = Jogo [[Vazio,Vazio,Bloco,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Porta],[Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (0,1) Este False)

jogo6 :: Jogo
jogo6 = Jogo [[Vazio,Vazio,Bloco,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Porta],[Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (0,1) Este True)

jogo7 :: Jogo
jogo7 = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio],[Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Bloco,Porta,Vazio,Bloco,Vazio,Vazio],[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (0,1) Este False)

jogo8 :: Jogo
jogo8 = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio],[Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Bloco,Porta,Vazio,Bloco,Vazio,Vazio],[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (0,1) Este True)

jogo9 :: Jogo
jogo9 = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio],[Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Bloco,Porta,Vazio,Bloco,Vazio,Vazio],[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (0,1) Este True)

jogo10 :: Jogo
jogo10 = Jogo [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio],[Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Bloco,Porta,Vazio,Bloco,Vazio,Vazio],[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (0,1) Este False)