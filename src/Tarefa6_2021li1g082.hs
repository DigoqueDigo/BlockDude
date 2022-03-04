{- |
Module      : Tarefa6_2021li1g082
Description : Resolução de um puzzle

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
    
= Introdução 
    
Para esta tarefa tivemos como objetivo desenvolver um bot. Este bot teria de ser capaz de verificar se
era possível resolver determinado Mapa num certo número de movimentos, no caso de o Mapa ser impossível
seria retornado Nothing, já no caso de ser possível deveria retornar uma combinação de movimentos que
representassem uma forma de alcançar a Porta, contudo se o número de movimentos pretendidos fosse inferior
àquele da combinação obtida também seria retornado Nothing.

= Estratégias Adotadas

Quando nos debruçamos sobre esta tarefa, pensamos de imediato em criar uma função que determinasse a posição
do Jogador em relação à Porta, assim se o Jogador estivesse à esquerda da Porta, este teria de movimentar-se
para a direita e vice-versa. Depois de descoberta a posição relativa do Jogador, fizemos uso de algumas funções
utilizadas na Tarefa 4 com o objetivo de colocar o Jogador na mesma coluna que a Porta, contudo se o Jogador está
na mesma coluna que a Porta isso não significa necessariamente que a Porta tenha sido alcançada. Perante isto,
pensamos que a partir daqui deveríamos colocar o Jogador na mesma linha que a Porta, pois desta forma assegurávamos
que ambos estavam à mesma altura. Assim, ao assegurarmos isto, apenas seria necessário chamar novamente as funções
que tínhamos utilizado anteriormente para colocar o Jogador na mesma coluna que a Porta, pois aí já teríamos certamente
uma resolução para um Mapa.  

= Discussão dos Resultados Obtidos

Após a realização de vários testes, chegamos à conclusão de que o nosso bot não era capaz de interagir com uma Caixa,
pois o método que traçamos inicialmente não o permitia. Perante isto, pensamos em aplicar o mesmo raciocínio que
utilizamos para a Porta, mas agora com as Caixas, contudo, mais tarde questionamo-nos como é que o bot saberia se
a Caixa seria útil para a resolução do Mapa ou não. Para além disso, o bot teria que ser capaz de identificar em
que situações seria necessário utilizar Caixas e quantas teria de usar. Assim, com cada vez mais obstáculos a
sobreporem-se, fomos incapazes de avançar, pois o movimento de interação com as Caixas é algo muito vago, na medida
em que tanto pode ser útil à resolução de um Mapa como também pode ser inútil.


= Conclusão

Chegados a este ponto, percebemos o quão difícil é construir um bot que obedeça a todos o caso, existem determinadas
variáveis que são muito difíceis de prever, como tal, apesar de não termos cumprido esta tarefa na íntegra e de o nosso
bot não ser prefeito, estamos bastantes orgulhosos do trabalho realizado, pois sabemos que pelo menos é capaz de resolver
Mapas com alguma complexidade, ainda que baixa.
-}

module Tarefa6_2021li1g082 where

import LI12122
import Tarefa1_2021li1g082
import Tarefa2_2021li1g082  
import Tarefa3_2021li1g082  
import Tarefa4_2021li1g082

{- | Esta função permite saber se é possível resolver determinado jogo num certo número de movimentos,
contudo não fomos capazes de embutir nesta função casos em que o Jogador teria de interagir com a Caixa
para poder chegar à Porta
-}

resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo x j | x < length (movebot2 j) = Nothing
                | verificaChegada j = Just (movebot2 j) 
                | otherwise = Nothing

{- | Esta função permite atribuir a cada Peca uma coordenada, contudo a Porta e os Vazios são ignorados,
pelo que não lhes é atribuída qualquer Coordenada
-}

coordenadasTotal1 :: Coordenadas -> [Peca] -> [Coordenadas]
coordenadasTotal1 (x,y) [] = []
coordenadasTotal1 (x,y) (a:b) | a == Vazio || a == Porta = coordenadasTotal1 (x+1,y) b
                              | otherwise = (x,y) : coordenadasTotal1 (x+1,y) b 

{- | Esta função faz praticamente o mesmo que a anterior, contudo é capaz de analisar um Mapa:

== Exemplos de utilização:
>>> coordenadasTotalMapa1 (0,0) [[Vazio,Bloco],[Porta,Vazio],[Caixa,Bloco]]
[(1,0),(0,2),(1,2)]
-}

coordenadasTotalMapa1 :: Coordenadas -> Mapa -> [Coordenadas]
coordenadasTotalMapa1 (x,y) [] = []
coordenadasTotalMapa1 (x,y) (a:b) = coordenadasTotal1 (x,y) a ++ coordenadasTotalMapa1 (0,y+1) b

-- | Esta função retorna a Coordenada da Porta no Mapa, como só há uma Porta, apenas será retornada uma Coordenda  

coordenadaPorta :: Mapa -> Coordenadas
coordenadaPorta l = tiraLista (contaCoordenadas (Porta) l)

-- | Esta função recebe como argumento uma lista com uma única Coordenada e retorna essa mesma Coordenda 

tiraLista :: [Coordenadas] -> Coordenadas
tiraLista [(x,y)] = (x,y)
tiraLista (x:xs) = tiraLista [x]

-- | A função 'tiraLista2' recebe como argumento uma lista com um único Movimento e retorna esse mesmo Movimento

tiraLista2 :: [Movimento] -> Movimento
tiraLista2 [x] = x

-- | A função 'ondeIr' retorna uma String a partir da qual sabemos para onde ir, se o Jogador estiver à esquerda da Porta então terá de se mover para a direita

ondeIr :: Jogador -> Coordenadas -> String
ondeIr (Jogador (x,y) _ _ ) (a,b) | x < a = "VaiDireita"
                                  | x > a = "VaiEsquerda"
                                  | otherwise = "ChegouPorta"

-- | A função 'verificaportaesquerda' verifica se existe uma Porta imediatamente à esquerda do Jogador

verificaportaesquerda :: Jogador -> Coordenadas -> Bool
verificaportaesquerda (Jogador (x,y) _ _ ) (a,b) | x == a+1 && y == b = True
                                                 | otherwise = False

-- | Esta função verifica se existe uma Porta na primeira coordenada da diagonal esquerda do Jogador

verificaportaesquerdaTrepar :: Jogador -> Coordenadas -> Bool
verificaportaesquerdaTrepar (Jogador (x,y) _ _ ) (a,b) | x == a+1 && y == b+1 = True 
                                                       | otherwise = False

-- | A função 'verificaportaesquerda' verifica se existe uma Porta imediatamente à direita do Jogador

verificaportadireita :: Jogador -> Coordenadas -> Bool
verificaportadireita (Jogador (x,y) _ _ ) (a,b) | x == a-1 && y == b = True
                                                | otherwise = False

-- | Esta função verifica se existe uma Porta na primeira coordenada da diagonal direita do Jogador

verificaportadireitaTrepar :: Jogador -> Coordenadas -> Bool
verificaportadireitaTrepar (Jogador (x,y) _ _ ) (a,b) | x == a-1 && y == b+1 = True
                                                      | otherwise = False

-- | Quando o Jogador está à esquerda da Porta, a função 'movebotdireita1' retorna uma lista de movimetos de forma a que o Jogador fique na mesma coluna que a Porta

movebotdireita1 :: Jogo -> [Movimento] 
movebotdireita1 j@(Jogo l (Jogador (x,y) f g)) | g == False && (verificaObstaculodireita (x,y) (coordenadasTotalMapa1 (0,0) l)) == True && (verificaObstaculodireita (x,y-1) (coordenadasTotalMapa1 (0,0) l)) == False = [Trepar]
                                               | g == True && (verificaObstaculodireita (x,y) (coordenadasTotalMapa1 (0,0) l)) == True && (verificaObstaculodireita (x,y-1) (coordenadasTotalMapa1 (0,0) l)) == False && (verificaObstaculodireita (x,y-2) (coordenadasTotalMapa1 (0,0) l)) == False = [Trepar]
                                               | g == True && (verificaObstaculodireita (x,y) (coordenadasTotalMapa1 (0,0) l)) == False && (verificaObstaculodireita (x,y-1) (coordenadasTotalMapa1 (0,0) l)) == False = [AndarDireita]
                                               | g == False && (verificaObstaculodireita (x,y) (coordenadasTotalMapa1 (0,0) l)) == False = [AndarDireita]
                                               | otherwise = []

-- | Quando o Jogador está à direita da Porta, a função 'movebotesquerda1' retorna uma lista de movimetos de forma a que o Jogador fique na mesma coluna que a Porta

movebotesquerda1 :: Jogo -> [Movimento]
movebotesquerda1 j@(Jogo l (Jogador (x,y) f g)) | g == False && (verificaObstaculoesquerda (x,y) (coordenadasTotalMapa1 (0,0) l)) == True && (verificaObstaculoesquerda (x,y-1) (coordenadasTotalMapa1 (0,0) l)) == False = [Trepar]
                                                | g == True && (verificaObstaculoesquerda (x,y) (coordenadasTotalMapa1 (0,0) l)) == True && (verificaObstaculoesquerda (x,y-1) (coordenadasTotalMapa1 (0,0) l)) == False && (verificaObstaculoesquerda (x,y-2) (coordenadasTotalMapa1 (0,0) l)) == False = [Trepar]
                                                | g == True && (verificaObstaculoesquerda (x,y) (coordenadasTotalMapa1 (0,0) l)) == False && (verificaObstaculoesquerda (x,y-1) (coordenadasTotalMapa1 (0,0) l)) == False = [AndarEsquerda]
                                                | g == False && (verificaObstaculoesquerda (x,y) (coordenadasTotalMapa1 (0,0) l)) == False = [AndarEsquerda]
                                                | otherwise = []

-- | A função 'movebot1' apenas tenta colocar o bot na mesma coluna que a Porta, e para isso utiliza a 'movebotdireita1' ou a 'movebotesquerda1'

movebot1 :: Jogo -> [Movimento]
movebot1 j@(Jogo l (Jogador (x,y) f g)) | ondeIr (Jogador (x,y) f g) (coordenadaPorta l) == "ChegouPorta" = [] 
                                        | ondeIr (Jogador (x,y) f g) (coordenadaPorta l) == "VaiEsquerda" && movebotesquerda1 j == [] = []
                                        | ondeIr (Jogador (x,y) f g) (coordenadaPorta l) == "VaiDireita" && movebotdireita1 j == [] = []
                                        | ondeIr (Jogador (x,y) f g) (coordenadaPorta l) == "VaiDireita" && verificaportadireitaTrepar (Jogador (x,y) f g) (coordenadaPorta l) == True = [Trepar]
                                        | ondeIr (Jogador (x,y) f g) (coordenadaPorta l) == "VaiEsquerda" && verificaportaesquerdaTrepar (Jogador (x,y) f g) (coordenadaPorta l) == True = [Trepar]
                                        | ondeIr (Jogador (x,y) f g) (coordenadaPorta l) == "VaiDireita" && verificaportadireita (Jogador (x,y) f g) (coordenadaPorta l) == True = [AndarDireita]
                                        | ondeIr (Jogador (x,y) f g) (coordenadaPorta l) == "VaiEsquerda" && verificaportaesquerda (Jogador (x,y) f g) (coordenadaPorta l) == True = [AndarEsquerda]
                                        | ondeIr (Jogador (x,y) f g) (coordenadaPorta l) == "VaiDireita" = movebotdireita1 j ++ movebot1 (moveJogador j (tiraLista2 (movebotdireita1 j)))
                                        | ondeIr (Jogador (x,y) f g) (coordenadaPorta l) == "VaiEsquerda" = movebotesquerda1 j ++ movebot1 (moveJogador j (tiraLista2 (movebotesquerda1 j)))
                                        | otherwise = []

-- | Esta função identifica se o Jogador está na mesma linha que a Porta

ondeIr2 :: Jogo -> Coordenadas -> String
ondeIr2 (Jogo l (Jogador (x,y) f g)) (a,b) | y == b = "NaLinha"
                                           | otherwise = "ContinuaAndar"

-- | A função 'cimadaPorta' verifica se o Jogador está em cima da Porta

cimadaPorta :: Jogo -> Coordenadas -> Bool
cimadaPorta (Jogo l (Jogador (x,y) f g)) (a,b) | x == a && y == b-1 = True
                                               | otherwise = False

-- | Esta função atribui um movimento conforme a posição do Jogador relativamente à Porta 

atribuimovimento :: Jogador -> Coordenadas -> Movimento
atribuimovimento j@(Jogador (x,y) f g) (a,b) | ondeIr j (a,b) == "VaiDireita" = AndarDireita
                                             | ondeIr j (a,b) == "VaiEsquerda" = AndarEsquerda
                                             | otherwise = Trepar

-- | A função 'chegalinhadireita' coloca o Jogador na mesma linha que a Porta, sabendo que inicialmente a Porta estava à direita do Jogador

chegalinhadireita :: Jogo -> [Movimento] 
chegalinhadireita j@(Jogo l (Jogador (x,y) f g)) | ondeIr2 j (coordenadaPorta l) == "NaLinha" = []
                                                 | otherwise = movebotdireita1 j ++ chegalinhadireita (correrMovimentos j (movebotdireita1 j))

-- | A função 'chegalinhadireita' coloca o Jogador na mesma linha que a Porta, sabendo que inicialmente a Porta estava à esquerda do Jogador

chegalinhaesquerda :: Jogo -> [Movimento]
chegalinhaesquerda j@(Jogo l (Jogador (x,y) f g)) | ondeIr2 j (coordenadaPorta l) == "NaLinha" = []
                                                  | otherwise = movebotesquerda1 j ++ chegalinhaesquerda (correrMovimentos j (movebotesquerda1 j))

-- | A função 'movebot2' retorna uma tentativa de solução para um determinado Jogo, contudo não é capaz de atribuir o Movimento InteragirCaixa

movebot2 :: Jogo -> [Movimento]
movebot2 j@(Jogo l (Jogador (x,y) f g)) | cimadaPorta (correrMovimentos j (movebot1 j)) (coordenadaPorta l) == True = movebot1 j
                                        | ondeIr2 (correrMovimentos j (movebot1 j)) (coordenadaPorta l) == "NaLinha" = movebot1 j
                                        | ondeIr2 (correrMovimentos j (movebot1 j)) (coordenadaPorta l) == "ContinuaAndar" && ondeIr (Jogador (x,y) f g) (coordenadaPorta l) == "VaiDireita" = movebot1 j ++ chegalinhadireita (correrMovimentos j (movebot1 j)) ++ movebot1 (correrMovimentos j (movebot1 j ++ chegalinhadireita (correrMovimentos j (movebot1 j))))
                                        | ondeIr2 (correrMovimentos j (movebot1 j)) (coordenadaPorta l) == "ContinuaAndar" && ondeIr (Jogador (x,y) f g) (coordenadaPorta l) == "VaiEsquerda" = movebot1 j ++ chegalinhaesquerda (correrMovimentos j (movebot1 j)) ++ movebot1 (correrMovimentos j (movebot1 j ++ chegalinhaesquerda (correrMovimentos j (movebot1 j))))
                                        | otherwise = movebot1 j

-- | Esta função dá uma ideia da possivel posição do Jogador conforme os seus movimentos, contudo o eixo das ordenadas é ignorado

atribuicoordenadas :: Jogador -> [Movimento] -> Coordenadas
atribuicoordenadas (Jogador (x,y) f g) [] = (x,y)
atribuicoordenadas (Jogador (x,y) f g) (a:as) | a == AndarEsquerda = atribuicoordenadas (Jogador (x-1,y) Oeste g) as
                                              | a == AndarDireita = atribuicoordenadas (Jogador (x+1,y) Este g) as
                                              | a == Trepar && f == Este = atribuicoordenadas (Jogador (x+1,y) Este g) as
                                              | a == Trepar && f == Oeste = atribuicoordenadas (Jogador (x-1,y) Oeste g) as

{- | A função 'igualdade' analisa se duas Coordendas estão na mesma coluna:

== Exemplos de utilização:
>>> igualdade (1,6) (1,9)
True
>>> igualdade (1,6) (3,9)
False
-}

igualdade :: Coordenadas -> Coordenadas -> Bool
igualdade (x,y) (a,b) | a == x = True
                      | otherwise = False

-- | Com o auxilio das funções 'igualdade' e 'atribuicoordenadas' esta função diz se os movimentos dados pela 'movebot2' permitem que o Jogador chegue à Porta ou não

verificaChegada :: Jogo -> Bool
verificaChegada j@(Jogo l (Jogador (a,b) f g)) = igualdade (atribuicoordenadas (Jogador (a,b) f g) (movebot2 j)) (coordenadaPorta l) 