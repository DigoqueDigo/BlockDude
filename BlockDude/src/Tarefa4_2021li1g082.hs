{- |
Module      : Tarefa4_2021li1g082
Description : Movimentação do personagem
Copyright   : Diogo Marques <a100897@alunos.uminho.pt>;
            : Luís de Castro Rodrigues Caetano <a100893@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g082 where

import LI12122
import Tarefa3_2021li1g082


moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo l (Jogador (a,b) d f)) m | f == False && m == AndarDireita || f == False && m == AndarEsquerda = Jogo l (moveEsquerdaDireita (Jogador (a,b)d f) l m)
                                           | f == False && m == Trepar = Jogo l (trepaEsquerdaDireita (Jogador (a,b)d f) l m) 
                                           | f == False && m == InterageCaixa = alteraMapa1 (Jogo l (Jogador (a,b) d f)) m  
                                           | f == True && m == InterageCaixa = alteraMapa1 (Jogo l (Jogador (a,b) d f)) m 
                                           | f == True && m == AndarEsquerda || f == True && m == AndarDireita = movercomCaixa (Jogo l (Jogador (a,b) d f)) m
                                           | f == True && m == Trepar = treparcomCaixa (Jogo l (Jogador (a,b) d f)) m 
                                           | otherwise = (Jogo l (Jogador (a,b) d f)) 


correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos j@(Jogo l (Jogador (a,b) d f)) [] = j
correrMovimentos j@(Jogo l (Jogador (a,b) d f)) (m:ms) = correrMovimentos (moveJogador j m) ms 

{- | A função 'maisDireita' retorna o elemento com a coordenada mais à direita:

== Exemplos de utilização:
>>> maisDireita [(Bloco, (0,0)),(Bloco, (1,0)),(Bloco, (0,1)),(Bloco, (2,1)),(Bloco, (1,1))]
(2,1)
>>> maisDireita [(0,0),(1,0),(0,1),(2,1),(1,1),(3,2)]
(3,2)
-}

maisDireita :: [Coordenadas] -> Coordenadas
maisDireita [] = (0,0)
maisDireita [(x,y)] = (x,y)
maisDireita ((x,y):(a,b):z) | x >= a = maisDireita ((x,y):z)
                            | otherwise = maisDireita ((a,b):z)

-- | A função 'ultimaAbcissa' funciona apenas como uma auxiliar para a 'moveparaDireita1'    

ultimaAbcissa :: Coordenadas -> Int
ultimaAbcissa (x,y) = x                        

-- | Esta função retorna as Coordenadas de uma lista de Peca, atribuindo a cada Peca que não seja um Vazio uma Coordenada

coordenadasTotal :: Coordenadas -> [Peca] -> [Coordenadas]
coordenadasTotal (x,y) [] = []
coordenadasTotal (x,y) (a:b) | a == Vazio = coordenadasTotal (x+1,y) b
                             | otherwise = (x,y) : coordenadasTotal (x+1,y) b 

{- | Faz o mesmo que a função anterior, contudo consegue analisar um Mapa:

== Exemplos de utilização:
>>> coordenadasTotalMapa (0,0) [[Vazio,Bloco,Caixa,Porta],[Vazio,Bloco,Vazio,Vazio]]
[(1,0),(2,0),(3,0),(1,1)]
>>> coordenadasTotalMapa (0,0) [[Vazio,Bloco,Caixa,Porta],[Vazio,Bloco,Bloco,Bloco]]
[(1,0),(2,0),(3,0),(1,1),(2,1),(3,1)]
-}

coordenadasTotalMapa :: Coordenadas -> Mapa -> [Coordenadas]
coordenadasTotalMapa (x,y) [] = []
coordenadasTotalMapa (x,y) (a:b) = coordenadasTotal (x,y) a ++ coordenadasTotalMapa (0,y+1) b


-- | Tal como o nome diz, esta função analisa se existe alguma Peca à direita de determinada Coordenada

verificaObstaculodireita :: Coordenadas -> [Coordenadas] -> Bool
verificaObstaculodireita (x,y) [] = False
verificaObstaculodireita (x,y) ((a,b):c) | y == b && x == a-1 = True
                                         | otherwise = verificaObstaculodireita (x,y) c

-- | A função 'move' faz com que o Jogador se mova para a esquerda ou direita, contudo funciona para qualquer situação                                         

move :: Jogador -> Movimento -> Jogador
move (Jogador (a,b) d f) m | m == AndarEsquerda = (Jogador (a-1,b) Oeste f)
                           | m == AndarDireita = (Jogador (a+1,b) Este f)

-- | A função 'moveparaDireita' funciona simplesmente como uma auxiliar para a 'moveparaDireita1'

moveparaDireita :: Bool -> Jogador -> Movimento -> Jogador
moveparaDireita t (Jogador (a,b) d f) m | t == True = (Jogador (a,b) Este f)
                                        | otherwise = move (Jogador (a,b) d f) m

-- | Esta função faz com que o Jogador se mova para a Direita apenas quando possivel  

moveparaDireita1 :: Jogador -> Mapa -> Movimento -> Jogador
moveparaDireita1 j@(Jogador (a,b) d f) l@((x:xs):y) m | a == ultimaAbcissa (maisDireita (coordenadasTotalMapa (0,0) l)) = (Jogador (a,b) Este f)
                                                      | otherwise = moveparaDireita (verificaObstaculodireita (a,b) (coordenadasTotalMapa (0,0) l)) j m


-- | Tal como o nome diz, esta função analisa de existe alguma Peca à esquerda de determinada Coordenada

verificaObstaculoesquerda :: Coordenadas -> [Coordenadas] -> Bool
verificaObstaculoesquerda (x,y) [] = False
verificaObstaculoesquerda (x,y) ((a,b):c) | y == b && x == a+1 = True
                                          | otherwise = verificaObstaculoesquerda (x,y) c

-- | A exemplo da 'moveparaDireita' esta função é apenas uma auxiliar para a 'moveparaEsquerda1'

moveparaEsquerda :: Bool -> Jogador -> Movimento -> Jogador
moveparaEsquerda t (Jogador (a,b) d f) m | t == True = (Jogador (a,b) Oeste f)
                                         | otherwise = move (Jogador (a,b) d f) m

{- | Esta função faz com que o Jogador se mova para a esquerda apenas quando possivel,
ou seja, quando não tem nada a bloquea-lo ou não está num ponto de abcissa igual a zero 
-}

moveparaEsquerda1 :: Jogador -> Mapa -> Movimento -> Jogador
moveparaEsquerda1 j@(Jogador (a,b) d f) l@((x:xs):y) m | a == 0 = (Jogador (a,b) Oeste f)
                                                       | otherwise = moveparaEsquerda (verificaObstaculoesquerda(a,b) (coordenadasTotalMapa (0,0) l)) j m

-- | Esta função no fundo junta a 'moveparaEsquerda1' e 'moveparaDireita1' com o objetivo de mover o Jogador apenas quando possivel

moveEsquerdaDireita :: Jogador -> Mapa -> Movimento -> Jogador
moveEsquerdaDireita j l m | m == AndarDireita = colocaNoChao (moveparaDireita1 j l m) l
                          | m == AndarEsquerda = colocaNoChao (moveparaEsquerda1 j l m) l

-- | Esta função garante que o Jogador tem sempre uma base, por outras palavras, nunca está a flutuar
                
colocaNoChao :: Jogador -> Mapa -> Jogador
colocaNoChao j@(Jogador (a,b) d f) l@((x:xs):y) = gravidade (terraAlta (retirasuperior (a,b) (dameColuna (a,b) (coordenadasTotalMapa (0,0) l)))) (analisaBase (a,b) (coordenadasTotalMapa (0,0) l)) j 

{- | Esta função verifica se determinada Coordenada pertence ou não a uma lista de Coordenadas, caso pertença retorna False:

== Exemplos de utilização:
>>> verificaExistencia (0,0) [(9,5),(3,2),(5,2)]
True
>>> verificaExistencia (0,0) [(9,5),(3,2),(5,2)]
False
-}

verificaExistencia :: Coordenadas -> [Coordenadas] -> Bool
verificaExistencia (x,y) [] = True
verificaExistencia (x,y) ((a,b):c) | x == a && y == b = False
                                   | otherwise = verificaExistencia (x,y) c 

-- | 'analisaMuro' verifica se existe algum elemento por cima daquilo que queremos trepar, caso exista um muro a função retorna True                               

analisaMuro :: Coordenadas -> [Coordenadas] -> Bool
analisaMuro (x,y) [] = False
analisaMuro (x,y) ((a,b):c) | x == a && y == b+1 = True
                            | otherwise = analisaMuro (x,y) c

-- | Esta função dá as Coordenadas do elemento à direita do Jogador

coordenadaDireitaBase :: Jogador -> Coordenadas
coordenadaDireitaBase (Jogador (a,b) d f) = (a+1,b)

-- | Esta função dá as Coordenadas do elemento à esquerda do Jogador

coordenadaEsquerdaBase :: Jogador -> Coordenadas
coordenadaEsquerdaBase (Jogador (a,b) d f) = (a-1,b)

-- | Funciona como uma simples auxiliar para a 'trepaDireita1'

trepaDireita :: Bool -> Jogador -> Bool -> Movimento -> Jogador  
trepaDireita f (Jogador (a,b) m t) r s | f == True = (Jogador (a,b) m t)
                                     | f == False && r == False = (Jogador (a,b) m t)
                                     | otherwise = (Jogador (a+1,b-1) m t)

-- | Funciona como uma simples auxiliar para a 'trepaEsquerda1'

trepaEsquerda :: Bool -> Jogador -> Bool -> Movimento-> Jogador
trepaEsquerda f (Jogador (a,b) m t) r s | f == True = (Jogador (a,b) m t)
                                        | f == False && r == False = (Jogador (a,b) m t)
                                        | otherwise = (Jogador (a-1,b-1) m t)

{- | Esta função permite ao Jogador Trepar, mas apenas em casos que são possiveis, ou seja,
quando existe algo para trepar e não existe nada em cima desse algo
-}

trepaDireita1 :: Jogador -> Mapa -> Movimento -> Jogador
trepaDireita1 j@(Jogador (a,b) d f) l m = trepaDireita (analisaMuro (coordenadaDireitaBase j) (coordenadasTotalMapa (0,0) l)) j (verificaObstaculodireita (a,b) (coordenadasTotalMapa (0,0) l))  m

-- | Faz o mesmo que a função 'trepaDireita1', mas aplicando o movimento Trepar ao lado esquerdo

trepaEsquerda1 :: Jogador -> Mapa -> Movimento -> Jogador
trepaEsquerda1 j@(Jogador (a,b) d f) l m = trepaEsquerda (analisaMuro (coordenadaEsquerdaBase j) (coordenadasTotalMapa (0,0) l)) j (verificaObstaculoesquerda (a,b) (coordenadasTotalMapa (0,0) l)) m

-- | Esta junção junta as duas anteriores para aplicar o movimento a ambos os lados apenas quando possivel

trepaEsquerdaDireita :: Jogador -> Mapa -> Movimento -> Jogador 
trepaEsquerdaDireita j@(Jogador (a,b) d f) l@((x:xs):y) m | m == Trepar && d == Oeste = colocaNoChao  (trepaEsquerda1 j l m) l 
                                                          | m == Trepar && d == Este = colocaNoChao (trepaDireita1 j l m) l
                                                          | otherwise = j

-- | Esta função retorna as Coordenadas de todas as Caixas numa lista de Peca

dameCoordenadasCaixa :: Coordenadas -> [Peca] -> [Coordenadas]
dameCoordenadasCaixa (x,y) [] = []
dameCoordenadasCaixa (x,y) (z:zs) | z == Caixa = (x,y) : dameCoordenadasCaixa (x+1,y) zs
                                  | otherwise = dameCoordenadasCaixa (x+1,y) zs

{- | Faz o mesmo que a função anterior, contudo é capaz de analisar um Mapa 

== Exemplos de utilização:
>>> dameCoordenadasCaixaMapa (0,0) [[Caixa,Bloco,Vazio],[Porta,Caixa,Caixa]]
[(0,0),(1,1),(2,1)]
>>> dameCoordenadasCaixaMapa (0,0) [[Caixa,Bloco,Vazio,Caixa],[Porta,Caixa,Caixa,Bloco]]
[(0,0),(3,0),(1,1),(2,1)]
-}                                  

dameCoordenadasCaixaMapa :: Coordenadas -> Mapa -> [Coordenadas]
dameCoordenadasCaixaMapa (x,y) [] = []
dameCoordenadasCaixaMapa (x,y) (z:zs) = dameCoordenadasCaixa (x,y) z ++ dameCoordenadasCaixaMapa (0,y+1) zs

-- | Verifica a existencia de uma caixa imediatamente à direita do Jogador, caso exista Caixa a função retorna True 

analisaCaixaDireita :: Coordenadas -> [Coordenadas] -> Bool
analisaCaixaDireita (x,y) [] = False
analisaCaixaDireita (x,y) ((a,b):c) | x == a+1 && y == b = True
                                    | otherwise = analisaCaixaDireita (x,y) c

-- | Verifica a existencia de uma caixa imediatamente à esquerda do Jogador, caso exista Caixa a função retorna True

analisaCaixaEsquerda :: Coordenadas -> [Coordenadas] -> Bool
analisaCaixaEsquerda (x,y) [] = False
analisaCaixaEsquerda (x,y) ((a,b):c) | x == a-1 && y == b = True
                                     | otherwise = analisaCaixaEsquerda (x,y) c

-- | Esta função verifica se exite alguma Peca imediatamente a cima de determinada Coordenada

verificaTeto :: Coordenadas -> [Coordenadas] -> Bool
verificaTeto (x,y) [] = False
verificaTeto (x,y) ((a,b):c) | x == a && y == b+1 = True
                             | otherwise = verificaTeto (x,y) c

-- | Esta função auxiliar permite saber as coordenadas de cada Peca e substitui-la, mas apenas numa lista de Peca

auxtrocaPeca1 :: Coordenadas -> (Peca, Coordenadas) -> [Peca] -> [Peca]
auxtrocaPeca1 (c,d) (u,(x,y)) [] = []
auxtrocaPeca1 (c,d) (u,(x,y)) (a:b) | x == c && d == y = u:b
                                    | otherwise = a : auxtrocaPeca1 (c+1,d) (u,(x,y)) b

-- | Esta função faz o mesmo que a anterior, contudo consegue analisar um Mapa

auxtrocaPeca2 :: Coordenadas -> (Peca, Coordenadas) -> Mapa -> Mapa
auxtrocaPeca2 (c,d) (u,(x,y)) [] = []
auxtrocaPeca2 (c,d) (u,(x,y)) ((a:b):h) = auxtrocaPeca1 (c,d) (u,(x,y)) (a:b) : auxtrocaPeca2 (c,d+1) (u,(x,y)) h 

-- | Com o auxilio das funções auxiliares é possivel trocar uma Peca (Bloco\Porta\Caixa) por um Vazio no sitio correto 

trocaPeca :: (Peca, Coordenadas) -> Mapa -> Mapa
trocaPeca (u,(x,y)) [] = []
trocaPeca (u,(x,y)) ((a:b):h) = auxtrocaPeca2 (0,0) (u,(x,y)) ((a:b):h)

-- | Esta função faz o mesmo que a anterior, muda o facto de conseguir substituir uma lista de Peca

trocaTodasPeca :: [(Peca, Coordenadas)] -> Mapa -> Mapa
trocaTodasPeca [] ((a:b):h) = ((a:b):h)
trocaTodasPeca (x:xs) l = trocaTodasPeca xs (trocaPeca x l)

-- | Esta função serve apenas como uma auxiliar para a função 'levantaCaixaDireita1'

levantaCaixaDireita :: Bool -> Jogador -> Movimento -> Bool -> Jogador
levantaCaixaDireita r j@(Jogador (a,b) d h) m l | h == False && m == InterageCaixa && r == True && l == False = (Jogador (a,b) d True)   
                                                | otherwise = j

-- | Esta função serve apenas como uma auxiliar para a função 'levantaCaixaEsquerda1'

levantaCaixaEsquerda :: Bool -> Jogador -> Movimento -> Bool -> Jogador
levantaCaixaEsquerda r j@(Jogador (a,b) d h) m l | h == False && m == InterageCaixa && r == True && l == False = (Jogador (a,b) d True)   
                                                 | otherwise = j

-- | Esta função faz com que o Jogador levante uma Caixa imediatamente à sua direita apenas quando possivel                                                  

levantaCaixaDireita1 :: Jogador -> Mapa -> Movimento -> Jogador
levantaCaixaDireita1 j@(Jogador (a,b) d f) l m = levantaCaixaDireita (analisaCaixaDireita (a,b) (dameCoordenadasCaixaMapa (0,0) l)) j m (verificaTeto (a,b) (coordenadasTotalMapa (0,0) l))

-- | Esta função faz com que o Jogador levante uma Caixa imediatamente à sua esquerda apenas quando possivel  

levantaCaixaEsquerda1 :: Jogador -> Mapa -> Movimento -> Jogador
levantaCaixaEsquerda1 j@(Jogador (a,b) d f) l m = levantaCaixaEsquerda (analisaCaixaEsquerda (a,b) (dameCoordenadasCaixaMapa (0,0) l)) j m (verificaTeto (a,b) (coordenadasTotalMapa (0,0) l))

-- | Esta função no fundo junta as duas anteriores, com o objetivo de o Jogador só poder levantar uma Caixa em determinadas condições

levantaCaixaEsquerdaDireita :: Jogador -> Mapa -> Movimento -> Jogador
levantaCaixaEsquerdaDireita j@(Jogador (a,b) d f)  l m | m == InterageCaixa && d == Oeste = levantaCaixaDireita1 j l m
                                                       | m == InterageCaixa && d == Este = levantaCaixaEsquerda1 j l m 
                                                       | otherwise = j

-- | A função 'larga' é apenas uma auxiliar para a 'largaDireita' e 'largaEsquerda'

larga :: Bool -> Jogador -> Movimento -> Jogador 
larga r j@(Jogador (a,b) d h) m | h == True && r == False = (Jogador (a,b) d False) 
                                | otherwise = j

-- | Esta função faz com que o Jogador largue uma Caixa à sua direita, mas apenas quando possivel

largaDireita :: Jogador -> Mapa -> Movimento -> Jogador
largaDireita j@(Jogador (a,b) d f) l m = larga (verificaObstaculodireita (a,b) (coordenadasTotalMapa (0,0) l)) j m 

-- | Esta função faz com que o Jogador largue uma Caixa à sua esquerda, mas apenas quando possivel

largaEsquerda :: Jogador -> Mapa -> Movimento -> Jogador
largaEsquerda j@(Jogador (a,b) d f) l m = larga (verificaObstaculoesquerda (a,b) (coordenadasTotalMapa (0,0) l)) j m 

-- | Esta função faz com que o Jogador largue uma Caixa para ambos os lados

largaEsquerdaDireita :: Jogador -> Mapa -> Movimento -> Jogador
largaEsquerdaDireita j@(Jogador (a,b) d f) l m | m == InterageCaixa && d == Este = largaDireita j l m
                                               | m == InterageCaixa && d == Oeste = largaEsquerda j l m 
                                               | otherwise = j

-- | A função 'levantaLargaCaixa' permite ao Jogador levantar e largar uma Caixa quer para um lado quer para o outro

levantaLargaCaixa :: Jogador -> Mapa -> Movimento -> Jogador
levantaLargaCaixa j@(Jogador (a,b) d f) l m | f == True && m == InterageCaixa = largaEsquerdaDireita j l m
                                            | f == False && m == InterageCaixa = levantaCaixaEsquerdaDireita j l m 
                                            | otherwise = (Jogador (a,b) d f)

-- | A função 'ff', no fundo diz-me se o Jogador está a carregar uma Caixa ou não

ff :: Jogador -> Bool
ff (Jogador (a,b) c d) = d

{- | Esta função indica se o Jogador largou ou pegou numa caixa, caso o seu valor 
de verdade mude de Verdadeiro para Falso significa que a largou, caso contrário pegou nela
-}

pegaOuLarga :: Bool -> Bool -> String
pegaOuLarga x y | x == True && y == False = "largou"
                | x == False && y == True = "pegou"
                | otherwise = "" 

-- | Esta função foi criada com o objetivo de a Caixa ficar sempre em cima do Jogador

colocaemcima :: Coordenadas -> Coordenadas
colocaemcima (x,y) = (x,y-1)                

-- | Caso o Jogador tenha largado ou pegado numa Caixa, esta função faz as devidas alterações no Mapa

alteraMapa :: String -> Jogador -> Mapa -> Mapa 
alteraMapa s (Jogador (a,b) d f) l | s == "largou" && d == Oeste = trocaTodasPeca [(Vazio,(a,b-1)),(Caixa,(colocaemcima(terraAlta (retirasuperior (a,b) (dameColuna (a-1,b) (coordenadasTotalMapa (0,0) l))))))] l
                                   | s == "largou" && d == Este = trocaTodasPeca [(Vazio,(a,b-1)),(Caixa,(colocaemcima(terraAlta (retirasuperior (a,b) (dameColuna (a+1,b) (coordenadasTotalMapa (0,0) l))))))] l
                                   | s == "pegou" && d == Oeste = trocaTodasPeca [(Vazio,(a-1,b)),(Caixa,(a,b-1))] l
                                   | s == "pegou" && d == Este = trocaTodasPeca [(Vazio,(a+1,b)),(Caixa,(a,b-1))] l 
                                   | s == "" = l

-- | A função 'alteraMapa1' é parecida à anterior, contudo recebe um Jogo e um Movimento como argumentos

alteraMapa1 :: Jogo -> Movimento -> Jogo
alteraMapa1 (Jogo l (Jogador (a,b) d p)) m | m == InterageCaixa = (Jogo (alteraMapa (pegaOuLarga p (ff (levantaLargaCaixa (Jogador (a,b) d p) l m))) (Jogador (a,b) d p) l) (levantaLargaCaixa (Jogador (a,b) d p) l m))
                                           | otherwise = (Jogo l (Jogador (a,b) d p))

-- | Esta função permite ao Jogador mover-se com uma Caixa, contudo se houver obstaculos à passagem do Jogador ou da Caixa não ocorre movimento

movercomCaixa :: Jogo -> Movimento -> Jogo
movercomCaixa (Jogo l (Jogador (a,b) d f)) m | f == True && m == AndarDireita && (verificaObstaculodireita (a,b) (coordenadasTotalMapa (0,0) l)) == True = (Jogo (trocaTodasPeca [(Caixa,(a,b-1))] l) (Jogador (a,b) Este f))
                                             | f == True && m == AndarEsquerda && (verificaObstaculoesquerda (a,b) (coordenadasTotalMapa (0,0) l)) == True = (Jogo (trocaTodasPeca [(Caixa,(a,b-1))] l) (Jogador (a,b) Oeste f))
                                             | f == True && m == AndarEsquerda && (verificaObstaculoesquerda (a,b-1) (coordenadasTotalMapa (0,0) l)) == True = (Jogo (trocaTodasPeca [(Caixa,(a,b-1))] l) (Jogador (a,b) Oeste f))
                                             | f == True && m == AndarDireita && (verificaObstaculodireita (a,b-1) (coordenadasTotalMapa (0,0) l)) == True = (Jogo (trocaTodasPeca [(Caixa,(a,b-1))] l) (Jogador (a,b) Este f))
                                             | f == True && m == AndarDireita = (Jogo (trocaTodasPeca [(Vazio,(a,b-1)),(Caixa,(colocaNoChao2 (a+1,b) l))] l) (moveEsquerdaDireita (Jogador (a,b) d f) l m))
                                             | f == True && m == AndarEsquerda = (Jogo (trocaTodasPeca [(Vazio,(a,b-1)),(Caixa,(colocaNoChao2 (a-1,b) l))] l) (moveEsquerdaDireita (Jogador (a,b) d f) l m))
                                             | otherwise = (Jogo l (moveEsquerdaDireita (Jogador (a,b) d f) l m))


{- | Esta função foi criada para servir o 'colocaNoChao2', ou seja, se a Caixa está em cima 
do Jogador, isso significa que está duas Coordenadas acima da base onde o Jogador está apoiado
-} 

meteDoisemCima :: Coordenadas -> Coordenadas
meteDoisemCima (x,y) = (x,y-2)

-- | Uma função que permite quedas do Jogador sem que este deixe a Caixa para trás

colocaNoChao2 :: Coordenadas -> Mapa -> Coordenadas
colocaNoChao2 (x,y) l = meteDoisemCima (terraAlta (retirasuperior (x,y) (dameColuna (x,y) (coordenadasTotalMapa (0,0) l))))

-- | Esta função permite que um Jogador a carregar uma Caixa trepe Pecas 

treparcomCaixa :: Jogo -> Movimento -> Jogo
treparcomCaixa (Jogo l (Jogador (a,b) d f)) m | f == True && m == Trepar && d == Este && (verificaObstaculodireita (a,b) (coordenadasTotalMapa (0,0) l)) == True && (analisaMuro (a+1,b) (coordenadasTotalMapa (0,0) l)) == False && (analisaMuro (a+1,b-1) (coordenadasTotalMapa (0,0) l)) == False = (Jogo (trocaTodasPeca [(Vazio,(a,b-1)),(Caixa,(a+1,b-2))] l) (trepaEsquerdaDireita (Jogador (a,b) d f) l m))
                                              | f == True && m == Trepar && d == Oeste && (verificaObstaculoesquerda (a,b) (coordenadasTotalMapa (0,0) l)) == True && (analisaMuro (a-1,b) (coordenadasTotalMapa (0,0) l)) == False && (analisaMuro (a-1,b-1) (coordenadasTotalMapa (0,0) l)) == False = (Jogo (trocaTodasPeca [(Vazio,(a,b-1)),(Caixa,(a-1,b-2))] l) (trepaEsquerdaDireita (Jogador (a,b) d f) l m))
                                              | a == 1 = (Jogo (trocaTodasPeca [(Caixa,(a,b-1))] l) (Jogador (a,b) d f)) 
                                              | otherwise = (Jogo (trocaTodasPeca [(Caixa,(a,b-1))] l) (Jogador (a,b) d f))