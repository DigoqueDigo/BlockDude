{- |
Module      : Tarefa1_2021li1g082
Description : Validação de um potencial mapa
Copyright   : Diogo Marques <a100897@alunos.uminho.pt>;
            : Luís de Castro Rodrigues Caetano <a100893@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g082 where

import LI12122

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa l@((u,(x1,y1)):z) = not (verificaCoordenadasrepetem l) && verificaPorta l && verificaApoioCaixaFinal ((u,(x1,y1)):z) && verificaVazio l && analisaBase l



-- | PRIMEIRO TÓPICO - Feito

{- | 'verificaCoordenadaExiste' : função recursiva que verifica se determinada coordenada
pertence a uma lista de coordenadas, caso pertença retorna True 
-} 

verificaCoordenadaExiste :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
verificaCoordenadaExiste (x,y) [] = False
verificaCoordenadaExiste (x,y) ((u,(a,b)):z) | x == a && y == b = True
                                             | otherwise = verificaCoordenadaExiste (x,y) z

{- | 'verificaCoordenadasrepetem' : função recursiva que verifica se existem coordenadas repetidas 
numa dada lista de coordenadas, para tal chama a função 'verificaCoordenadaExiste':

== Exemplos de utilização:
>>> verificaCoordenadasrepetem [(Bloco,(0,1)),(Bloco,(2,0)),(Bloco, (0,1))]
True
>>> verificaCoordenadasrepetem [(Bloco,(0,1)),(Bloco,(2,0))]
False
-}

verificaCoordenadasrepetem :: [(Peca, Coordenadas)] -> Bool
verificaCoordenadasrepetem [] = False
verificaCoordenadasrepetem ((u,(x1,y1)):z) = verificaCoordenadaExiste (x1,y1) z || verificaCoordenadasrepetem z


-- | SEGUNDO TÓPICO - Feito

{- | 'verificanaotemPorta' : função que analisa se uma lista não possui uma Porta,
ou seja, uma lista sem a Peca Porta retorna True 
-}

verificanaotemPorta :: [(Peca, Coordenadas)] -> Bool
verificanaotemPorta [] = True
verificanaotemPorta ((v,(a,b)):z) | Porta /= v = verificanaotemPorta z
                                  | otherwise= False


{- | 'verificaPorta' : analisa a existencia de apenas uma Porta, caso se encontre uma Porta, 
é chamada a função verificanaotemPorta para garantir a existencia de apenas uma Porta:

== Exemplos de utilização:
>>> verificaPorta [(Porta,(1,4)),(Bloco,(2,3)),(Bloco,(3,4))]
True
>>> verificaPorta [(Porta,(1,4)),(Bloco,(2,3)),(Porta,(3,4))]
False
-}

verificaPorta :: [(Peca, Coordenadas)] -> Bool
verificaPorta [] = False
verificaPorta ((v,(a,b)):z) | Porta == v = verificanaotemPorta z
                            | otherwise = verificaPorta z 


-- | TERCEIRO TÓPICO - Feito

{- | 'dameCaixa' : função que dada uma lista de peças com as respetivas coordenadas calcula 
apenas as coordenadas das caixas, ignorando tudo aquilo que não é Caixa
-}

dameCaixa :: [(Peca, Coordenadas)] -> [Coordenadas]
dameCaixa [] = []
dameCaixa ((u,(a,b)):z) | u == Caixa = (a,b) : dameCaixa z
                        | otherwise = dameCaixa z

{- | 'verificaApoio' : averigua se uma coordenada está apoiada por uma Caixa ou Bloco,
ou seja, procura coordenadas com a mesma abcissa e com a ordenada superior, daí que y == b-1 
-}

verificaApoio :: Coordenadas -> [(Peca, Coordenadas)]  -> Bool
verificaApoio (x,y) [] = False
verificaApoio (x,y) ((u,(a,b)):z) | x == a && y == b-1 = u == Bloco || u == Caixa
                                  | otherwise = verificaApoio (x,y) z

{- | 'verificaApoioCaixa' : faz o mesmo que a função anterior, contudo é mais potente, 
dado que consegue fazer a mesma analise a uma lista de coordenadas 
-}

verificaApoioCaixa :: [Coordenadas] -> [(Peca, Coordenadas)] -> Bool
verificaApoioCaixa [] ((u,(a,b)):z) = True
verificaApoioCaixa ((x,y):w) ((u,(a,b)):z) = verificaApoio (x,y) ((u,(a,b)):z) && verificaApoioCaixa w z

{- | 'verificaApoioCaixaFinal' : uma função que no fundo junta as três anteriores:

== Exemplos de utilização:
>>> verificaApoioCaixaFinal [(Bloco,(0,1)),(Caixa, (0,0)),(Bloco, (5,2))]
True
>>> verificaApoioCaixaFinal [(Bloco,(0,2)),(Caixa, (0,0)),(Bloco, (5,2))]
False
-}

verificaApoioCaixaFinal :: [(Peca, Coordenadas)] -> Bool
verificaApoioCaixaFinal [] = False
verificaApoioCaixaFinal l | dameCaixa l == [] = True
                          | otherwise = verificaApoioCaixa (dameCaixa l) l


-- | QUARTO TÓPICO - Feito

transformaLista :: [(Peca, Coordenadas)] -> [Coordenadas]
transformaLista [] = []
transformaLista ((u,(a,b)):z) = (a,b) : transformaLista z

-- | Função que calcula a coordenada mais à direita

maisDireita :: [Coordenadas] -> Coordenadas
maisDireita [] = (0,0)
maisDireita [(x,y)] = (x,y)
maisDireita ((x,y):(a,b):z) | x >= a = maisDireita ((x,y):z)
                            | otherwise = maisDireita ((a,b):z)

-- | Função que calcula a coordenada mais em baixo
 
maisAbaixo :: [Coordenadas] -> Coordenadas
maisAbaixo [] = (0,0)
maisAbaixo [(x,y)] = (x,y)
maisAbaixo ((x,y):(a,b):z) | y >= b = maisAbaixo ((x,y):z)  
                           | otherwise = maisAbaixo ((a,b):z)

{- | A partir dos dois pontos dados pelas funções anteriores, 
esta função calcula o ponto do canto inferior direito, sendo que o mapa é retângulo 
-}   

ultimoPontoMapa :: Coordenadas -> Coordenadas -> Coordenadas
ultimoPontoMapa (x,y) (a,b) = (x,b) 

{- | Apenas com a ultima coordenada de uma mapa (essa coordenada pode ser um espaço vazio),
esta função dá-me todas as coordenadas (há coordenadas que se volta a repetir na lista)
-} 

totalCoordenadas :: Coordenadas -> [Coordenadas]
totalCoordenadas (0,0) = [(0,0)]
totalCoordenadas (x,y) | y == 0 = totalCoordenadas (x-1,0) ++ [(x,0)] 
                       | x == 0 = totalCoordenadas (0,y-1) ++ [(0,y)]
                       | otherwise = (totalCoordenadas (x,y-1)) ++ (totalCoordenadas (x-1,y)) ++ [(x,y)]

-- | Verifica se uma coordenada pertence a uma lista, se não pertencer dá True

verificaNaoExiste :: Coordenadas -> [Coordenadas] -> Bool
verificaNaoExiste (x,y) [] = True
verificaNaoExiste (x,y) ((a,b):z) | x == a && y == b = False
                                  | otherwise = verificaNaoExiste (x,y) z

-- | Faz o mesmo que a 'verificaNaoExiste', mas consegue analisar uma lista

verificaNaoExistedeLista :: [Coordenadas] -> [Coordenadas] -> Bool
verificaNaoExistedeLista ((x,y):z) [] = True
verificaNaoExistedeLista [] l = False
verificaNaoExistedeLista ((x,y):z) l = verificaNaoExiste (x,y) l || verificaNaoExistedeLista z l

{- | Função que analisa se um mapa tem espaços vazios ou não, caso tenha dá True:

== Exemplos de utilização:
>>> verificaVazio [(Bloco, (0,0)),(Bloco, (1,0)),(Bloco, (0,1)),(Bloco, (1,1))]
False
>>> verificaVazio [(Bloco, (0,0)),(Bloco, (1,0)),(Bloco, (0,1))]
True
-}

verificaVazio :: [(Peca, Coordenadas)] -> Bool
verificaVazio [] = True
verificaVazio l@((u,(x1,y1)):z) = verificaNaoExistedeLista (totalCoordenadas (ultimoPontoMapa (maisDireita (transformaLista l))  (maisAbaixo (transformaLista l)))) (transformaLista l)


-- | QUINTO TÓPICO - Feito

-- | Esta função dá uma lista com as coordenadas de todos os Blocos 

dameBloco :: [(Peca, Coordenadas)] -> [Coordenadas]
dameBloco [] = []
dameBloco ((u,(a,b)):z) | u == Bloco = (a,b) : dameBloco z
                        | otherwise = dameBloco z

{- | A partir de uma lista de coordenadas, esta função dá a ultima coordenada, 
ou seja, a coordenada com a maior abcissa e ordenada 
-}

ultimoBloco :: [Coordenadas] -> Coordenadas
ultimoBloco [(x,y)] = (x,y)
ultimoBloco ((x,y):(a,b):z) | x > a = ultimoBloco ((x,y):z)
                            | x == a && y >= b = ultimoBloco ((x,y):z)
                            | otherwise = ultimoBloco ((a,b):z)

{- | A partir de uma lista de coordenadas esta função dá a ultima coordenada, 
ou seja, a coordenada com menor abcissa e maior ordenada 
-}

primeiroBloco :: [Coordenadas] -> Coordenadas
primeiroBloco [(x,y)] = (x,y)
primeiroBloco ((x,y):(a,b):z) | x < a = primeiroBloco ((x,y):z)
                              | x == a && y >= b = primeiroBloco ((x,y):z)
                              | otherwise = primeiroBloco ((a,b):z)  

-- | Esta função retorna as Coordenadas da Peca mais distante do ponto (0,0)

ultimodoMapa :: [(Peca, Coordenadas)] -> Coordenadas
ultimodoMapa [(p,(x,y))] = (x,y)
ultimodoMapa ((p,(x,y)):(u,(a,b)):c) | x > a = ultimodoMapa ((p,(x,y)):c)
                                     | x == a && y >= b = ultimodoMapa ((p,(x,y)):c)
                                     |otherwise = ultimodoMapa ((u,(a,b)):c)   

-- | Esta função dá todas as Coordenadas de uma lista de Peca com as respetivas Coordendas

todasCoordenadas :: [(Peca, Coordenadas)] -> [Coordenadas]
todasCoordenadas [] = []
todasCoordenadas ((p,(x,y)):z) = (x,y) : todasCoordenadas z

{- | Esta função analisa a base de uma mapa, ou seja, a partir do primeiroBloco, 
ela analisa se este tem outros blocos à sua direita, caso tenha, vai ver se esse mesmo bloco 
tem mais blocos à sua frente, e assim sucessivamente até chegar ao ultimoBloco 
-} 

verificaVizinhos :: Coordenadas -> [Coordenadas] -> Bool
verificaVizinhos (x,y) [] = False
verificaVizinhos (x,y) l@((a,b):c) | x == (a-1) && y == b = verificaVizinhos (a,b) l
                                   | x == (a-1) && y == (b-1) = verificaVizinhos (a,b) l
                                   | x == (a-1) && y == (b+1) = verificaVizinhos (a,b) l
                                   | (x,y) == ultimoBloco l = True
                                   | otherwise = verificaVizinhos (x,y) c 

{- | Esta função analisa se a base de uma mapa existe, contudo 
essa base não pode ter Blocos debaixo dela, uma vez que nesse caso dá False:

== Exemplos de utilização:
>>> analisaBase  [(Bloco,(0,3)),(Bloco,(2,0)),(Bloco,(1,4)),(Bloco,(2,3)),(Bloco, (0,0)),(Bloco,(3,3))]
True
>>> analisaBase  [(Bloco,(0,3)),(Bloco,(2,0)),(Bloco,(1,4)),(Bloco,(2,3)),(Bloco, (0,0)),(Bloco,(3,0))]
False
-}

analisaBase :: [(Peca, Coordenadas)] -> Bool
analisaBase l@((u,(a,b)):z) | ultimoBloco (dameBloco l) == ultimodoMapa l = verificaVizinhos (primeiroBloco (dameBloco l)) (dameBloco l)
                            | (a,b) == ultimoBloco (dameBloco l) = True
                            |otherwise = False