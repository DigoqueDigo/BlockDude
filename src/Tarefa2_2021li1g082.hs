{- |
Module      : Tarefa2_2021li1g082
Description : Construção/Desconstrução do mapa
Copyright   : Diogo Marques <a100897@alunos.uminho.pt>;
            : Luís de Castro Rodrigues Caetano <a100893@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g082 where

import LI12122

import Data.List (nub)

-- | Esta função é capaz de devolver um 'Mapa' ao receber uma lista de (Peca,Coordenadas)

constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa l = funcaoFinal1 l

-- | Esta função faz basicamente o oposto da 'constroiMapa'

desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa l = funcaoFinal2 l


-- | PRIMEIRA FUNÇÃO (constroiMapa)

-- | Função que apenas dá as coordenadas das Pecas

transformaLista :: [(Peca, Coordenadas)] -> [Coordenadas]
transformaLista [] = []
transformaLista ((u,(a,b)):z) = (a,b) : transformaLista z

-- | Esta função calcula a coordenada do elemento que está mais à direita no mapa

maisDireita :: [Coordenadas] -> Coordenadas
maisDireita [] = (0,0)
maisDireita [(x,y)] = (x,y)
maisDireita ((x,y):(a,b):z) | x >= a = maisDireita ((x,y):z)
                            | otherwise = maisDireita ((a,b):z)

-- | Esta função calcula a coordenada do elemento que está mais em baixo no mapa
 
maisAbaixo :: [Coordenadas] -> Coordenadas
maisAbaixo [] = (0,0)
maisAbaixo [(x,y)] = (x,y)
maisAbaixo ((x,y):(a,b):z) | y >= b = maisAbaixo ((x,y):z)  
                           | otherwise = maisAbaixo ((a,b):z)
 
-- | A partir das ultimas duas funções é possivel calcular o último ponto do mapa, ou seja, o elemento que está no canto inferior direito 

ultimoPontoMapa :: Coordenadas -> Coordenadas -> Coordenadas
ultimoPontoMapa (x,y) (a,b) = (x,b) 

{- | Com o último ponto do mapa, criamos um mapa onde apenas existem Vazios: 

Exemplo de utilização:
>>> dameMapaVazios (2,1)
[[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio]]
-}

dameMapaVazios :: Coordenadas -> Mapa
dameMapaVazios (x,y)  = replicate (y+1) (replicate (x+1) Vazio)

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

-- | Uma função que no fundo compila todas as anteriores de modo a obter a função 'constroiMapa'

funcaoFinal1 :: [(Peca, Coordenadas)] -> Mapa
funcaoFinal1 [] = []
funcaoFinal1 l = trocaTodasPeca l (dameMapaVazios (ultimoPontoMapa (maisDireita (transformaLista l)) (maisAbaixo (transformaLista l))))


-- | SEGUNDA FUNÇÃO (desconstroiMapa)

{- | Esta função é capaz de devolver uma lista sem quaisquer Vazios:

Exemplo de utilização:
>>> ignoraVazios [Bloco,Bloco,Vazio,Caixa,Porta]
[Bloco,Bloco,Caixa,Porta]
-}

ignoraVazios :: [Peca] -> [Peca]
ignoraVazios [] = []
ignoraVazios (x:xs) | x == Vazio = ignoraVazios xs
                    | otherwise = x : ignoraVazios xs

-- | Esta função faz o mesmo que a anterior (ignoraVazios), contudo é capaz de analisar um Mapa

ignoraVaziosMapa :: Mapa -> Mapa
ignoraVaziosMapa [] = []
ignoraVaziosMapa ((x:xs):y) = ignoraVazios (x:xs) : ignoraVaziosMapa y

-- | A função 'dameLista3' foi criada com a intenção de transformar um Mapa em apenas uma lista  

dameLista3 :: Mapa -> [Peca]
dameLista3 [] = [] 
dameLista3 ([]:xs) = dameLista3 xs
dameLista3 (x:xs) = x ++ dameLista3 xs

-- | Esta função consegue calcular as coordenadas de cada Peca numa lista de Peca  

auxcontaCoordenadas1 :: Coordenadas -> Peca -> [Peca] -> [Coordenadas]
auxcontaCoordenadas1 (x,y) a [] = []
auxcontaCoordenadas1 (x,y) a (z:zs) | a == z = (x,y) : auxcontaCoordenadas1 (x+1,y) a zs
                                    | otherwise = auxcontaCoordenadas1 (x+1,y) a zs

-- | A função 'auxcontaCoordenadas2' faz o mesmo que a 'auxcontaCoordenadas1', contudo analisa um Mapa                                

auxcontaCoordenadas2 :: Coordenadas -> Peca -> Mapa -> [Coordenadas]
auxcontaCoordenadas2 (x,y) a [] = []
auxcontaCoordenadas2 (x,y) a ((c:b):h) = (auxcontaCoordenadas1 (x,y) a (c:b)) ++ auxcontaCoordenadas2 (x,y+1) a h

-- | A função 'contaCoordenadas' auxilia-se da função 'auxcontaCoordenadas2' para saber as coordenadas de uma Peca num Mapa

contaCoordenadas :: Peca -> Mapa -> [Coordenadas]
contaCoordenadas p m = auxcontaCoordenadas2 (0,0) p m

-- | Dada uma lista de Peca, esta função retorna uma lista com todas as Coordenadas dessas mesmas Pecas no Mapa

contaTodasCoordenadas :: [Peca] -> Mapa -> [Coordenadas]
contaTodasCoordenadas (x:xs) [] = []
contaTodasCoordenadas [] y = []
contaTodasCoordenadas (x:xs) y = contaCoordenadas x y ++ contaTodasCoordenadas xs y

{- | A função 'auxcontaRepete1' devolve o número de vezes que uma determinada Peca aparece numa lista de Peca:

== Exemplos de utilização:
>>> auxcontaRepete1 Bloco [Bloco,Porta,Bloco,Vazio,Bloco]
3
-}

auxcontaRepete1 :: Peca -> [Peca] -> Int
auxcontaRepete1 p [] = 0
auxcontaRepete1 p (x:xs) | p == x = 1 + auxcontaRepete1 p xs
                         | otherwise = auxcontaRepete1 p xs

-- | A função 'auxcontaRepete2' já é capaz de analisar quantas vezes determinada Peca aparece num Mapa

auxcontaRepete2 :: Peca -> Mapa -> Int
auxcontaRepete2 p [] = 0
auxcontaRepete2 p ((x:xs):y) = auxcontaRepete1 p (x:xs) + auxcontaRepete2 p y

{- | Esta função no fundo serve-se da 'auxcontaRepete1' e da 'auxcontaRepete2' para cobrir casos mais abrangentes:

== Exemplos de utilização:
>>> contaRepete [Bloco,Porta] [[Bloco,Caixa,Bloco],[Porta,Bloco,Bloco]]
[4,1]
-}

contaRepete :: [Peca] -> Mapa -> [Int]
contaRepete [] [] = []
contaRepete (x:xs) [] = []
contaRepete [] l = []
contaRepete (x:xs) l = auxcontaRepete2 x l : contaRepete xs l

{- | A função 'multiplica', multiplica cada Peca por um determinado número (número de vezes que aparece repetida no Mapa):

== Exemplos de utilização:
>>> multiplica [4,1] [Bloco,Porta]
[Bloco,Bloco,Bloco,Bloco,Porta]
-}

multiplica :: [Int] -> [Peca] -> [Peca]
multiplica [] [] = []
multiplica (x:xs) [] = []
multiplica [] (y:ys) = []
multiplica (x:xs) (y:ys) = (replicate x y) ++ multiplica xs ys

-- | Uma função que no fundo compila todas as anteriores de modo a obter a função 'desconstroiMapa', para isso serve-se também de funções pré-definidas, como a 'nub' e a 'zip'

funcaoFinal2 ::  Mapa -> [(Peca, Coordenadas)]
funcaoFinal2 l = zip (multiplica (contaRepete (nub (dameLista3 (ignoraVaziosMapa l))) l) (nub (dameLista3 (ignoraVaziosMapa l)))) (nub (contaTodasCoordenadas (dameLista3 (ignoraVaziosMapa l)) l))