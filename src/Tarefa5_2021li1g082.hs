{- |
Module      : Tarefa5_2021li1gXXX
Description : Aplicação Gráfica

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.


= Introdução

Foi nesta tarefa que criámos o jogo em si, ou seja, a partir do código desenvolvido nas tarefas anteriores,
mais em específico as tarefas 3 e 4, foi possível desenvolver o aspeto visual do jogo e dar significado às
ações que realizamos no nosso teclado (input/output).

= Objetivo

O nosso principal objetivo foi sempre criar um jogo que pudesse simplesmente ser jogado, nunca tivemos como
intenção criar algo muito rebuscado, pois a probabilidade de meter a pata na poça e chegar ao fim sem que o
jogo desse para jogar seria muito maior. Assim sendo, decidimos que o nosso jogo seria composto por vários
Menus em que apenas seria possível escolher coisas que já estavam definidas no código. 

= Fontes de Informação

Ao longo do trabalho foram surgindo várias dificuldades, contudo ao estudar o código do trabalho de uma aluna
do 4º ano, muitas das dificuldades que tínhamos foram superadas, além disso, ao longo das aulas o professor
foi-nos dando várias dicas que em muitos nos ajudaram. Por fim, a maior ajuda que tivemos foi o Stack Overflow,
pois muitas das vezes em que o nosso código não compilava, pesquisar o erro que aparecia no Stack Overflow foi
sempre a nossa tábua de salvação. 

= Estratégias Adotadas

Quando nos deparámos com esta tarefa pensamos de imediato em definir uma estratégia, essa estratégia passava essencialmente
por dois grandes tópicos, o primeiro era referente à criação de imagens e à sua aplicação, enquanto que o segundo dizia respeito
à criação de vários Menus e à forma como seria possível fazer a informação passar entre o diferentes Menus sem que fosse perdida.
Quanto à criação de imagens, devo referir que não houve quaisquer dificuldades na sua criação, pois os programas que optámos por
utilizar (GIMP e FFmpeg) permitiram-nos editar e converter as imagens sem qualquer dificuldade, já no que diz respeito à implementação
das imagens em código também não houve muitas dificuldades, pois como tínhamos convertido as imagens para bmp apenas era necessário
escrever: fromJust (lookup “nome da imagem” imagens) para que determinada imagem fosse aplicada.
O que nos deu mais dores de cabeça foi realmente a criação de Menus (os Menus são designados de Mundo no código), pois não estávamos
a perceber qual seria a estrutura mais adequada, contudo, mais tarde percebemos que uma estrutura simple seria a ideal, ou seja,
cada Mundo apenas deveriam ser composto por um Menu e uma série de Imagens. Por fim, a maior dificuldade que sentimos foi a passagem
de informação entre os vários Mundos, pois como queríamos dar a opção de escolher um Jogador e um Mapa, essa informação não se podia perder.
Assim sendo, decidimos colocar vários Int em cada Menu, pois assim seria possível guardar a informação em cadeia, uma vez que o Int
escolhido num determinado Menu passaria para o seguinte e assim sucessivamente até chegar ao Menu do Jogo (esta ideia não é originalmente
nossa, mas sim uma sugestão que o professor nos deu).

= Conclusão

Em suma, esta tarefa foi uma das mais trabalhosas que tivemos, mas ao mesmo tempo foi também a mais divertida, pois pudemos
verificar que muito daquilo que tínhamos feito até agora funcionava de forma a que o jogo pudesse ser jogado por qualquer pessoa. 

-}
module Main where

import LI12122
import Mapas
import Tarefa1_2021li1g082
import Tarefa2_2021li1g082
import Tarefa3_2021li1g082
import Tarefa4_2021li1g082
import Tarefa6_2021li1g082
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game
import Data.List (elemIndex)
import Data.Maybe
import qualified Data.Text as Tx

-- | Cada 'Mundo' é uma página do Jogo, ou seja, um Mundo pode ser o Menu Principal do Jogo ou a página onde o Jogo está a decorrer

type Mundo = (Menu, Imagens) 

-- | Um 'Menu' é o contrutor mais importante de um 'Mundo' uma vez que é ele que define exatamente o local do Jogo em que estamos

data Menu = MenuPrincipal OpcaoMenuPrincipal | ModoDN Int | EscolheMapa (Int, OpcaoEscolheMapa) | EscolheJogador (Int,OpcaoEscolheJogador) | ModoJogo (Jogo,Int,Int,Int) | MenuVitoria (Int,OpcaoVitoria) | MenuCreditos | MenuControlos deriving (Eq, Show)

-- | O 'MenuPrincipal' tem três opções de escolha, são elas o 'Jogar' 'Creditos' e 'Controlos' daí a 'OpcaoMenuPrincipal' possui essas três opções

data OpcaoMenuPrincipal = Jogar | Creditos | Controlos deriving (Eq, Show, Read)

-- | O 'OpcaoVitoria' tem duas opções de escolha, são elas o 'Jogar Novamente' e 'Sair' daí a 'OpcaoVitoria' possui essas duas opções

data OpcaoVitoria = JogarNovamente | Sair deriving (Eq, Show ,Read)

-- | Este Int serve apenas para fazer a alteração entre a escolha de 'Mapa'

type OpcaoEscolheMapa = Int

-- | Neste par de Int, o primeiro Int serve para fazer a alteração entre a escolha de 'Jogador' e o segundo Int serve para gaurda a informação do Menu 'EscolheMapa'

type OpcaoEscolheJogador = (Int,Int)

-- | As 'Imagens' são um dos elementos do 'Mundo' e no fundo são o contrutor que permite a aplicação de imagens, sendo que cada 'Foto' está associada a uma 'Picture'

type Imagens = [(Foto, Picture)]

-- | A 'Foto' permite aplicar um imagem no Jogo

data Foto = Caixas | Blocos | Vazios | Portas | Fundo1 | Fundo2 | Fundo3 | Fundo4 | Fundo5 | Fundo6 | Fundo7 | Fundo8 | Fundo9 | Fundo10 | Fundo11 | Fundo12 | Fundo13 | Personagem1Estes | Personagem1Oestes | Personagem2Estes | Personagem2Oestes | Personagem3Estes | Personagem3Oestes | Noite | Noite1 | Noite2 | Noite3 | Noite4 | Noite5 | Noite6 | Noite7 | Noite8 | Noite9 | Noite10 | Teclas | Pfundo1 | Pfundo2 | Pfundo3 deriving Eq

-- | Esta função não tem qualquer interesse para o Jogo, contudo é necessária na função 'main'

reageTempo :: Float -> Mundo -> Mundo
reageTempo n x = x

-- | Função que define a frame rate do Jogo, neste tipo de Jogo a frame rate não é importante.

fr :: Int
fr = 50

-- | Função que atribui uma janela na qual o Jogo irá ser apresentado

mainDisplay :: Display
mainDisplay = InWindow "BlockDude" displayDimension (0,0)

-- | A dimensão da janela do Jogo.

displayDimension :: (Int,Int)
displayDimension = (1280,703)

-- | A função 'reageEvento' faz com que a partir de determinado EventKey possamos obter um novo Mundo.

reageEvento :: Event -> Mundo -> Mundo
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (MenuPrincipal Jogar, imagens) = (MenuPrincipal Jogar, imagens)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (MenuPrincipal Jogar, imagens) = (MenuPrincipal Creditos, imagens)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (MenuPrincipal Creditos, imagens) = (MenuPrincipal Jogar, imagens)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (MenuPrincipal Creditos, imagens) = (MenuPrincipal Controlos, imagens)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (MenuPrincipal Controlos, imagens) = (MenuPrincipal Creditos, imagens)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (MenuPrincipal Controlos, imagens) = (MenuPrincipal Controlos, imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (MenuPrincipal Controlos, imagens) = (MenuControlos, imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (MenuControlos, imagens) = (MenuPrincipal Controlos, imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (MenuPrincipal Creditos, imagens) = (MenuCreditos, imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (MenuCreditos, imagens) = (MenuPrincipal Creditos, imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (MenuPrincipal Jogar, imagens) = (ModoDN 0, imagens)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (ModoDN _, imagens) = (ModoDN 1, imagens)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (ModoDN _, imagens) = (ModoDN 0, imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (ModoDN s, imagens) = (EscolheMapa (s,0), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheMapa (s,0), imagens) = (EscolheJogador (s,(0,0)), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheMapa (s,1), imagens) = (EscolheJogador (s,(0,1)), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheMapa (s,2), imagens) = (EscolheJogador (s,(0,2)), imagens)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (EscolheMapa (s,x), imagens) = if (x == 0) then (EscolheMapa (s,0), imagens) else (EscolheMapa (s,x-1), imagens)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (EscolheMapa (s,x), imagens) = if (x == 2) then (EscolheMapa (s,2), imagens) else (EscolheMapa (s,x+1), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheJogador (s,(0,0)), imagens) = (ModoJogo (Jogo mapa0 (Jogador (10,6) Oeste False),0,0,s), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheJogador (s,(1,0)), imagens) = (ModoJogo (Jogo mapa0 (Jogador (10,6) Oeste False),1,0,s), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheJogador (s,(2,0)), imagens) = (ModoJogo (Jogo mapa0 (Jogador (10,6) Oeste False),2,0,s), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheJogador (s,(0,1)), imagens) = (ModoJogo (Jogo mapa1 (Jogador (10,2) Oeste False),0,1,s), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheJogador (s,(1,1)), imagens) = (ModoJogo (Jogo mapa1 (Jogador (10,2) Oeste False),1,1,s), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheJogador (s,(2,1)), imagens) = (ModoJogo (Jogo mapa1 (Jogador (10,2) Oeste False),2,1,s), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheJogador (s,(0,2)), imagens) = (ModoJogo (Jogo mapa2 (Jogador (10,0) Oeste False),0,2,s), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheJogador (s,(1,2)), imagens) = (ModoJogo (Jogo mapa2 (Jogador (10,0) Oeste False),1,2,s), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheJogador (s,(2,2)), imagens) = (ModoJogo (Jogo mapa2 (Jogador (10,0) Oeste False),2,2,s), imagens)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (EscolheJogador (s,(x,y)), imagens) = if (x == 0) then (EscolheJogador (s,(0,y)), imagens) else (EscolheJogador (s,(x-1,y)), imagens) 
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (EscolheJogador (s,(x,y)), imagens) = if (x == 2) then (EscolheJogador (s,(2,y)), imagens) else (EscolheJogador (s,(x+1,y)), imagens)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo (j@(Jogo l (Jogador (a,b) c d)),x,y,s), imagens) = if cimadaPorta (moveJogador j Trepar) (coordenadaPorta l) then (MenuVitoria (s,JogarNovamente), imagens) else (ModoJogo (moveJogador j Trepar,x,y,s), imagens)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo (j@(Jogo l (Jogador (a,b) c d)),x,y,s), imagens) = if cimadaPorta (moveJogador j AndarEsquerda) (coordenadaPorta l) then (MenuVitoria (s,JogarNovamente), imagens) else (ModoJogo (moveJogador j AndarEsquerda,x,y,s), imagens)
reageEvento (EventKey (Char 'c') Down _ _) (ModoJogo (jogo,x,y,s), imagens) = (ModoJogo (moveJogador jogo InterageCaixa,x,y,s), imagens)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo (j@(Jogo l (Jogador (a,b) c d)),x,y,s), imagens) = if cimadaPorta (moveJogador j AndarDireita) (coordenadaPorta l) then (MenuVitoria (s,JogarNovamente), imagens) else (ModoJogo (moveJogador j AndarDireita,x,y,s), imagens)
reageEvento (EventKey (Char 'r') Down _ _) (ModoJogo (jogo,x,0,s), imagens) = (ModoJogo (Jogo mapa0 (Jogador (10,6) Oeste False),x,0,s), imagens)
reageEvento (EventKey (Char 'r') Down _ _) (ModoJogo (jogo,x,1,s), imagens) = (ModoJogo (Jogo mapa1 (Jogador (10,2) Oeste False),x,1,s), imagens)
reageEvento (EventKey (Char 'r') Down _ _) (ModoJogo (jogo,x,2,s), imagens) = (ModoJogo (Jogo mapa2 (Jogador (10,0) Oeste False),x,2,s), imagens)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (MenuVitoria (s,_), imagens) = (MenuVitoria (s,Sair), imagens)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (MenuVitoria (s,_), imagens) = (MenuVitoria (s,JogarNovamente), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (MenuVitoria (s,Sair), imagens) = undefined
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (MenuVitoria (s,JogarNovamente), imagens) = (MenuPrincipal Jogar, imagens)
reageEvento _ m = m

-- | Esta função atibui a cada Mundo uma Imagem ou uma série de Imagens, isto conforme o tipo de Mundo.

desenhaMundo :: Mundo -> Picture
desenhaMundo (MenuPrincipal Jogar, imagens) = fromJust (lookup Pfundo1 imagens)
desenhaMundo (MenuPrincipal Creditos, imagens) = fromJust (lookup Pfundo2 imagens)
desenhaMundo (MenuPrincipal Controlos, imagens) = fromJust (lookup Pfundo3 imagens)
desenhaMundo (MenuControlos, imagens) = fromJust (lookup Teclas imagens)
desenhaMundo (MenuCreditos, imagens) = fromJust (lookup Fundo4 imagens) 
desenhaMundo (ModoDN 0, imagens) = fromJust (lookup Noite1 imagens)
desenhaMundo (ModoDN 1, imagens) = fromJust (lookup Noite2 imagens)
desenhaMundo (EscolheMapa (s,0), imagens) = if s == 0 then fromJust (lookup Fundo6 imagens) else fromJust (lookup Noite3 imagens) 
desenhaMundo (EscolheMapa (s,1), imagens) = if s == 0 then fromJust (lookup Fundo7 imagens) else fromJust (lookup Noite4 imagens)
desenhaMundo (EscolheMapa (s,2), imagens) = if s == 0 then fromJust (lookup Fundo8 imagens) else fromJust (lookup Noite5 imagens)
desenhaMundo (EscolheJogador (s,(0,_)), imagens) = if s == 0 then fromJust (lookup Fundo9 imagens) else fromJust (lookup Noite6 imagens)
desenhaMundo (EscolheJogador (s,(1,_)), imagens) = if s == 0 then fromJust (lookup Fundo10 imagens) else fromJust (lookup Noite7 imagens)
desenhaMundo (EscolheJogador (s,(2,_)), imagens) = if s == 0 then fromJust (lookup Fundo11 imagens) else fromJust (lookup Noite8 imagens)
desenhaMundo (MenuVitoria (s,JogarNovamente), imagens) = if s == 0 then fromJust (lookup Fundo12 imagens) else fromJust (lookup Noite9 imagens)
desenhaMundo (MenuVitoria (s,Sair), imagens) = if s == 0 then fromJust (lookup Fundo13 imagens) else fromJust (lookup Noite10 imagens)
desenhaMundo (ModoJogo (Jogo mapa (Jogador (a,b) f g),x,y,s), imagens) = if s == 0 then Pictures ([fromJust (lookup Fundo1 imagens)] ++ (desenhaMapa mapa (0,0) imagens) ++ (desenhaJogadorMapa mapa (0,0) (Jogador (a,b) f g) x imagens)) else Pictures ([fromJust (lookup Noite imagens)] ++ (desenhaMapa mapa (0,0) imagens) ++ (desenhaJogadorMapa mapa (0,0) (Jogador (a,b) f g) x imagens))

-- | Esta função permite colocar a Imagem de cada Peca do Mapa no seu respetivo sítio.

desenhaMapa :: Mapa -> (Int,Int) -> Imagens -> [Picture]
desenhaMapa [] _ _ = []
desenhaMapa (l:ls) (x,y) imagens = desenhaLinha l (x,y) imagens ++ desenhaMapa ls (0,y+1) imagens

-- | A função 'desenhaLinha' permite colocar a Imagem de cada Peca de uma linha no seu respetivo sítio.

desenhaLinha :: [Peca] -> (Int,Int) -> Imagens -> [Picture]
desenhaLinha [] _ _ = []
desenhaLinha (p:ps) (x,y) imagens | p == Vazio = [Translate (i-270) (j+268) $ Scale (0.3) (0.3) (fromJust (lookup Vazios imagens))] ++ desenhaLinha ps (x+1,y) imagens
                                  | p == Bloco = [Translate (i-270) (j+268) $ Scale (0.3) (0.3) (fromJust (lookup Blocos imagens))] ++ desenhaLinha ps (x+1,y) imagens
                                  | p == Porta = [Translate (i-270) (j+268) $ Scale (0.3) (0.3) (fromJust (lookup Portas imagens))] ++ desenhaLinha ps (x+1,y) imagens
                                  | p == Caixa = [Translate (i-270) (j+268) $ Scale (0.3) (0.3) (fromJust (lookup Caixas imagens))] ++ desenhaLinha ps (x+1,y) imagens
    where 
         i = fromIntegral (x*60)
         j = fromIntegral (-y*60)

-- | Se o jogador estiver numa determinada linha do Mapa, esta função vai colocar uma Imagem do Jogador no local que as suas Coordenadas indicam.

desenhaJogadorLinha :: [Peca] -> (Int,Int) -> Jogador -> Int -> Imagens -> [Picture]
desenhaJogadorLinha [] _ _ _ _= []
desenhaJogadorLinha (l:ls) (x,y) (Jogador (a,b) c d) n imagens | x == a && y == b && n == 0 && c == Este = [Translate (i-270) (j+268) $ Scale (0.3) (0.3) (fromJust (lookup Personagem1Estes imagens))]
                                                               | x == a && y == b && n == 0 && c == Oeste = [Translate (i-270) (j+268) $ Scale (0.3) (0.3) (fromJust (lookup Personagem1Oestes imagens))]
                                                               | x == a && y == b && n == 1 && c == Este = [Translate (i-270) (j+268) $ Scale (0.3) (0.3) (fromJust (lookup Personagem2Estes imagens))]
                                                               | x == a && y == b && n == 1 && c == Oeste = [Translate (i-270) (j+268) $ Scale (0.3) (0.3) (fromJust (lookup Personagem2Oestes imagens))]
                                                               | x == a && y == b && n == 2 && c == Este = [Translate (i-270) (j+268) $ Scale (0.3) (0.3) (fromJust (lookup Personagem3Estes imagens))]
                                                               | x == a && y == b && n == 2 && c == Oeste = [Translate (i-270) (j+268) $ Scale (0.3) (0.3) (fromJust (lookup Personagem3Oestes imagens))]
                                                               | otherwise = desenhaJogadorLinha ls (x+1,y) (Jogador (a,b) c d) n imagens
     where
         i = fromIntegral (x*60)
         j = fromIntegral (-y*60)

-- | Com o auxilio da função 'desenhaJogadorLinha' esta função faz com que seja possivel colocar a Imagem do Jogador em qualquer parte do Mapa.

desenhaJogadorMapa :: Mapa -> (Int,Int) -> Jogador -> Int -> Imagens -> [Picture]
desenhaJogadorMapa [] _ _ _ _ = []
desenhaJogadorMapa (l:ls) (x,y) (Jogador (a,b) c d) n imagens = desenhaJogadorLinha l (x,y) (Jogador (a,b) c d) n imagens ++ desenhaJogadorMapa ls (x,y+1) (Jogador (a,b) c d) n imagens

-- | Função principal, aquela que no fundo junta todas as anterior para fazer o Jogo acontecer.

main :: IO ()
main = do 
     caixa1 <- loadBMP "Resources1/caixa1.bmp"
     porta1 <- loadBMP "Resources1/porta1.bmp"
     vazio1 <- loadBMP "Resources1/vazio1.bmp"
     bloco1 <- loadBMP "Resources1/bloco1.bmp"
     fundo1 <- loadBMP "Resources1/fundo1.bmp"
     fundo2 <- loadBMP "Resources1/fundo2.bmp"
     fundo3 <- loadBMP "Resources1/fundo3.bmp"
     fundo4 <- loadBMP "Resources1/fundo4.bmp"
     fundo5 <- loadBMP "Resources1/fundo5.bmp"
     fundo6 <- loadBMP "Resources1/fundo6.bmp"
     fundo7 <- loadBMP "Resources1/fundo7.bmp"
     fundo8 <- loadBMP "Resources1/fundo8.bmp"
     fundo9 <- loadBMP "Resources1/fundo9.bmp"
     fundo10 <- loadBMP "Resources1/fundo10.bmp"
     fundo11 <- loadBMP "Resources1/fundo11.bmp"
     fundo12 <- loadBMP "Resources1/fundo12.bmp"
     fundo13 <- loadBMP "Resources1/fundo13.bmp"
     noite <- loadBMP "Resources1/noite.bmp"
     noite1 <- loadBMP "Resources1/fundo1noite.bmp"
     noite2 <- loadBMP "Resources1/fundo2noite.bmp"
     noite3 <- loadBMP "Resources1/fundo3noite.bmp"
     noite4 <- loadBMP "Resources1/fundo4noite.bmp"
     noite5 <- loadBMP "Resources1/fundo5noite.bmp"
     noite6 <- loadBMP "Resources1/fundo6noite.bmp"
     noite7 <- loadBMP "Resources1/fundo7noite.bmp"
     noite8 <- loadBMP "Resources1/fundo8noite.bmp"
     noite9 <- loadBMP "Resources1/fundo9noite.bmp"
     noite10 <- loadBMP "Resources1/fundo10noite.bmp"
     personagem1este <- loadBMP "Resources1/personagem1.bmp"
     personagem2este <- loadBMP "Resources1/personagem2.bmp"
     personagem3este <- loadBMP "Resources1/personagem3.bmp"
     personagem1oeste <- loadBMP "Resources1/personagem1oeste.bmp"
     personagem2oeste <- loadBMP "Resources1/personagem2oeste.bmp"
     personagem3oeste <- loadBMP "Resources1/personagem3oeste.bmp"
     pfundo1 <- loadBMP "Resources1/pfundo1.bmp"
     pfundo2 <- loadBMP "Resources1/pfundo2.bmp"
     pfundo3 <- loadBMP "Resources1/pfundo3.bmp"
     teclas <- loadBMP "Resources1/teclas.bmp"
     let imagens = [(Caixas, caixa1), (Portas, porta1), (Blocos, bloco1), (Vazios, vazio1), (Fundo1, fundo1), (Fundo2, fundo2), (Fundo3, fundo3), (Fundo4, fundo4), (Fundo5, fundo5), (Fundo6, fundo6), (Fundo7, fundo7), (Fundo8, fundo8), (Fundo9, fundo9), (Fundo10, fundo10), (Fundo11, fundo11), (Fundo12, fundo12), (Fundo13, fundo13), (Personagem1Estes, personagem1este), (Personagem2Estes, personagem2este), (Personagem3Estes, personagem3este), (Personagem1Oestes, personagem1oeste), (Personagem2Oestes, personagem2oeste), (Personagem3Oestes, personagem3oeste), (Noite1, noite1), (Noite2, noite2), (Noite3, noite3), (Noite4, noite4), (Noite5, noite5), (Noite6, noite6), (Noite7, noite7), (Noite8, noite8), (Noite9, noite9), (Noite10, noite10), (Noite, noite), (Teclas, teclas), (Pfundo1, pfundo1), (Pfundo2, pfundo2), (Pfundo3, pfundo3)]
     let estadoInicial = (MenuPrincipal Jogar, imagens)
     play mainDisplay
         (greyN 0.25)
         fr
         estadoInicial
         desenhaMundo
         reageEvento
         reageTempo