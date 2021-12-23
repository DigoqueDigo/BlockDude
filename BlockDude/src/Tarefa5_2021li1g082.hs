{- |
Module      : Tarefa5_2021li1gXXX
Description : Aplicação Gráfica

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
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

type Mundo = (Menu, Imagens) 

data Menu = MenuPrincipal OpcaoMenuPrincipal | EscolheMapa OpcaoEscolheMapa | EscolheJogador OpcaoEscolheJogador | ModoJogo (Jogo,Int) | MenuVitoria OpcaoVitoria | MenuCreditos deriving (Eq, Show)

data OpcaoMenuPrincipal = Jogar | Creditos deriving (Eq, Show, Read)  

data OpcaoVitoria = JogarNovamente | Sair deriving (Eq, Show ,Read)

type OpcaoEscolheMapa = Int

type OpcaoEscolheJogador = (Int,Int)

type Imagens = [(Foto, Picture)]

data Foto = Caixas | Blocos | Vazios | Portas | Fundo1 | Fundo2 | Fundo3 | Fundo4 | Fundo5 | Fundo6 | Fundo7 | Fundo8 | Fundo9 | Fundo10 | Fundo11 | Fundo12 | Fundo13 | Personagem1Estes | Personagem1Oestes | Personagem2Estes | Personagem2Oestes | Personagem3Estes | Personagem3Oestes deriving Eq


reageTempo :: Float -> Mundo -> Mundo
reageTempo n x = x

fr :: Int
fr = 50

mainDisplay :: Display
mainDisplay = InWindow "BlockDude" displayDimension (0,0)


-- | A dimensão da janela do jogo.

displayDimension :: (Int,Int)
displayDimension = (1280,703)


reageEvento :: Event -> Mundo -> Mundo
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (MenuPrincipal _, imagens) = (MenuPrincipal Jogar, imagens)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (MenuPrincipal _, imagens) = (MenuPrincipal Creditos, imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (MenuPrincipal Creditos, imagens) = (MenuCreditos, imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (MenuCreditos, imagens) = (MenuPrincipal Jogar, imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (MenuPrincipal Jogar, imagens) = (EscolheMapa 0, imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheMapa 0, imagens) = (EscolheJogador (0,0), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheMapa 1, imagens) = (EscolheJogador (0,1), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheMapa 2, imagens) = (EscolheJogador (0,2), imagens)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (EscolheMapa x, imagens) = if (x == 0) then (EscolheMapa 0, imagens) else (EscolheMapa (x-1), imagens)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (EscolheMapa x, imagens) = if (x == 2) then (EscolheMapa 2, imagens) else (EscolheMapa (x+1), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheJogador (0,0), imagens) = (ModoJogo (Jogo mapa0 (Jogador (10,6) Oeste False),0), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheJogador (1,0), imagens) = (ModoJogo (Jogo mapa0 (Jogador (10,6) Oeste False),1), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheJogador (2,0), imagens) = (ModoJogo (Jogo mapa0 (Jogador (10,6) Oeste False),2), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheJogador (0,1), imagens) = (ModoJogo (Jogo mapa1 (Jogador (10,2) Oeste False),0), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheJogador (1,1), imagens) = (ModoJogo (Jogo mapa1 (Jogador (10,2) Oeste False),1), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheJogador (2,1), imagens) = (ModoJogo (Jogo mapa1 (Jogador (10,2) Oeste False),2), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheJogador (0,2), imagens) = (ModoJogo (Jogo mapa2 (Jogador (10,0) Oeste False),0), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheJogador (1,2), imagens) = (ModoJogo (Jogo mapa2 (Jogador (10,0) Oeste False),1), imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (EscolheJogador (2,2), imagens) = (ModoJogo (Jogo mapa2 (Jogador (10,0) Oeste False),2), imagens)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (EscolheJogador (x,y), imagens) = if (x == 0) then (EscolheJogador (0,y), imagens) else (EscolheJogador (x-1,y), imagens) 
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (EscolheJogador (x,y), imagens) = if (x == 2) then (EscolheJogador (2,y), imagens) else (EscolheJogador (x+1,y), imagens)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo (j@(Jogo l (Jogador (a,b) c d)),x), imagens) = if cimadaPorta (moveJogador j Trepar) (coordenadaPorta l) then (MenuVitoria JogarNovamente, imagens) else (ModoJogo (moveJogador j Trepar,x), imagens)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo (j@(Jogo l (Jogador (a,b) c d)),x), imagens) = if cimadaPorta (moveJogador j AndarEsquerda) (coordenadaPorta l) then (MenuVitoria JogarNovamente, imagens) else (ModoJogo (moveJogador j AndarEsquerda,x), imagens)
reageEvento (EventKey (Char 'c') Down _ _) (ModoJogo (jogo,x), imagens) = (ModoJogo (moveJogador jogo InterageCaixa,x), imagens)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo (j@(Jogo l (Jogador (a,b) c d)),x), imagens) = if cimadaPorta (moveJogador j AndarDireita) (coordenadaPorta l) then (MenuVitoria JogarNovamente, imagens) else (ModoJogo (moveJogador j AndarDireita,x), imagens)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (MenuVitoria _, imagens) = (MenuVitoria Sair, imagens)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (MenuVitoria _, imagens) = (MenuVitoria JogarNovamente, imagens)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (MenuVitoria Sair, imagens) = undefined
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (MenuVitoria JogarNovamente, imagens) = (MenuPrincipal Jogar, imagens)
reageEvento _ m = m


desenhaMundo :: Mundo -> Picture
desenhaMundo (MenuPrincipal Jogar, imagens) = fromJust (lookup Fundo2 imagens)
desenhaMundo (MenuPrincipal Creditos, imagens) = fromJust (lookup Fundo3 imagens)
desenhaMundo (MenuCreditos, imagens) = fromJust (lookup Fundo4 imagens) 
desenhaMundo (EscolheMapa 0, imagens) = fromJust (lookup Fundo6 imagens) 
desenhaMundo (EscolheMapa 1, imagens) = fromJust (lookup Fundo7 imagens)
desenhaMundo (EscolheMapa 2, imagens) = fromJust (lookup Fundo8 imagens)
desenhaMundo (EscolheJogador (0,_), imagens) = fromJust (lookup Fundo9 imagens)
desenhaMundo (EscolheJogador (1,_), imagens) = fromJust (lookup Fundo10 imagens)
desenhaMundo (EscolheJogador (2,_), imagens) = fromJust (lookup Fundo11 imagens)
desenhaMundo (MenuVitoria JogarNovamente, imagens) = fromJust (lookup Fundo12 imagens)
desenhaMundo (MenuVitoria Sair, imagens) = fromJust (lookup Fundo13 imagens)
desenhaMundo (ModoJogo (Jogo mapa (Jogador (a,b) f g),x), imagens) = Pictures ([fromJust (lookup Fundo1 imagens)] ++ (desenhaMapa mapa (0,0) imagens) ++ (desenhaJogadorMapa mapa (0,0) (Jogador (a,b) f g) x imagens))

-- o int da frente é o do Jogador

dimensaomapa :: Mapa -> (Int, Int)
dimensaomapa [] = (0,0)
dimensaomapa m = (length m, length (head m))


desenhaMapa :: Mapa -> (Int,Int) -> Imagens -> [Picture]
desenhaMapa [] _ _ = []
desenhaMapa (l:ls) (x,y) imagens = desenhaLinha l (x,y) imagens ++ desenhaMapa ls (0,y+1) imagens


desenhaLinha :: [Peca] -> (Int,Int) -> Imagens -> [Picture]
desenhaLinha [] _ _ = []
desenhaLinha (p:ps) (x,y) imagens | p == Vazio = [Translate (i-270) (j+268) $ Scale (0.3) (0.3) (fromJust (lookup Vazios imagens))] ++ desenhaLinha ps (x+1,y) imagens
                                  | p == Bloco = [Translate (i-270) (j+268) $ Scale (0.3) (0.3) (fromJust (lookup Blocos imagens))] ++ desenhaLinha ps (x+1,y) imagens
                                  | p == Porta = [Translate (i-270) (j+268) $ Scale (0.3) (0.3) (fromJust (lookup Portas imagens))] ++ desenhaLinha ps (x+1,y) imagens
                                  | p == Caixa = [Translate (i-270) (j+268) $ Scale (0.3) (0.3) (fromJust (lookup Caixas imagens))] ++ desenhaLinha ps (x+1,y) imagens
    where 
         i = fromIntegral (x*60)
         j = fromIntegral (-y*60)


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


desenhaJogadorMapa :: Mapa -> (Int,Int) -> Jogador -> Int -> Imagens -> [Picture]
desenhaJogadorMapa [] _ _ _ _ = []
desenhaJogadorMapa (l:ls) (x,y) (Jogador (a,b) c d) n imagens = desenhaJogadorLinha l (x,y) (Jogador (a,b) c d) n imagens ++ desenhaJogadorMapa ls (x,y+1) (Jogador (a,b) c d) n imagens


-- | Função principal

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
     personagem1este <- loadBMP "Resources1/personagem1.bmp"
     personagem2este <- loadBMP "Resources1/personagem2.bmp"
     personagem3este <- loadBMP "Resources1/personagem3.bmp"
     personagem1oeste <- loadBMP "Resources1/personagem1oeste.bmp"
     personagem2oeste <- loadBMP "Resources1/personagem2oeste.bmp"
     personagem3oeste <- loadBMP "Resources1/personagem3oeste.bmp"
     let imagens = [(Caixas, caixa1), (Portas, porta1), (Blocos, bloco1), (Vazios, vazio1), (Fundo1, fundo1), (Fundo2, fundo2), (Fundo3, fundo3), (Fundo4, fundo4), (Fundo5, fundo5), (Fundo6, fundo6), (Fundo7, fundo7), (Fundo8, fundo8), (Fundo9, fundo9), (Fundo10, fundo10), (Fundo11, fundo11), (Fundo12, fundo12), (Fundo13, fundo13), (Personagem1Estes, personagem1este), (Personagem2Estes, personagem2este), (Personagem3Estes, personagem3este), (Personagem1Oestes, personagem1oeste), (Personagem2Oestes, personagem2oeste), (Personagem3Oestes, personagem3oeste)]
     let estadoInicial = (MenuPrincipal Jogar, imagens)
     play mainDisplay
         (greyN 0.25)
         fr
         estadoInicial
         desenhaMundo
         reageEvento
         reageTempo