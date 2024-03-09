module Lib(criaTabuleiro, loop,exibePosicao,posicoesVazias, verificalinha,verAtributo,traduzposicao,insereEmPosicao,modificacoluna,moveDireita,moveEsquerda,linToCol,
converte,moveBaixo,colToLinB,moveCima,colToLinC, moveVazioD, moveVazioE,jogaLinha, somaElem, sortearPosicao,busca,joga,fimJogo,verifica2048,Posicao(..),Tabuleiro(..),Movimento(..)) where

import Text.Printf
import Data.List
import System.Random

data Posicao = Vazia | Ocupada Int deriving (Show, Eq)

type Tabuleiro = [[Posicao]] 

data Movimento = Cima | Baixo | Esquerda | Direita deriving Show

--Inicia Tabuleiro com todas as posicoes zeradas
linha:: Int -> [Posicao]
linha 0 = []
linha c = Vazia : linha (c-1)

criaTabuleiro:: Tabuleiro

criaTabuleiroaux::Int -> Int -> Tabuleiro
criaTabuleiroaux 0 _ = []
criaTabuleiroaux l c = (linha c) : criaTabuleiroaux (l-1) c

criaTabuleiro = criaTabuleiroaux 4 4
-------------------------------------------------

-- FUNÇOES PARA EXIBIR O TABULEIRO NAQUELE MOMENTO
exibeTabuleiro :: Tabuleiro -> IO ()
exibeTabuleiro tabuleiro = do
    putStr "\n"
    mapM_ (putStrLn . exibeLinha) tabuleiro

exibePosicao :: Posicao -> String
exibePosicao Vazia = (printf "____  ")
exibePosicao (Ocupada a) = (printf "% 4d  " a)

exibeLinha :: [Posicao] -> String
exibeLinha [] = " "
exibeLinha (x:xs) = exibePosicao x ++ exibeLinha xs
--------------------------------------------------

--FUNÇÃO PARA VERIFICAR QUAIS POSIÇÕES ESTÃO VAZIAS
posicoesVazias:: Tabuleiro -> [Int]
posicoesVaziasaux:: Tabuleiro -> Int -> Int -> [Int]
verificalinha:: [Posicao] -> Int -> Int -> [Int] 

posicoesVazias tabuleiro = posicoesVaziasaux tabuleiro 0 0

posicoesVaziasaux [] _ _ = []
posicoesVaziasaux (x:xs) l c = verificalinha x l c ++ posicoesVaziasaux xs (l+1) 0

verificalinha [] _ _ = []
verificalinha (x:xs) l c
 |verAtributo x = (traduzposicao l c) : verificalinha xs l (c+1)
 |otherwise = verificalinha xs l (c+1)

--Verifica se a Posicao é Vazia
verAtributo:: Posicao -> Bool

verAtributo Vazia = True
verAtributo (Ocupada _) = False
------------------------------- 

--Funcao que pega os indices da matriz e transforma em um identificador unico, como foi pedido
traduzposicao:: Int -> Int -> Int 

traduzposicao l c 
 |(l == 0) && (c == 0) = 0
 |(l == 0) && (c == 1) = 1
 |(l == 0) && (c == 2) = 2
 |(l == 0) && (c == 3) = 3
 |(l == 1) && (c == 0) = 4
 |(l == 1) && (c == 1) = 5
 |(l == 1) && (c == 2) = 6
 |(l == 1) && (c == 3) = 7
 |(l == 2) && (c == 0) = 8
 |(l == 2) && (c == 1) = 9
 |(l == 2) && (c == 2) = 10
 |(l == 2) && (c == 3) = 11
 |(l == 3) && (c == 0) = 12
 |(l == 3) && (c == 1) = 13
 |(l == 3) && (c == 2) = 14
 |(l == 3) && (c == 3) = 15

--------------------------------------------------

--FUNÇÃO QUE INSERE 2 EM DETERMINADA POSICAO

insereEmPosicao  :: Tabuleiro -> Int -> Tabuleiro
modificacoluna :: [Posicao] -> Int -> [Posicao]

insereEmPosicao (a:b:c:d:e) x
 | x <= 3 = (modificacoluna a x):b:c:d:[]
 | x <= 7 = a:(modificacoluna b x):c:d:[]
 | x <= 11 = a:b:(modificacoluna c x):d:[]
 |otherwise = a:b:c:(modificacoluna d x):[]

modificacoluna (a:b:c:d:e) x
 | (x `mod` 4) == 0 = (Ocupada 2) : b : c : d:[]
 | (x `mod` 4) == 1 = a : (Ocupada 2) : c : d:[]
 | (x `mod` 4) == 2 = a : b : (Ocupada 2) : d:[]
 | (x `mod` 4) == 3 = a : b : c : (Ocupada 2):[]
--------------------------------------------------
--mover os atributos Ocupada _ para direita

moveDireita::Tabuleiro -> Tabuleiro
moveDireita [] = []
moveDireita (a:as) = (moveVazioD (reverse(jogaLinha (reverse a)))):(moveDireita as)
--------------------------------------------------

--mover os atributos Ocupada _ para esquerda

moveEsquerda::Tabuleiro -> Tabuleiro
moveEsquerda [] = []
moveEsquerda (a:as) = ((moveVazioE (jogaLinha a))):(moveEsquerda as)
---------------------------------------------------

--TRANSFORMA AS LINHAS EM COLUNAS-----------------
linToCol::  [Posicao] -> [Posicao] -> [Posicao] -> [Posicao] -> Tabuleiro
converte:: Tabuleiro -> Tabuleiro
converte (a:b:c:d:as) = (linToCol a b c d)   
linToCol [] [] [] [] = []
linToCol(a:as) (b:bs) (c:cs) (d:ds) = (a:b:c:d:[]): (linToCol as bs cs ds)
---------------------------------------------------

--move os atributos Ocupada _ para baixo-------------------------------
moveBaixo:: Tabuleiro -> Tabuleiro
moveBaixo [] = []
moveBaixo (a:b:c:d:as) = converte(colToLinB a b c d)

--Faz os calculos para cada linha gerada pelas colunas para baixo
colToLinB:: [Posicao] -> [Posicao] -> [Posicao] -> [Posicao] -> Tabuleiro
colToLinB [] [] [] [] = []
colToLinB (a:as) (b:bs) (c:cs) (d:ds) = moveVazioD (reverse(jogaLinha (reverse(a:b:c:d:[])))): (colToLinB  as bs cs ds)
--------------------FIM MOVEBAIXO----------------------

--move os atributos Ocupada _ para cima-------------------------------
moveCima:: Tabuleiro -> Tabuleiro
moveCima [] = []
moveCima (a:b:c:d:as) = converte(colToLinC a b c d)
--auxiliar da moveCima, faz os calculos para cada linha gerada pelas colunas para cima
colToLinC:: [Posicao] -> [Posicao] -> [Posicao] -> [Posicao] -> Tabuleiro
colToLinC [] [] [] [] = []
colToLinC (a:as) (b:bs) (c:cs) (d:ds) =  (moveVazioE (jogaLinha (a:b:c:d:[]))): (colToLinC  as bs cs ds)
--------------------FIM MOVECIMA----------------------

--Move os elementos Ocupada para direita
moveVazioD:: [Posicao] -> [Posicao]
moveVazioD linha
 |length (filter (/=Vazia) linha) == 3 = Vazia: (filter (/=Vazia) linha)
 |length (filter (/=Vazia) linha) == 2 = Vazia:Vazia: (filter (/=Vazia) linha)
 |length (filter (/=Vazia) linha) == 1 = Vazia:Vazia:Vazia:(filter (/=Vazia) linha)
 |otherwise = linha
--Move os elementos Ocupada para esquerda
moveVazioE:: [Posicao] -> [Posicao]
moveVazioE linha
 |length (filter (/=Vazia) linha) == 3 = (filter (/=Vazia) linha) ++[Vazia]
 |length (filter (/=Vazia) linha) == 2 = (filter (/=Vazia) linha) ++[Vazia,Vazia]
 |length (filter (/=Vazia) linha) == 1 = (filter (/=Vazia) linha) ++[Vazia,Vazia,Vazia]
 |otherwise = linha
--------------------------------------------------
--Faz as operações necessárias em cada linha com os elementos Ocupada
jogaLinha:: [Posicao] -> [Posicao]
jogaLinha [] = []
jogaLinha (a:[]) = a:[]
jogaLinha(a:b:cs)
 | (verAtributo a) && (verAtributo b) = a:b:(jogaLinha cs)
 | (verAtributo a) = a:(jogaLinha (b:cs))
 | (verAtributo b) = b:(jogaLinha (a:cs))
 | a /= b =  a:(jogaLinha (b:cs))
 | a == b = (Vazia):(somaElem a b): (jogaLinha (cs))
 
--soma os elementos iguais 
somaElem:: Posicao -> Posicao -> Posicao
somaElem (Ocupada a) (Ocupada b) = Ocupada (a+b)
--------------------------------------------------

--Função para sortear Posicao Vazia
sortearPosicao:: Tabuleiro -> IO Int
sortearPosicao tabuleiro = do
 numero <- (randomRIO (0,15)::IO Int)
 if (posicoesVazias tabuleiro) == [] 
  then return(-1)--tratamento para caso n tenha posicao vazia no tabuleiro
  else if busca (numero) (posicoesVazias tabuleiro) 
   then return(numero)
   else sortearPosicao tabuleiro 
------------------------------------------------
   
-- Busca elemento em uma lista ---------------
busca:: Int -> [Int] -> Bool
busca _ [] = False
busca a (x:xs)
 | a == x = True
 | otherwise = busca a xs
--------------------------------------------------
--FUNCAO JOGA TABULEIRO------------------------

joga:: Tabuleiro -> Movimento -> Tabuleiro

joga tabuleiro Cima = moveCima tabuleiro
joga tabuleiro Baixo = moveBaixo tabuleiro
joga tabuleiro Esquerda = moveEsquerda tabuleiro
joga tabuleiro Direita = moveDireita tabuleiro
--------------------------------------------------
--Funcao que le o caracter digitado e se for WASD ou wasd retorna o movimento especifico
leMovimento:: IO Movimento
leMovimento = do
 letra <- getChar
 if (letra == 'W') ||(letra == 'w') 
  then return(Cima)
  else if  (letra == 'S') ||(letra == 's') 
   then return(Baixo)
   else if (letra == 'A') ||(letra == 'a') 
    then return(Esquerda)
    else if (letra == 'D') ||(letra == 'd') 
     then return(Direita)
     else do
      leMovimento
--------------------------------------------------      

--Funcao que verifica se ainda e possivel realizar algum movimento, senao o jogo e encerrado--
fimJogo:: Tabuleiro-> Bool
fimJogo tabuleiro 
 | (posicoesVazias tabuleiro== []) && ((tabuleiro == joga tabuleiro Cima) && (tabuleiro == joga tabuleiro Baixo) &&(tabuleiro == joga tabuleiro Esquerda) && (tabuleiro == joga tabuleiro Direita)) = True
 | otherwise = False
--------------------------------------------------

--Verifica se o valor 2048 foi atingido-----------
verifica2048:: Tabuleiro -> Bool
verifica2048 [] = False
verifica2048 (a:as)
 | length (filter (==(Ocupada 2048)) a) >= 1 = True
 | otherwise = verifica2048 as
--------------------------------------------------

-- Funcao responsavel por chamar as funcoes em forma de loop para que o jogo funcione  
loop:: Tabuleiro -> IO()
loop tabuleiro1 = do
  valor <- sortearPosicao tabuleiro1
  let tabuleiro2 = insereEmPosicao tabuleiro1 valor
  exibeTabuleiro tabuleiro2
  if (verifica2048 tabuleiro2) then do
   putStrLn "Voce Ganhou!!!"
  else if (fimJogo tabuleiro2) then do
   putStrLn "Fim de Jogo!"
   return()
  else do
   putStrLn "Para fazer o movimento digite:\nW(Cima)\nA(Esquerda)\nS(Baixo)\nD(Direita)" 
   mov <- leMovimento
   let tabuleiro3 = (joga tabuleiro2 mov)
   loop tabuleiro3
