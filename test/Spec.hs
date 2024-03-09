import Lib
import Test.Hspec


main :: IO ()
main = hspec $ do

        describe "Tarefa 2: Jogo 2048" $ do

            it "criaTabuleiro - teste para criar tabuleiro vazio" $
                criaTabuleiro `shouldBe` [[Vazia,Vazia,Vazia,Vazia],[Vazia,Vazia,Vazia,Vazia],[Vazia,Vazia,Vazia,Vazia],[Vazia,Vazia,Vazia,Vazia]]
                               
            it "exibePosicao - mostra a posicao Vazio" $
                exibePosicao Vazia `shouldBe` "____  "
            
            it "posicoesVazias - recupera as posicoes onde esta Vazia, nesse caso todas" $
                posicoesVazias [[Vazia,Vazia,Vazia,Vazia],[Vazia,Vazia,Vazia,Vazia],[Vazia,Vazia,Vazia,Vazia],[Vazia,Vazia,Vazia,Vazia]] `shouldBe` [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
            it "posicoesVazias - recupera as posicoes onde esta Vazia" $
                posicoesVazias [[Vazia,Vazia,Ocupada 2,Vazia],[Ocupada 2,Vazia,Vazia,Vazia],[Vazia,Vazia,Vazia,Vazia],[Vazia,Vazia,Vazia,Ocupada 2]] `shouldBe` [0,1,3,5,6,7,8,9,10,11,12,13,14]

            it "verificalinha - ve quais posicoes estao Vazias em uma lista" $
                verificalinha [Ocupada 2,Vazia,Vazia,Ocupada 2] 0 0 `shouldBe` [1,2]
            it "verAtributo - verifica se o atributo e Vazia e retorna True" $
                verAtributo Vazia `shouldBe` True

            it "verAtributo - verifica se o atributo e Vazia e retorna True" $
                verAtributo (Ocupada 2) `shouldBe` False
            it "traduzposicao - recebe em qual linha e coluna esta o elemento e retorna sua posicao em um unico valor de 0 a 15" $
                traduzposicao 2 3 `shouldBe` 11

            it "traduzposicao - recebe em qual linha e coluna esta o elemento e retorna sua posicao em um unico valor de 0 a 15" $
               traduzposicao 0 2 `shouldBe` 2
            it "insereEmPosicao - Insere Ocupada 2 em um tabuleiro na posicao que lhe e passada de 0 a 15" $
               insereEmPosicao [[Vazia,Vazia,Vazia,Vazia],[Vazia,Vazia,Vazia,Vazia],[Vazia,Vazia,Vazia,Vazia],[Vazia,Vazia,Vazia,Vazia]] 11 `shouldBe` [[Vazia,Vazia,Vazia,Vazia],[Vazia,Vazia,Vazia,Vazia],[Vazia,Vazia,Vazia,Ocupada 2],[Vazia,Vazia,Vazia,Vazia]]
            it "modificacoluna - coloca o elemento Ocupada 2 na coluna correta de acordo com a posicao informada" $
                modificacoluna [Vazia,Vazia,Vazia,Vazia] 13`shouldBe` [Vazia,Ocupada 2,Vazia,Vazia]
                
            it "moveDireita - move todos os elementos Ocupada _ para direita se possivel e soma os iguais" $
                moveDireita [[Vazia,Ocupada 2, Ocupada 2, Vazia],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Ocupada 2,Ocupada 2, Ocupada 2, Ocupada 4]] `shouldBe` [[Vazia,Vazia, Vazia, Ocupada 4],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Vazia,Ocupada 2, Ocupada 4, Ocupada 4]]
                
            it "moveEsquerda - move todos os elementos Ocupada _ para esquerda se possivel e soma os iguais" $
                moveEsquerda [[Vazia,Ocupada 2, Ocupada 2, Vazia],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Ocupada 2,Ocupada 2, Ocupada 2, Ocupada 4]] `shouldBe` [[Ocupada 4, Vazia, Vazia, Vazia],[Ocupada 2,Ocupada 4,Vazia, Vazia],[Ocupada 2,Ocupada 4,Vazia,Vazia],[Ocupada 4,Ocupada 2, Ocupada 4, Vazia]]
                
            it "linToCol - faz com que as linhas virem colunas no tabuleiro" $
                linToCol [Vazia,Ocupada 2, Ocupada 2, Vazia] [Vazia,Vazia, Ocupada 2, Ocupada 4] [Vazia,Vazia, Ocupada 2, Ocupada 4] [Ocupada 2,Ocupada 2, Ocupada 2, Ocupada 4] `shouldBe` [[Vazia,Vazia,Vazia,Ocupada 2],[Ocupada 2,Vazia,Vazia,Ocupada 2],[Ocupada 2,Ocupada 2,Ocupada 2,Ocupada 2],[Vazia,Ocupada 4,Ocupada 4,Ocupada 4]]
                
            it "converte - Faz com que as linhas do tabuleiro se tornem as colunas" $
                converte [[Vazia,Ocupada 2, Ocupada 2, Vazia],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Ocupada 2,Ocupada 2, Ocupada 2, Ocupada 4]] `shouldBe` [[Vazia,Vazia,Vazia,Ocupada 2],[Ocupada 2,Vazia,Vazia,Ocupada 2],[Ocupada 2,Ocupada 2,Ocupada 2,Ocupada 2],[Vazia,Ocupada 4,Ocupada 4,Ocupada 4]]
                
            it "moveBaixo - move todos os elementos Ocupada _ para Baixo se possivel e soma os iguais" $
                moveBaixo [[Vazia,Ocupada 2, Ocupada 2, Vazia],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Ocupada 2,Ocupada 2, Ocupada 2, Ocupada 4]] `shouldBe` [[Vazia,Vazia,Vazia,Vazia],[Vazia,Vazia,Vazia,Vazia],[Vazia,Vazia,Ocupada 4,Ocupada 4],[Ocupada 2,Ocupada 4,Ocupada 4,Ocupada 8]]
                
            it "moveCima - move todos os elementos Ocupada _ para Cima se possivel e soma os iguais " $
                moveCima [[Vazia,Ocupada 2, Ocupada 2, Vazia],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Ocupada 2,Ocupada 2, Ocupada 2, Ocupada 4]] `shouldBe` [[Ocupada 2,Ocupada 4,Ocupada 4,Ocupada 8],[Vazia,Vazia,Ocupada 4,Ocupada 4],[Vazia,Vazia,Vazia,Vazia],[Vazia,Vazia,Vazia,Vazia]]
                
            it "moveVazioD - Move os elementos Ocupada para direita " $
                moveVazioD [Vazia,Ocupada 2, Ocupada 2, Vazia] `shouldBe` [Vazia,Vazia,Ocupada 2,Ocupada 2]
                
            it "moveVazioE - Move os elementos Ocupada para esquerda" $
                moveVazioE [Vazia,Ocupada 2, Ocupada 2, Vazia] `shouldBe` [Ocupada 2, Ocupada 2, Vazia,Vazia]
                
            it "jogaLinha - Faz as operações necessárias em cada linha com os elementos Ocupada " $
                jogaLinha [Vazia,Ocupada 2, Ocupada 2, Vazia] `shouldBe` [Vazia,Vazia, Ocupada 4, Vazia]
                
            it "somaElem - Soma dois elementos Ocupada do tipo Posicao " $
                somaElem (Ocupada 2) (Ocupada 2) `shouldBe` (Ocupada 4)
                               
            it "busca - busca um numero em uma lista" $
                busca 2 [1,2,3,5] `shouldBe` True
            it "busca - busca um numero em uma lista" $
                busca 4 [1,2,3,5] `shouldBe` False
                
            it "joga - realiza os movimentos no tabuleiro" $
                joga[[Vazia,Ocupada 2, Ocupada 2, Vazia],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Ocupada 2,Ocupada 2, Ocupada 2, Ocupada 4]] Direita `shouldBe` [[Vazia,Vazia, Vazia, Ocupada 4],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Vazia,Ocupada 2, Ocupada 4, Ocupada 4]]
                
            it "joga - realiza os movimentos no tabuleiro" $
                joga [[Vazia,Ocupada 2, Ocupada 2, Vazia],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Ocupada 2,Ocupada 2, Ocupada 2, Ocupada 4]] Esquerda `shouldBe` [[Ocupada 4, Vazia, Vazia, Vazia],[Ocupada 2,Ocupada 4,Vazia, Vazia],[Ocupada 2,Ocupada 4,Vazia,Vazia],[Ocupada 4,Ocupada 2, Ocupada 4, Vazia]]
                
            it "joga - realiza os movimentos no tabuleiro" $
                joga [[Vazia,Ocupada 2, Ocupada 2, Vazia],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Ocupada 2,Ocupada 2, Ocupada 2, Ocupada 4]] Baixo `shouldBe` [[Vazia,Vazia,Vazia,Vazia],[Vazia,Vazia,Vazia,Vazia],[Vazia,Vazia,Ocupada 4,Ocupada 4],[Ocupada 2,Ocupada 4,Ocupada 4,Ocupada 8]]
            it "joga - realiza os movimentos no tabuleiro" $
                joga [[Vazia,Ocupada 2, Ocupada 2, Vazia],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Ocupada 2,Ocupada 2, Ocupada 2, Ocupada 4]] Cima `shouldBe` [[Ocupada 2,Ocupada 4,Ocupada 4,Ocupada 8],[Vazia,Vazia,Ocupada 4,Ocupada 4],[Vazia,Vazia,Vazia,Vazia],[Vazia,Vazia,Vazia,Vazia]]
                
            it "fimJogo - verifica se ainda e possivel realizar algum movimento, senao o jogo e encerrado" $
                fimJogo [[Ocupada 2,Ocupada 4,Ocupada 2,Ocupada 4],[Ocupada 4,Ocupada 2,Ocupada 4,Ocupada 2],[Ocupada 2,Ocupada 4,Ocupada 2,Ocupada 4],[Ocupada 4,Ocupada 2,Ocupada 4,Ocupada 2]] `shouldBe` True
            
            it "fimJogo - verifica se ainda e possivel realizar algum movimento, senao o jogo e encerrado" $
                fimJogo [[Ocupada 2,Ocupada 2,Ocupada 2,Ocupada 4],[Ocupada 4,Ocupada 2,Ocupada 4,Ocupada 2],[Ocupada 2,Ocupada 4,Ocupada 2,Ocupada 4],[Ocupada 4,Ocupada 2,Ocupada 4,Ocupada 2]] `shouldBe` False 
            
            it "verifica2048 - verifica se o valor 2048 foi atingido em Ocupada" $
                verifica2048 [[Vazia,Ocupada 2, Ocupada 2, Vazia],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Ocupada 2,Ocupada 2, Ocupada 2048, Ocupada 4]] `shouldBe` True 
                
            it "verifica2048 - verifica se o valor 2048 foi atingido em Ocupada" $
                verifica2048 [[Vazia,Ocupada 2, Ocupada 2, Vazia],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Vazia,Vazia, Ocupada 2, Ocupada 4],[Ocupada 2,Ocupada 2, Ocupada 2, Ocupada 4]] `shouldBe` False 
