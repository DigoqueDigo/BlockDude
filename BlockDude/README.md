# Projeto de Laboratórios de Informática I

## Clonar o Repositório

```bash
$ git clone https://github.com/DigoqueDigo/BlockDude.git
```

## "Requirements"

Haskell compiler (GHC)
|| gloss
|| gloss-juicy
|| HUnit (opcinal, apenas serve para correr os testes)

## Comandos para instalar os "Requirements"

```bash
$ sudo pacman -S ghc cabal-install happy alex haskell-haddock-library
$ cabal update
$ cabal install gloss
$ cabal install gloss-juicy
```
*Atenção que o primeiro comando é apenas para distribuições Linux baseadas em Arch

*Acho que o gloss-juicy não está disponizel em Windows, portanto não é possivel compilar as Tarefas

## Gerar Executável e Jogar

```bash
$ ghc Tarefa5_2021li1g082.hs
$ ./Tarefa5_2021li1g082
``` 

## Testes

O projecto contém testes unitários escritos usando a biblioteca [HUnit](https://hackage.haskell.org/package/HUnit). Os testes podem ser executados da seguinte forma.

```bash
$ ghci -i="src" -i="tests" tests/Tests.hs
>>> runTestsT1 -- Correr os testes da tarefa 1
>>> runTestsT2 -- Correr os testes da tarefa 2
>>> runTestsT3 -- Correr os testes da tarefa 3
>>> runTestsT4 -- Correr os testes da tarefa 4
>>> runTestsT6 -- Correr os testes da tarefa 6
>>> runAllTests -- Correr todos os testes
```
