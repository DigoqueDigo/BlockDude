# Laboratórios de Informática I - BlockDude

Trabalho criado totalmente por mim no âmbito da cadeira de Laboratórios de Informática I, era suposto o trabalho ter sido feito por duas pessoas, mas infelizmente o colega que me saiu na rifa não mexeu uma palha.

## Requisitos

- GHC (Glasgow Haskell Compiler)
- gloss
- gloss-juicy
- HUnit (opcional)

## Instalação dos Requisitos

Depois de instalarem o ghc no vosso pc, executem os seguintes comandos.

```bash
$ cabal update
$ cabal install gloss
$ cabal install gloss-juicy
```
## Criar executável e Jogar

Depois de escolherem a diretoria onde querem clonar o repósitorio, executem os seguintes comandos.

```bash
$ git clone git@gitlab.com:uminho-di/li1/2122/2021li1g082.git
$ cd BlockDude/src
$ ghc -main-is Main -outputdir Tarefa5 -o BlockDude Tarefa5_2021li1g082
$ ./BlockDude
```

## Testes

O projecto contém testes unitários escritos usando a biblioteca [HUnit](https://hackage.haskell.org/package/HUnit). Os testes podem ser executados da seguinte forma.

```bash
$ ghci -i="src" -i="tests" tests/Tests.hs
>>> runTestsT1 -- Correr os testes tarefa 1
>>> runTestsT2 -- Correr os testes tarefa 2
>>> runTestsT3 -- Correr os testes tarefa 3
>>> runTestsT4 -- Correr os testes tarefa 4
>>> runAllTests -- Correr todos os testes
```

## Agradecimentos

- **Professor Nelson Estevão**, que ao contrário do meu colega de grupo sempre se mostrou disponivel para ajudar.
