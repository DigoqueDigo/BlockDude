# Laboratórios de Informática I - BlockDude

<img align = "center" width = 1050px src = "https://raw.githubusercontent.com/DigoqueDigo/BlockDude/main/src/Resources1/Captura%20de%20ecr%C3%A3%20de%202022-03-05%2000-14-15.png"/>

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
$ git clone https://github.com/DigoqueDigo/BlockDude.git
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
