# Projeto de Laboratórios de Informática I

## Reposit



```bash
$ git clone git@gitlab.com:uminho-di/li1/2122/2021li1g082.git
$ cd 2021 
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
