# Laboratórios de Informática I

## Repositório

O sistema de controlo de versões utilizado é o git. O repositório encontra-se disponível [nesta organização](https://gitlab.com/uminho-di/li1/2122). Para obter o repositório na sua máquina, garanta que tem a chave pública SSH adicionada na sua conta do GitLab com o email instituicional ([User Settings/SSH Keys](https://gitlab.com/-/profile/keys)), depois basta efetuar clone ao repositório.

```bash
$ git clone git@gitlab.com:uminho-di/li1/2122/2021li1g082.git
$ cd 2021li1g082 
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

## Grupo 82

- **A100897** Diogo Marques;
- **A100893** Luís de Castro Rodrigues Caetano;