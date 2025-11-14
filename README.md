# RA2---Funcional: Sistema de Inventário em Haskell

Este é o nosso projeto para a RA2 da disciplina de Programação Logica e Funcional.

O objetivo foi construir um sistema de gerenciamento de inventário que roda no terminal.O principal desafio e o foco do trabalho foi usar os conceitos de Haskell, principalmente a **separação total entre a lógica pura** (em `Logica.hs` e `Relatorios.hs`) e as **operações impuras de I/O** (todas centralizadas no `main.hs`).

---

###  Informações do Grupo

* **Instituição:** Pontificia Universidade Catolica do Parana - PUCPR
* **Disciplina:** Programação Logica Funcional
* **Professor:** Frank Coelho de Alcantara
* **Alunos:**
    * Gustavo Lisboa Prestes - GitHub: Legarde2
    * Vinicius Edivaldo Souza - GitHub: ViniCodemosSZ

---

### Link de Execução

O projeto pode ser compilado e executado diretamente neste ambiente GDB Online:

**CLIQUE AQUI PARA EXECUTAR O PROJETO: https://onlinegdb.com/4HQf_X0IiB**


### Como Rodar

1.  Abra o link do GDB Online acima.
2.  Clique no botão "Run" (verde) no topo.
3.  Interaja com o programa no console na parte inferior.
4.  O sistema salvará os arquivos `Inventario.dat` e `Auditoria.log` no próprio ambiente do GDB (você os verá aparecer na lista de arquivos à esquerda).
5.  Caso os arquivos inventario.dat e auditoria.log ja existm voce pode exclui-los sem problemas para fins de teste

### Metodo Alternativo
Como alternativa, você pode baixar os arquivos do repositório, abri-los em sua IDE ou plataforma online de preferência e executar o código localmente.


### Documentação dos Testes Manuais

Abaixo estão os resultados da execução dos três cenários de teste obrigatórios, conforme solicitado no PDF.

*(**Nota:** Os arquivos `.dat` e `.log` foram deletados antes de iniciar o Cenário 1)*

#### Cenário 1: Persistência de Estado (Sucesso)

**Objetivo:** Verificar se o sistema salva o estado (`Inventario.dat`) e os logs (`Auditoria.log`) após operações de sucesso e os recarrega na inicialização.

**Passo 1: Primeira Execução (Arquivos Inexistentes)**
(O programa inicia e o `catch` lida com a ausência dos arquivos)

```
Carregando Inventario.dat... Aviso: Arquivo não encontrado ou permissão negada. Iniciando com dados vazios. Carregando Auditoria.log... Aviso: Arquivo não encontrado ou permissão negada. Iniciando com dados vazios.

Sistema de Gerenciamento de Inventário Comandos: add, remove, update, popular, listar, report, sair

```
**Passo 2: Popular com 10 Itens Mínimos e Sair**
(Usamos o comando `popular` para atender ao requisito de 10 itens)

```
Comando (add, remove, update, popular, listar, report, sair): popular Populando o inventário com itens de teste... Inventário populado com 10 novos itens! Estado do inventário salvo em Inventario.dat. Operação registrada em Auditoria.log. (mais 9 mensagens de 'Operação registrada...')

Comando (add, remove, update, popular, listar, report, sair): sair Encerrando...
```

**Passo 3: Segunda Execução (Carregar Arquivos Salvos)**
(Rodamos o programa novamente. Desta vez, ele encontra e lê os 10 itens salvos.)

```
Carregando Inventario.dat... Inventário carregado com sucesso. Carregando Auditoria.log... Logs carregados com sucesso.

Sistema de Gerenciamento de Inventário Comandos: add, remove, update, popular, listar, report, sair
```

**Resultado:** O Cenário 1 foi um sucesso. Os arquivos foram criados, populados com 10 itens e lidos na reinicialização.


#### Cenário 2: Erro de Lógica (Estoque Insuficiente)

**Objetivo:** Verificar se a lógica pura impede uma operação inválida e se o `main` registra a falha corretamente.

**Passo 1: Tentar Remover Estoque Insuficiente**
(Continuando da execução anterior, tentamos remover 15 "Teclados" (ID `001`), mas só temos 10)

```
Comando (add, remove, update, popular, listar, report, sair): remove ID do item: 001 Quantidade a remover: 15 Falha na operação: Erro: Estoque insuficiente. Operação registrada em Auditoria.log.
```

**Resultado:** O Cenário 2 foi um sucesso. O sistema detectou o erro, salvou *apenas* o log de falha e não travou.

---

#### Cenário 3: Geração de Relatório de Erros

**Objetivo:** Verificar se a função `logsDeErro` consegue ler os logs carregados e exibir a falha registrada no Cenário 2.

**Passo 1: Executar o Comando "report" e selecionar "1"**

```
Comando (add, remove, update, popular, report, sair): report

--- MÓDULO DE RELATÓRIOS --- 1: Ver logs de erro 2: Ver histórico por item 3: Ver item mais movimentado Escolha o relatório (ou 'sair' para voltar): 1

[Relatório: Logs de Erro] LogEntry {timestamp = 2025-11-13 19:15:45.601031688 UTC, acao = QueryFail, detalhes = "Erro: Estoque insuficiente.", status = Falha "Erro: Estoque insuficiente."}
```
**Resultado:** O Cenário 3 foi um sucesso. O relatório de erros identificou e exibiu a falha registrada.
