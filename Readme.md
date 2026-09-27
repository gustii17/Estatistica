# Homework 1 - Estatística Descritiva (Bike Sharing Dataset)

Repositório destinado à resolução do Homework 1 da disciplina de TI0111 - Estatística para Engenharia da Universidade Federal do Ceará (Semestre 2026.2). 

O objetivo deste projeto é aplicar conceitos fundamentais de estatística descritiva para analisar o comportamento de usuários de um sistema de compartilhamento de bicicletas, extraindo métricas e plotando visualizações a partir do arquivo original `HW1_bike_sharing.csv`.

## 👥 Equipe
* Davi Sousa Trevia Magalhaes (554934)
* Giovanni Gabriel Remedy Milan (578412)
* Gustavo Oliveira Seabra (567464)

## 🛠️ Tecnologias e Ferramentas Utilizadas
Todo o fluxo de trabalho, desde a análise de dados até a redação do relatório acadêmico, foi centralizado no **Visual Studio Code** utilizando as seguintes ferramentas:

### 1. Linguagem R (Análise de Dados)
A extração, tratamento e análise exploratória dos dados, bem como a geração dos gráficos vetoriais (.pdf), foram realizados em ambiente R.
* **Extensão VS Code:** [R (REditorSupport)](https://marketplace.visualstudio.com/items?itemName=REditorSupport.r)
* Os scripts estão divididos modularmente na pasta `codigos/Homework_1/` e interagem com a base de dados principal.

### 2. LaTeX (Redação do Relatório)
A redação foi padronizada sob as normas técnicas da disciplina, utilizando renderização avançada de códigos com a biblioteca Pygments (pacote `minted`).
* **Extensão VS Code:** [LaTeX Workshop (James Yu)](https://marketplace.visualstudio.com/items?itemName=James-Yu.latex-workshop)
* **Nota de Compilação:** Devido ao uso do pacote `minted` para o *syntax highlighting* dos scripts R nativamente no PDF, a compilação do arquivo `hw1-report.tex` exige a ativação da *flag* `-shell-escape` no compilador (ex: `pdflatex -shell-escape hw1-report.tex`).

## 📂 Estrutura do Repositório
* `Docs/`: Contém os arquivos-fonte do relatório em LaTeX (`.tex`, `.bib`) e os gráficos em PDF renderizados a partir do R.
* `codigos/Homework_1/`: Contém os scripts em linguagem R desenvolvidos para solucionar cada questão do Homework.