# FinEconBras

Material para aplicação de R em Análise de Investimentos. Baseado no curso REC 3610. A bibliografia que apresenta o conteúdo de uma forma conceitual é o Bodie, Kane e Marcus, 10a Edição. Os códigos são:

* `Initial_Code.R` - Código para uma análise inicial, usando o pacote `PortfolioAnalytics`. Boa parte dos temas é coberto no capítulo 05 do BKM.
* `Portfolio_Analysis.R`- Código para análise de carteira, também usando o pacote `PortfolioAnalytics`
* `Ibov_data.R`- Código para baixar os dados das ações componentes do IBOVESPA, usando o mesmo pacote `PortfolioAnalytics` e `BatchGetQuotes` do [Marcelo Perlin](https://msperlin.github.io/)
* `TwoAsset.R`- Código para elaborar a álgebra de carteiras com dois ativos
* `eff_frontier.R`- Código para o desenho de uma fronteira eficiente com alguns dados do IBOVESPA, usando os comandos do `PortfolioAnalytics`
* `diversification.R`- Código para mostrar o efeito da diversificação com alguns dados do IBOVESPA
* `Fund_Data.R` - Código para construir o banco de dados de fundos para CAPM e APT, com fatores de risco baixados do [NEFIN](http://www.nefin.com.br/)

Apresentações

* `Aula01.pdf` - Aula referente ao capítulo 5 do BKM, sobre medição de risco e retorno.
* `Aula02.pdf` - Continuação da Aula 01, sobre medindo o risco com distribuições não-normais.
* `Aula03.pdf` - Aula referente ao capítulo 7 do BKM, sobre carteiras arriscadas -- com apresentação de como baixar os dados das ações do IBOVESPA do Yahoo Finance.
* `Aula04.pdf` - Ainda trabalhando o capítulo 7 do BKM, sobre carteiras arriscadas e agora desenhando a Fronteira Eficiente.

Bancos de Dados
* `Data01.xlsx` - Planilha com 14 anos de cotações de fechamento do IBOVESPA, CDI, PETR4 e BBAS3
* `Comp_IBOV.xlsx` - Composição do IBOVESPA em Março de 2018.
* `Lucinda_Nerasti.xlsx` - Banco de dados do artigo: NERASTI, João Nascimento; LUCINDA, Claudio Ribeiro. Persistência de Desempenho em Fundos de Ações no Brasil/(Persistence in Mutual Fund Performance in Brazil). Revista Brasileira de Finanças, v. 14, n. 2, p. 269, 2016. **Favor Citar o Artigo Original**
