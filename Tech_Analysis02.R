#############################################
# Fazendo uma Estratégia de Análise Técnica
# Claudio R. Lucinda
# FEA-RP/USP
#############################################
# devtools::install_github("braverock/blotter")
# devtools::install_github("braverock/quantstrat")

library(BatchGetSymbols)
library(PortfolioAnalytics)
library(xlsx)
library(tidyverse)
library(quantmod)
library(tidyquant)
library(quantstrat)

rm(list=ls())
graphics.off()

initdate<-"2004-01-01"
from<-"2014-01-01"
to<-"2018-04-23"

# Set system environment timezone:
Sys.setenv(TZ = "UTC")
# Set currency (we’ll use USD for now):
currency("BRL")
# Obtain financial data:
getSymbols("PETR4.SA", from = from, to = to,
             src = "yahoo", adjust = TRUE)
# Treat as basic equity
stock("PETR4.SA", currency = "BRL", multiplier = 1)

#Tamanho de Trade
tradesize <- 10000
initeq <- 100000

strategy.st <- portfolio.st <- account.st <- "firststrat"

# Inicializando a Carteira
initPortf(portfolio.st,
          symbols = "PETR4.SA",
          initDate = initdate,
          currency = "BRL")

# Inicializando a conta
initAcct(account.st,
         portfolios = portfolio.st,
         initDate = initdate,
         currency = "BRL",
         initEq = initeq)

# Inicializando as Ordens
initOrders(portfolio.st, initDate = initdate)

# Inicializando a estratégia
strategy(strategy.st, store = TRUE)


# Inicializando os Indicadores - 
# Aqui vou fazer um esquema simples com relação a médias móveis

add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n = 50),
              label = "SMA50")

add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n = 10),
              label = "SMA10")

# Vendo os dados
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(PETR4.SA))
tail(test, n = 3)

# Gerando os sinais de compra
add.signal(strategy.st,
           name = "sigCrossover",
           arguments = list(columns = c("SMA10", "SMA50"),
                            relationship = "gt"),
           label = "longfilter")

add.signal(strategy.st,
           name = "sigCrossover",
           arguments = list(columns = c("SMA10", "SMA50"),
                            relationship = "lt"),
           label = "shortfilter")


test2<-applySignals(strategy=strategy.st,mktdata = test)

# Gerando o que fazer em caso de sinal
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "longfilter", sigval = TRUE,
                          orderqty = 100, ordertype = "market",
                          orderside = "long"),
         type = "enter")


add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "shortfilter", sigval = TRUE,
                          orderqty = 100, ordertype = "market",
                          orderside = "short"),
         type = "exit")


# Rodando a estratégia

applyStrategy(strategy = strategy.st,
                portfolios = portfolio.st)
updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(account.st, daterange)
updateEndEq(account.st)

tStats <- dailyStats(Portfolios = portfolio.st, envir=.blotter)
tStats

sma10 <- SMA(x = Cl(PETR4.SA), n = 10)
sma50 <- SMA(x = Cl(PETR4.SA), n = 50)

chart.Posn(Portfolio = portfolio.st, symbol = "PETR4.SA")
add_TA(sma10, on = 1, col = "blue")
add_TA(sma50, on = 1, col = "red")

