###################################################
# Código Inicial de entrada dos dados
# Claudio R. Lucinda
# FEA-RP/USP
###################################################

# install.packages("PortfolioAnalytics")
# install.packages("xlsx")
library(PortfolioAnalytics)
library(xlsx)
library(tidyverse)

rm(list=ls())
########################################################
# Miolo de Funções
########################################################
conv_ts <- function(.df) {
  OUT<-xts(.df[,-1],order.by=.df[,1])
  return(OUT)
}




# Importando os dados
PETR4<-read.xlsx("./Data01.xlsx",sheetName="PETR4")
BBAS3<-read.xlsx("./Data01.xlsx",sheetName="BBAS3")
IBOV<-read.xlsx("./Data01.xlsx",sheetName="IBOV")
CDI<-read.xlsx("./Data01.xlsx",sheetName="CDI")
PETR4[,3:4]<-NULL
PETR4<-PETR4[!is.na(PETR4$Data),]



# Transformando para ts
PETR4<-conv_ts(PETR4)
BBAS3<-conv_ts(BBAS3)
IBOV<-conv_ts(IBOV)
CDI<-conv_ts(CDI)

# Ações Individuais
ret_PETR4<-Return.calculate(PETR4)[-1,]
ret_BBAS3<-Return.calculate(BBAS3)[-1,]
ret_IBOV<-Return.calculate(IBOV)[-1,]
ret_CDI<-Return.calculate(CDI)[-1,]

# Carteira
carteira<-merge.xts(ret_PETR4,ret_BBAS3)
peso_inicial<-c(.5,.5)

carteira_bh<-Return.portfolio(carteira,weights=peso_inicial,verbose=TRUE)
carteira_rebal<-Return.portfolio(carteira,weights=peso_inicial, rebalance_on="months",verbose=TRUE)

par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
plot.zoo(carteira_bh)
plot.zoo(carteira_rebal)

peso_bh<-carteira_bh$EOP.Weight
peso_rebal<-carteira_rebal$EOP.Weight

# Peso
par(mfrow = c(2, 1), mar=c(2, 4, 2, 2))
plot.zoo(peso_bh$ret_PETR4)
plot.zoo(peso_rebal$ret_PETR4)


#####################################################
# Características de Risco e Retorno do IBOVESPA
#####################################################

IBOV_mensal<-to.monthly(IBOV)

# Fechamento Mensal
plot.zoo(IBOV_mensal[,4])

# Calculando os Retornos Mensais
IBOV_ret_mens<-Return.calculate(IBOV_mensal[,4])

# Retornos mensais
plot.zoo(IBOV_ret_mens)

#Retornos Médios - Aritméticos
mean(IBOV_ret_mens,na.rm=T)

#Retornos Médios - Geométricos
mean.geometric(IBOV_ret_mens)

sd(IBOV_ret_mens,na.rm=T)

CDI_mensal<-to.monthly(CDI)
plot.zoo(CDI_mensal[,4])

CDI_ret_mens<-Return.calculate(CDI_mensal[,4])
plot.zoo(CDI_ret_mens)
