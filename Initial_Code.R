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

##############################################
# Retornos Anualizados
##############################################

IBOV_anual<-Return.annualized(ret_IBOV,scale=12)
CDI_anual<-Return.annualized(ret_CDI,scale=12)

sd_IBOV_anual<-StdDev.annualized(ret_IBOV,scale=12)
sd_CDI_anual<-StdDev.annualized(ret_CDI, scale=12)

sharpe_IBOV_anual<-SharpeRatio.annualized(ret_IBOV, Rf=ret_CDI,scale=12)

##############################################
# Desenhando o retorno anualizado em uma janela de 12 meses
#########################################################

chart.RollingPerformance(R = ret_IBOV, width = 12, FUN = "Return.annualized")
abline(h = IBOV_anual)

# Plotting the 12-month rolling annualized standard deviation
chart.RollingPerformance(R = ret_IBOV, width = 12, FUN = "StdDev.annualized")
abline(h = sd_IBOV_anual)

# Plotting the 12-month rolling annualized Sharpe ratio
chart.RollingPerformance(R = ret_IBOV, width = 12, FUN = "SharpeRatio.annualized",Rf=ret_CDI)
abline(h = sharpe_IBOV_anual)


############################################################
# Análise para subperíodos
############################################################

IBOV_2008<-window(ret_IBOV, start="2008-01-01", end="2008-12-31")
IBOV_2014<-window(ret_IBOV, start="2014-01-01", end="2014-12-31")

# Parâmetros dos gráficos
par(mfrow = c(1, 2) , mar=c(3, 2, 2, 2))
names(IBOV_2008) <- "IBOV_2008"
names(IBOV_2014) <- "IBOV_2014"

# Histograma de Retornos
chart.Histogram(IBOV_2008,methods=c("add.density","add.normal"))

# Histograma de Retornos 2014
chart.Histogram(IBOV_2014,methods=c("add.density","add.normal"))

#########################################################
# Detectando a não Normalidade
#########################################################

skewness(ret_IBOV)
skewness(IBOV_ret_mens)

kurtosis(ret_IBOV)
kurtosis(IBOV_ret_mens)


########################################################
# Medida de ``Downside Risk''
########################################################
# Calculate the SemiDeviation
SemiDeviation(IBOV_ret_mens)

# Calculate the value at risk
VaR(IBOV_ret_mens,p=0.025)
VaR(IBOV_ret_mens,p=0.05)


# Calculate the expected shortfall
ES(IBOV_ret_mens,p=.025)
ES(IBOV_ret_mens,p=.05)


# Table of drawdowns
table.Drawdowns(ret_IBOV)

# Plot of drawdowns
chart.Drawdown(ret_IBOV)