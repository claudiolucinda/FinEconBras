#########################################################
# Baixando os dados das ações componentes do IBOVESPA
# Claudio R. Lucinda
#########################################################
install.packages("quantmod")
library(PortfolioAnalytics)
library(xlsx)
library(tidyverse)
library(quantmod)

rm(list=ls())
# Importando os dados
IBOV_Data<-read.xlsx("./Comp_IBOV.xlsx",sheetName="IBOV",stringsAsFactors=F)
tickers<-data.frame(IBOV_Data$Ticker,stringsAsFactors=FALSE)
suffix <- data.frame(replicate(nrow(tickers), ".SA"),stringsAsFactors=F)
tickers_Yah<-paste0(tickers[[1]],".SA")[1:64]

datainicial<-"2004-01-01"
datafinal<-"2018-02-23"

# Empresas que não consigo baixar no Yahoo Finance
# KLBN11
# SAPR11
# TAEE11
# VVAR11
getSymbols(tickers_Yah[1:35],auto.assign=TRUE,from=datainicial,to=datafinal)
getSymbols(tickers_Yah[37:52],auto.assign=TRUE,from=datainicial,to=datafinal)
getSymbols(tickers_Yah[54:56],auto.assign=TRUE,from=datainicial,to=datafinal)
getSymbols(tickers_Yah[58:62],auto.assign=TRUE,from=datainicial,to=datafinal)
getSymbols(tickers_Yah[64],auto.assign=TRUE,from=datainicial,to=datafinal)

tickers_Yah_fin<-tickers_Yah[!tickers_Yah %in% c("KLBN11.SA","SAPR11.SA","TAEE11.SA","VVAR11.SA")]

###############################################
# Funções para automatizar as coisas
###############################################

gen_ret <- function(.xts_frame) {
  y<-assign(.xts_frame)
  x<-Return.calculate(y[,6])[-1,]
}

Returns<-map_df(tickers_Yah_fin,gen_ret)

for (.df in tickers_Yah_fin) {
  cat(.df)
  x<-gen_ret(.df)
  names(x)<-paste0("ret_",.df)
}