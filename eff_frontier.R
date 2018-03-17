####################################
# Fronteira Eficiente
# Claudio R. Lucinda
####################################
# Puxando os dados do IBOVESPA

library(BatchGetSymbols)
library(PortfolioAnalytics)
library(xlsx)
library(tidyverse)
library(quantmod)
library(tidyquant)

rm(list=ls())
graphics.off()
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
# dataEnv <- new.env()
# getSymbols(tickers_Yah[1:35],auto.assign=TRUE,from=datainicial,to=datafinal,env=dataEnv)
# getSymbols(tickers_Yah[37:52],auto.assign=TRUE,from=datainicial,to=datafinal,env=dataEnv)
# getSymbols(tickers_Yah[54:56],auto.assign=TRUE,from=datainicial,to=datafinal,env=dataEnv)
# getSymbols(tickers_Yah[58:62],auto.assign=TRUE,from=datainicial,to=datafinal,env=dataEnv)
# getSymbols(tickers_Yah[64],auto.assign=TRUE,from=datainicial,to=datafinal,env=dataEnv)
# plist <- eapply(dataEnv, Ad)
# IBOV_Cots <- do.call(merge, plist)

tickers_Yah_fin<-tickers_Yah[!tickers_Yah %in% c("KLBN11.SA","SAPR11.SA","TAEE11.SA","VVAR11.SA")]


##############################################
# Versão Marcelo Perlin - 100%
##############################################

df_IBOV<-BatchGetSymbols(tickers_Yah,first.date = datainicial,last.date = datafinal,thresh.bad.data = .75)
data_IBOV<-reshape.wide(df_IBOV$df.tickers)$price.close

conv_ts <- function(.df) {
  OUT<-xts(.df[,-1],order.by=.df[,1])
  return(OUT)
}

clean_na <- function(DF) {
  OUT<-DF[, sapply(DF, function(x) sum(is.na(x)))!=nrow(DF)]
  
  return(OUT)
}

remove_outliers <- function(x, .pct=.25, iqr.tresh=1.5, na.rm=TRUE, ...) {
  .pctsup<-1-.pct
  qnt <- quantile(x, probs=c(.pct, .pctsup), na.rm = na.rm, ...)
  H <- iqr.tresh * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

df_returns <- function(.df) {
  df_temp<-.df
  for (nom in colnames(.df)) {
    df_temp[,nom]<-Return.calculate(df_temp[,nom])
    
  }
  return(df_temp)
}

df_monthly <- function(.df) {
  df_0<-to.monthly(.df[,1])[,4]
  colnames(df_0)[1]<-colnames(.df)[1]
  len<-length(colnames(.df))
  for (nom in seq(2,len)) {
    df_0<-merge(df_0,to.monthly(.df[,nom])[,4])
    colnames(df_0)[nom]<-colnames(.df)[nom]
  }
  return(df_0)
}

data_IBOV<-conv_ts(data_IBOV)
data_IBOV<-remove_outliers(data_IBOV,pct=.25,iqr.tresh = 3)
data_IBOV<-clean_na(data_IBOV)
data_IBOV_Mensal<-na.omit(df_monthly(data_IBOV))



  
  
######################################
# Especificando
######################################
Sys.setlocale("LC_ALL","English")

port_spec<-portfolio.spec(colnames(data_IBOV_Mensal))
port_spec <- add.constraint(portfolio = port_spec,
                            type = "full_investment")
port_spec <- add.constraint(portfolio = port_spec,
                              type = "long_only")
port_spec <- add.objective(portfolio = port_spec,
                             type = "return",
                             name = "mean")
port_spec <- add.objective(portfolio = port_spec,
                             type = "risk",
                             name = "StdDev")

opt <- optimize.portfolio(data_IBOV_Mensal, portfolio = port_spec,
                          optimize_method = "random", search_size = 20000,
                          trace = TRUE)
opt2 <- optimize.portfolio(data_IBOV_Mensal, portfolio = port_spec,
                          optimize_method = "DEoptim",
                          trace = TRUE)

chart.RiskReward(opt2, risk.col = "StdDev", return.col = "mean",
                   chart.assets = TRUE)

chart.EfficientFrontier(opt2, match.col="StdDev", rf=.01)