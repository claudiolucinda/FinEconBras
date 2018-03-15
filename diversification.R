########################################
# Diversification
# Claudio R. Lucinda
########################################

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

# teste<-combn(colnames(data_IBOV),2,simplify = FALSE)
# teste2<-na.omit(data_IBOV[,teste[[1]]])
# testewgts<-rep((1/2), each=2)
# 
# returns<-data.frame(lapply(teste2,Return.calculate))[-1,]
# data_02<-rownames(returns)
# returns<-xts(returns,order.by=as.Date(data_02, format="%Y-%m-%d"))
# carteira_rebal<-Return.portfolio(returns,weights=testewgts, rebalance_on="months",verbose=TRUE)

#Retornos Médios - Aritméticos
mean(carteira_rebal$returns,na.rm=T)
sd(carteira_rebal$returns)

inner2 <- function(.x,.df,.wts,.pb=NULL) {
  if (.pb$i < .pb$n) .pb$tick()$print()
  teste2<-na.omit(.df[,.x])
  returns<-data.frame(lapply(teste2,Return.calculate))[-1,]
  data_02<-rownames(returns)
  returns<-xts(returns,order.by=as.Date(data_02, format="%Y-%m-%d"))
  carteira_rebal<-Return.portfolio(returns,weights=.wts, rebalance_on="months",verbose=FALSE)
  out<-sd(carteira_rebal$portfolio.returns)
  return(out)
}

inner <- function(.n,.df) {
  teste<-combn(colnames(.df),.n,simplify = FALSE)
  testewgts<-rep((1/.n), each=.n)
  pb<-progress_estimated(length(teste),0)
  ports<-plyr::ldply(teste, function(x) inner2(x,.df=.df,.wts=testewgts,.pb))
  med_sd<-mean(ports$V1)
  out<-data.frame(.n,med_sd)
  return(out)
}

outer <- function(.df) {
  max_ports<-5
  out<-plyr::ldply(seq(2,max_ports,1),function(x) inner(x,.df=.df))
  return(out)
  
}

teste<-outer(data_IBOV)
