########################################
# Diversification
# Claudio R. Lucinda
########################################

#library(doParallel)
library(BatchGetSymbols)
library(PortfolioAnalytics)
library(xlsx)
library(tidyverse)
library(quantmod)
#registerDoParallel()

rm(list=ls())
graphics.off()

load("IbovData.RDS")

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
  teste_menor<-sample(teste,100)
  testewgts<-rep((1/.n), each=.n)
  cat(paste0("Portfolios with ",.n," assets"))
  pb<-dplyr::progress_estimated(length(teste_menor),0)
  ports<-plyr::ldply(teste_menor, function(x) inner2(x,.df=.df,.wts=testewgts,.pb=pb))
  #ports<-plyr::ldply(teste, inner2,.df=.df,.wts=testewgts,.pb=pb,.parallel = TRUE,.paropts=list(.export=c("inner2"),.packages=c("xts","PortfolioAnalytics")))
  med_sd<-mean(ports$V1)
  out<-data.frame(.n,med_sd)
  return(out)
}

outer <- function(.df,.min,.max) {
  out<-plyr::ldply(seq(.min,.max,1),function(x) inner(x,.df=.df))
  return(out)
  
}

teste<-outer(data_IBOV,2,8)
teste2<-outer(data_IBOV,30,35)

Port_div<-rbind(teste,teste2)
p<-ggplot(data=Port_div, aes(x=.n, y=med_sd)) +
  geom_line() + ggtitle("Diversificação em Carteiras") +
  labs(x="Nro. de Ativos", y="Desvio-Padrão dos Retornos")

png("Diversif.png")
p
dev.off()