###########################################
# Aplicando o APT
# Claudio R. Lucinda
# FEA-RP/USP
###########################################

library(PortfolioAnalytics)
library(xlsx)
library(plyr)
library(tidyverse)
library(openxlsx)
library(xlsReadWrite)
library(matrixcalc)
library(xts)
rm(list=ls())
graphics.off()

load("Fund_Data.RDS")

# Dados diários de outubro de 2007 em diante

Fund_Returns_clean<-na.omit(Fund_Returns_clean)
fund_list_names<-colnames(Fund_Returns_clean)

Data_all<-merge(Fund_Returns_clean,Risk_Factors, join="inner")
Data_all$Rm<-Data_all$Rm_minus_Rf+Data_all$Risk_free

Fund_ex_returns<-Data_all
for (i in 1:60) {
  Fund_ex_returns[,i]<-Data_all[,i]-Data_all[,61]
}  

plot.zoo(Fund_ex_returns$SMB)
modelo_fundo_280<-lm(Fundo_280~Rm_minus_Rf+SMB+WML+HML+IML, data=Fund_ex_returns)

summary(modelo_fundo_280)


coefs<-ldply(as.list(fund_list_names), function(.x) coef(lm(get(.x)~Rm_minus_Rf+SMB+WML+HML+IML, data=Fund_ex_returns)))
mean_returns<-as.data.frame(sapply(Fund_ex_returns,mean)[1:60])
colnames(mean_returns)<-"E_pr"

coefs$key<-1:60
mean_returns$key<-1:60

data_teste1<-merge(coefs, mean_returns, by="key")
colnames(data_teste1)[2]<-"Cte"

Premios_Risco<-lm(E_pr~Rm_minus_Rf+WML+HML+SMB+IML, data=data_teste1)

summary(Premios_Risco)
