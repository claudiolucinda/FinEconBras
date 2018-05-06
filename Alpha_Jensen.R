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



coefs<-ldply(as.list(fund_list_names), function(.x) coef(lm(get(.x)~Rm_minus_Rf+SMB+WML+HML+IML, data=Fund_ex_returns)))

alpha_anual<-coefs$`(Intercept)`*252

chart.Histogram(alpha_anual,methods=c("add.density",
                                    "add.normal"))
