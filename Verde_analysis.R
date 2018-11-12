##############################################
# Style Analysis pt.01
# Defining the Factor Data
# Claudio R. Lucinda
##############################################
# please first install SIT.date
#devtools::install_github('systematicinvestor/SIT.date')
# 
#library(curl)
#curl_download('https://github.com/systematicinvestor/SIT/raw/master/SIT.tar.gz', 'sit',mode = 'wb',quiet=T)
#install.packages('sit', repos = NULL, type='source')
load.packages('quadprog')

library(plyr)
library(xlsx)
library(tidyverse)
library(quantmod)
library(tidyquant)
library(PortfolioAnalytics)
library("SIT")

rm(list=ls())
graphics.off()

load("Fund_Data.RDS")
load("Style_Data.RDS")

Verde_data<-read.xlsx("./CSHG Verde FICFI Mult.xlsx",sheetName="040177",stringsAsFactors=F)

Verde_data<-xts(Verde_data,order.by=Verde_data$Data)[,-1]
storage.mode(Verde_data)<-"numeric"
Verde_returns<-Return.calculate(Verde_data)[-1,]

Fund_Returns_clean<-na.omit(Fund_Returns_clean)
fund_list_names<-colnames(Fund_Returns_clean)

Data_all<-merge(Verde_returns,Risk_Factors, join="inner")
Data_all$Rm<-Data_all$Rm_minus_Rf+Data_all$Risk_free
Data_all$Rm_minus_Rf_Sq<-Data_all$Rm_minus_Rf^2
Data_all$Rm_minus_Rf_abs<-ifelse(Data_all$Rm_minus_Rf>0,Data_all$Rm_minus_Rf,0)
Data_all$Ex_ret<-Data_all$Cota-Data_all$Risk_free
# Mkt Timing


modelo_Verde<-lm(Ex_ret~Rm_minus_Rf+Rm_minus_Rf_Sq, data=Data_all)

summary(modelo_Verde)

modelo_Verde2<-lm(Ex_ret~Rm_minus_Rf+Rm_minus_Rf_abs, data=Data_all)

summary(modelo_Verde2)

data_graph<-Data_all[,c("Cota","Rm", "Risk_free")]
Ret_cum<-Return.cumulative(data_graph)
charts.PerformanceSummary(R=data_graph, Rf=Ret_cum$Risk_free, main="CSHG Verde FICFI Mult")
######################################################
# Calculando as medidas de desempenho pro Fundo_280
######################################################

#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

(Modigliani(Data_all$Cota,Data_all$Rm,Rf=mean(Data_all$Risk_free)))
(SharpeRatio(Data_all$Cota,Rf=mean(Data_all$Risk_free),FUN="StdDev"))
(TreynorRatio(Data_all$Cota,Data_all$Rm,Rf=mean(Data_all$Risk_free)))
# Esse é o Information Ratio do BKM
(AppraisalRatio(Data_all$Cota,Data_all$Rm))
(TrackingError(Data_all$Cota,Data_all$Rm))

# Information Ratio 
(InformationRatio(Data_all$Cota,Data_all$Rm))


#############################################
# Alpha & Factor structure
#############################################
modelo_01<-lm(Ex_ret~Rm_minus_Rf, data=Data_all)
modelo_02<-lm(Ex_ret~Rm_minus_Rf+SMB, data=Data_all)
modelo_03<-lm(Ex_ret~Rm_minus_Rf+SMB+HML, data=Data_all)
modelo_04<-lm(Ex_ret~Rm_minus_Rf+SMB+HML+WML, data=Data_all)
modelo_05<-lm(Ex_ret~Rm_minus_Rf+SMB+HML+WML+IML, data=Data_all)

ann_alpha<-function(x) (x*25200)
stargazer(modelo_01,modelo_02,modelo_03,modelo_04,modelo_05,
          align=TRUE, keep="Constant",apply.coef=ann_alpha, apply.se=ann_alpha,
          keep.stat = "rsq", column.labels = c("MKT", "SMB", "HML", "WML", "HML"),
          dep.var.caption = "")
#############################################
# Style Analysis
#############################################


hist.returns<-merge(Data_all$Risk_free,Factor_Returns[,c("DOL","IMA_Plus","IDA_DI","IDA_Geral","Size1","Size2","Size2_BM1","Size2_BM2")])
#hist.returns<-na.omit(merge(Data_all[,"Fundo_280"],hist.returns))
hist.returns<-na.omit(merge(Data_all[,"Cota"],hist.returns))

# setup
ndates = nrow(hist.returns)
n = ncol(hist.returns)-1
window.len = 48

style.weights = hist.returns[, -1]
style.weights[] = NA
style.r.squared = hist.returns[, 1]
style.r.squared[] = NA

# main loop
for( i in window.len:ndates ) {
  window.index = (i - window.len + 1) : i
  
  fit = lm.constraint( hist.returns[window.index, -1], hist.returns[window.index, 1] )    
  style.weights[i,] = fit$coefficients
  style.r.squared[i,] = fit$r.squared
}

# plot 
SIT:::aa.style.summary.plot('Style UnConstrained', style.weights, style.r.squared, window.len)


window.len = 96

style.weights[] = NA
style.r.squared[] = NA

# Setup constraints
# 0 <= x.i <= 1
constraints = new.constraints(n, lb = 0, ub = 1)

# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)        

# main loop
for( i in window.len:ndates ) {
  window.index = (i - window.len + 1) : i
  
  fit = lm.constraint( hist.returns[window.index, -1], hist.returns[window.index, 1], constraints )   
  style.weights[i,] = fit$coefficients
  style.r.squared[i,] = fit$r.squared
}

# plot  
SIT:::aa.style.summary.plot('Style Constrained', style.weights, style.r.squared, window.len)

