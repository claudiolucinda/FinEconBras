###########################################################
# Funções para Precificar Títulos de Dívida Brasileiros
# Claudio R. Lucinda
# FEA-RP/USP
############################################################
#install.packages("bizdays")
#install.packages("FinancialMath")
library(bizdays)
library(BatchGetSymbols)
library(PortfolioAnalytics)
library(xlsx)
library(tidyverse)
library(quantmod)
library(tidyquant)
library(lubridate)
library(FinancialMath)

rm(list=ls())
graphics.off()


data(holidaysANBIMA, package = 'bizdays')
range(holidaysANBIMA)
create.calendar(name='MyCalendar', holidays=holidaysANBIMA, weekdays=c('sunday', 'saturday'),
                adjust.from=adjust.next, adjust.to=adjust.previous)
# is.bizday('2016-07-12', 'MyCalendar')
# following('2016-09-07', 'MyCalendar')
# bizdays('2016-07-12', '2016-10-16', 'MyCalendar')

cupom<-0.1
VF<-1000
YTM<-0.098
data_hoje<-Sys.Date()
data_futura<-as.Date("2029-01-01")

x<-interval(data_hoje,data_futura)
n_sem<- x %/% months(6)

data_futura0<-data_futura

events<-list(adjust.next(data_futura0))
for(i in 1:n_sem) {
  data_futura0<-data_futura0-months(6)
  idx<-i+1
  events[[idx]]<-adjust.next(data_futura0)
}

datas<-plyr::ldply(events,function(.x) bizdays(data_hoje,.x,'MyCalendar')+1)
datas$CF<-VF*((1+cupom)^.5 -1)
datas$CF[1]<-datas$CF[1]+VF
datas$IRPF<-.225
datas$IRPF[datas$V1 %in% 180:360]<-.2
datas$IRPF[datas$V1 %in% 360:720]<-.175
datas$IRPF[datas$V1 >720 ]<-.15
datas$PV<-datas$CF/((1+YTM)^(datas$V1/252))
datas$weights<-datas$PV/sum(datas$PV)
duration<-sum(datas$weights*datas$V1)
datas$int_cal<-datas$PV*(datas$V1^2+datas$V1)
convexity<-(1/(sum(datas$PV)*(1+YTM^2)))*sum(datas$int_cal)

YTM<-IRR(cf0=-1045.91,cf=datas$CF, times=datas$V1)


# cf <- c(-10000, 1300, -1200, 12000) 
# npv <- function(i, cf, t=seq(along=cf)) sum(cf/(1+i)^t) 
# irr <- function(cf) { uniroot(npv, c(0,1), cf=cf)$root } 
# irr(cf)
# [1] 0.0686
# irrinpercent<- irr(cf)*100
# [1] 6.86