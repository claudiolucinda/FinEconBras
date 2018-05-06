#######################################
# Dados Curva de Juros
# Claudio R. Lucinda
# FEA-RP/USP
#######################################
library(BatchGetSymbols)
library(PortfolioAnalytics)
library(xlsx)
library(tidyverse)
library(quantmod)
library(tidyquant)

rm(list=ls())
graphics.off()

ytm_data <- read.csv("ytm_data.csv", stringsAsFactors = F, sep=";")
ytm_data$data<-as.Date(paste(ytm_data$year, ytm_data$month, ytm_data$day, sep='-'))
ytm_data[,c("year","month","day")]<-NULL

ytm_data<-xts(ytm_data,order.by=ytm_data$data)
ytm_data$data<-NULL

save(ytm_data,file="YTM_data.RDS")
