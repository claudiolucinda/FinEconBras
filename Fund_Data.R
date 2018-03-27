########################################################
# Organizando dados de Fundos de Investimentos em Ações
# Para a parte de CAPM e APT
# Claudio R. Lucinda
########################################################


library(PortfolioAnalytics)
library(xlsx)
library(tidyverse)
library(openxlsx)
library(xlsReadWrite)
rm(list=ls())
graphics.off()

rm(list=ls())
graphics.off()

Raw_data<-read.xlsx("./Lucinda_Nerasti.xlsx",sheet=1,startRow=1,colNames=TRUE)
Raw_data$Date<-as.Date(Raw_data$Date,origin="1899-12-30")

Fund_Info<-read.xlsx("./Lucinda_Nerasti.xlsx",sheet=2,startRow=1,colNames=TRUE)
Fund_Info$`Data.do|Início.da.Série`<-as.Date(Fund_Info$`Data.do|Início.da.Série`,origin="1899-12-30")

########################################################
# Risk Factors - NEFIN
########################################################

download.file("http://www.nefin.com.br/Risk%20Factors/Market_Factor.xls","MKT.xls", mode="wb")
download.file("http://www.nefin.com.br/Risk%20Factors/SMB_Factor.xls", "SMB.xls", mode="wb")
download.file("http://www.nefin.com.br/Risk%20Factors/HML_Factor.xls", "HML.xls", mode="wb")
download.file("http://www.nefin.com.br/Risk%20Factors/WML_Factor.xls", "WML.xls", mode="wb")
download.file("http://www.nefin.com.br/Risk%20Factors/IML_Factor.xls", "IML.xls", mode="wb")
download.file("http://www.nefin.com.br/Risk%20Factors/Risk_Free.xls", "RF.xls", mode="wb")

MKT<-xlsx::read.xlsx("./MKT.xls", sheetName="Sheet1")
MKT$Date<-lubridate::make_date(year=MKT$year,month=MKT$month,day=MKT$day)
MKT<-MKT[,c("Rm_minus_Rf","Date")]

SMB<-xlsx::read.xlsx("./SMB.xls", sheetName="Sheet1")
SMB$Date<-lubridate::make_date(year=SMB$year,month=SMB$month,day=SMB$day)
SMB<-SMB[,c("SMB","Date")]

HML<-xlsx::read.xlsx("./HML.xls", sheetName="Sheet1")
HML$Date<-lubridate::make_date(year=HML$year,month=HML$month,day=HML$day)
HML<-HML[,c("HML","Date")]

WML<-xlsx::read.xlsx("./WML.xls", sheetName="Sheet1")
WML$Date<-lubridate::make_date(year=WML$year,month=WML$month,day=WML$day)
WML<-WML[,c("WML","Date")]

IML<-xlsx::read.xlsx("./IML.xls", sheetName="Sheet1")
IML$Date<-lubridate::make_date(year=IML$year,month=IML$month,day=IML$day)
IML<-IML[,c("IML","Date")]

RF<-xlsx::read.xlsx("./RF.xls", sheetName="Sheet1")
RF$Date<-lubridate::make_date(year=RF$year,month=RF$month,day=RF$day)
RF<-RF[,c("Risk_free","Date")]

Raw_data<-xts(Raw_data, order.by = Raw_data$Date)
Raw_data<-Raw_data[,-1]

MKT<-xts(MKT, order.by=MKT$Date)
MKT<-MKT[,-2]

SMB<-xts(SMB, order.by=SMB$Date)
SMB<-SMB[,-2]

HML<-xts(HML, order.by=HML$Date)
HML<-HML[,-2]

WML<-xts(WML, order.by=WML$Date)
WML<-WML[,-2]

IML<-xts(IML, order.by=IML$Date)
IML<-IML[,-2]

RF<-xts(RF, order.by=RF$Date)
RF<-RF[,-2]

Risk_Factors<-merge.xts(RF,MKT)
Risk_Factors<-merge.xts(Risk_Factors,SMB)
Risk_Factors<-merge.xts(Risk_Factors,HML)
Risk_Factors<-merge.xts(Risk_Factors,WML)
Risk_Factors<-merge.xts(Risk_Factors,IML)


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


df_monthly_r <- function(.df) {
  fin_mes<-endpoints(.df,on="months")
  df_0<-period.sum(.df[,1], fin_mes)
  colnames(df_0)[1]<-colnames(.df)[1]
  len<-length(colnames(.df))
  for (nom in seq(2,len)) {
    df_0<-merge(df_0,period.sum(.df[,nom],fin_mes))
    colnames(df_0)[nom]<-colnames(.df)[nom]
  }
  return(df_0)
}


storage.mode(Raw_data)<-"numeric"
Fund_Returns<-df_returns(Raw_data)

Raw_data_monthly<-df_monthly(Raw_data)
Fund_Returns_monthly<-df_returns(Raw_data_monthly)

Risk_Factors_monthly<-df_monthly_r(Risk_Factors)
indexClass(Risk_Factors_monthly)<-"yearmon"

Data_clean<-Raw_data[, -which(colMeans(is.na(Raw_data)) > 0.5)]
Fund_Returns_clean<-df_returns(Data_clean)
Fund_Info_clean<-Fund_Info[-which(colMeans(is.na(Raw_data)) > 0.5),]
Data_monthly_clean<-df_monthly(Data_clean)
Fund_Returns_monthly_clean<-df_returns(Data_monthly_clean)

saveRDS(c("Data_clean", "Fund_Returns_clean", 
          "Fund_Info_clean", "Data_monthly_clean",
          "Fund_Returns_monthly_clean", "Risk_Factors", "Risk_Factors_monthly"),file="Fund_Data.RDS")
