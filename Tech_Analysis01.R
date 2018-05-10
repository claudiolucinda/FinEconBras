#################################
# Análise Técnica
# Claudio R. Lucinda
# FEA-RP/USP
#################################

library(BatchGetSymbols)
library(PortfolioAnalytics)
library(xlsx)
library(tidyverse)
library(quantmod)
library(tidyquant)

rm(list=ls())
graphics.off()

datainicial<-"2014-01-01"
datafinal<-"2018-02-23"

getSymbols("PETR4.SA",auto.assign=TRUE,from=datainicial,to=datafinal)

# Gráficos Técnicos Básicos

chartSeries(PETR4.SA["2017/2018"])

# first some high-low-close style bars, monochromatic theme 
barChart(PETR4.SA["2017/2018"],theme='white.mono',bar.type='hlc')


# how about some candles, this time with color 
candleChart(PETR4.SA["2017/2018"],multi.col=TRUE,theme='white')


# and now a line, with the default color scheme 
lineChart(PETR4.SA["2017/2018"],line.type='h',TA=NULL)


########################################
# Indicadores Técnicos
########################################

# Jeito de uma vez só
chartSeries(PETR4.SA["2017/2018"], theme="white",TA="addVo();addBBands();addCCI()")

# Jeito sequencial
chartSeries(PETR4.SA["2017/2018"], theme="white") #draw the chart 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index

chartSeries(PETR4.SA["2017/2018"], theme="white") #draw the chart 
addSMA(n=10)
addSMA(n=50)


# Criando um indicador Técnico
chartSeries(PETR4.SA["2017/2018"], TA=NULL)
addTA(OpCl(PETR4.SA["2017/2018"]),col='blue', type='h')

# Using newTA it is possible to create your own 
# generic TA function --- let's call it addOpCl 
# 
addOpCl <- newTA(OpCl,col='green',type='h') 

addOpCl() 

# Lista de Indicadores
# Indicator	TTR Name	quantmod Name
# Welles Wilder's Directional Movement Indicator	ADX	addADX
# Average True Range	ATR	addATR
# Bollinger Bands	BBands	addBBands
# Bollinger Band Width	N/A	addBBands
# Bollinger %b	N/A	addBBands
# Commodity Channel Index	CCI	addCCI
# Chaiken Money Flow	CMF	addCMF
# Chande Momentum Oscillator	CMO	addCMO
# Double Exponential Moving Average	DEMA	addDEMA
# Detrended Price Oscillator	DPO	addDPO
# Exponential Moving Average	EMA	addEMA
# Price Envelope	N/A	addEnvelope
# Exponential Volume Weigthed Moving Average	EVWMA	addEVWMA
# Options and Futures Expiration	N/A	addExpiry
# Moving Average Convergence Divergence	MACD	addMACD
# Momentum	momentum	addMomentum
# Rate of Change	ROC	addROC
# Relative Strength Indicator	RSI	addRSI
# Parabolic Stop and Reverse	SAR	addSAR
# Simple Moving Average	SMA	addSMA
# Stocastic Momentum Index	SMI	addSMI
# Triple Smoothed Exponential Oscillator	TRIX	addTRIX
# Volume	N/A	addVo
# Weighted Moving Average	WMA	addWMA
# Williams %R	WPR	addWPR
# ZLEMA	ZLEMA	addZLEMA