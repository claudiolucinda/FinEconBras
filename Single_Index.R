#########################################
# Modelo de Índice Único
# Claudio R. Lucinda
# FEA-RP/USP
# Bibliografia: BKM, cap. 8
#########################################
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

Data_fig<-as.zoo(Data_all[,c("Fundo_280","Rm")])
# Set a color scheme:
tsRainbow <- rainbow(ncol(Data_fig))
plot(x = Data_fig, ylab = "Retornos", main = "Fundo 280 versus Mercado", 
     col = tsRainbow, screens = 1)
legend(x = "topleft", legend = c("Fundo 280", "Mercado"), 
       lty = 1,col = tsRainbow)

# Regressão
model_280<-lm(Fundo_280~Rm, data=Data_fig)
summary(model_280)
confint(model_280)

# Scatter com reta
plot.zoo(Data_fig$Rm,Data_fig$Fundo_280,
         ylab="Fundo 280",xlab="Rm")
abline(lm(Fundo_280~Rm, data=Data_fig), col="red")
grid(nx=NULL, ny=NULL)

# Fazer o loop com os dados
#fund_list_names<-list(colnames(Fund_Returns_clean))

betas<-ldply(as.list(fund_list_names), function(.x) coef(lm(get(.x)~Rm, data=Data_all))[2])
sigmas2<-ldply(as.list(fund_list_names), function(.x) sigma(lm(get(.x)~Rm, data=Data_all))^2)
sigma2_Rm<-as.matrix(var(Data_all$Rm))

betas_vec<-as.matrix(betas)
sigmas2_vec<-as.matrix(sigmas2)

cov.mkt<-sigma2_Rm[1,1]*(betas_vec%*%t(betas_vec))
D.matrix<-matrix(0,nrow=nrow(cov.mkt),ncol=ncol(cov.mkt))

for (i  in 1:ncol(cov.mkt)) {
  D.matrix[i,i]<-sigmas2_vec[i,1]
}

cov.si<-cov.mkt+D.matrix
cov.hist<-cov(Fund_Returns_clean)

cov.si[1:4,1:4]
cov.hist[1:4,1:4]

is.positive.definite(cov.si)
is.positive.definite(cov.hist)

# Estabilidade dos Betas
rr <- rollapply(Data_fig, width = 252,
                FUN = function(z) coef(lm(Fundo_280 ~ Rm, data = as.data.frame(z))),
                by.column = FALSE, align = "right")

plot(rr[,2],ylab="Fundo 280", xlab="Data", main="Estabilidade do beta")
abline(h=coef(model_280)[2], col="red")
grid(nx=NULL, ny=NULL)


#################################################
# Modelo de Fator Único -- Abordagem Estatística
#################################################

stat.fact<-statistical.factor.model(Fund_Returns_clean,k=1)

stat.fact.data<-stat.fact$factor_realizations
stat.fact.betas<-stat.fact$factor_loadings

cov.si.stat.fact<-extractCovariance(stat.fact)
cov.si.stat.fact[1:4,1:4]
cov.hist[1:4,1:4]

is.positive.definite(cov.si.stat.fact)
