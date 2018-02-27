###################################################
# Código Inicial de entrada dos dados
# Claudio R. Lucinda
# FEA-RP/USP
###################################################

# install.packages("PortfolioAnalytics")
# install.packages("xlsx")
library(PortfolioAnalytics)
library(xlsx)
library(tidyverse)

rm(list=ls())
########################################################
# Miolo de Funções
########################################################
conv_ts <- function(.df) {
  OUT<-xts(.df[,-1],order.by=.df[,1])
  return(OUT)
}




# Importando os dados
PETR4<-read.xlsx("./Data01.xlsx",sheetName="PETR4")
BBAS3<-read.xlsx("./Data01.xlsx",sheetName="BBAS3")
IBOV<-read.xlsx("./Data01.xlsx",sheetName="IBOV")
CDI<-read.xlsx("./Data01.xlsx",sheetName="CDI")
IMA<-read.xlsx("./Data01.xlsx",sheetName="IMA")
PETR4[,3:4]<-NULL
PETR4<-PETR4[!is.na(PETR4$Data),]



# Transformando para ts
PETR4<-conv_ts(PETR4)
BBAS3<-conv_ts(BBAS3)
IBOV<-conv_ts(IBOV)
CDI<-conv_ts(CDI)
IMA<-conv_ts(IMA)

# Ações Individuais
ret_PETR4<-Return.calculate(PETR4)[-1,]
ret_BBAS3<-Return.calculate(BBAS3)[-1,]
ret_IBOV<-Return.calculate(IBOV)[-1,]
ret_CDI<-Return.calculate(CDI)[-1,]
ret_IMA<-Return.calculate(IMA)[-1,]

# Criando uma grade
grid <- seq(from = 0, to = 1, by = .01)

# Ineficiente, mas serve. Inicializando um vetor para os sharpe ratios
vsharpe <- rep(NA, times = length(grid))

# Loop
for(i in 1:length(grid)) {
  weight <- grid[i]
  preturns <- weight * ret_IBOV + (1 - weight) * ret_IMA
  vsharpe[i] <- SharpeRatio.annualized(preturns)
}

# Plot weights and Sharpe ratio
plot(grid, vsharpe, xlab = "Pesos", ylab= "Ann. Sharpe ratio")
abline(v = grid[vsharpe == max(vsharpe)], lty = 3)

######################################################
# Correlação


# Merge returns_equities and returns_bonds 
returns <- merge(ret_IBOV, ret_IMA)

# Find and visualize the correlation using chart.Correlation
chart.Correlation(returns)

# Visualize the rolling estimates using chart.RollingCorrelation
chart.RollingCorrelation(ret_IBOV, ret_IMA, width = 24)


###################################################
# Portfolio mais amplo
# Merge returns_equities and returns_bonds 
returns <- merge(returns, ret_BBAS3)
returns <- na.omit(merge(returns, ret_PETR4))

# Create a vector of returns 
means <- apply(returns, 2, "mean")

# Create a vector of standard deviation
sds<-apply(returns,2,"sd")

# Create a scatter plot
plot(sds, means)
text(sds, means, labels = colnames(returns), cex = 0.7)
abline(h = 0, lty = 3)

# Create a matrix with variances on the diagonal
diag_cov<-diag(sds^2)

# Create a covariance matrix of returns
cov_matrix<-cov(returns)

# Create a correlation matrix of returns
cor_matrix<-cor(returns)

weights<-rep(.25,ncol(returns))
# Create a weight matrix w
w<-as.matrix(weights)

# Create a matrix of returns
mu<-as.matrix(means)

sigma<-as.matrix(cov_matrix)

# Calculate portfolio mean monthly returns
t(w)%*%mu

# Calculate portfolio volatility
sqrt(t(w) %*% sigma %*% w)

# Create portfolio weights
weights<-c(.4,.4,.1,.1)

# Create volatility budget
vol_budget <- StdDev(returns, portfolio_method = "component", weights = weights)

# Make a table of weights and risk contribution
weights_percrisk<-cbind(weights,vol_budget$pct_contrib_StdDev)
colnames(weights_percrisk) <- c("weights", "perc vol contrib")

# Print the table
(weights_percrisk)
