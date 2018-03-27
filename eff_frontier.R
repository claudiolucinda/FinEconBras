####################################
# Fronteira Eficiente
# Claudio R. Lucinda
####################################
# Puxando os dados do IBOVESPA

#install.packages("ROI")
#install.packages("ROI.plugin.quadprog")
#install.packages("ROI.plugin.glpk")

library(BatchGetSymbols)
library(PortfolioAnalytics)
library(xlsx)
library(tidyverse)
library(quantmod)
library(tidyquant)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)

rm(list=ls())
graphics.off()

load("IbovData.RDS")


######################################
# Especificando
######################################
Sys.setlocale("LC_ALL","English")

port_spec<-portfolio.spec(colnames(IBOV_Returns_Final))
port_spec <- add.constraint(portfolio = port_spec,
                            type = "full_investment")
port_spec <- add.constraint(portfolio = port_spec,
                              type = "long_only")
port_spec <- add.objective(portfolio = port_spec,
                             type = "return",
                             name = "mean")
port_spec <- add.objective(portfolio = port_spec,
                             type = "risk",
                             name = "StdDev")

opt <- optimize.portfolio(IBOV_Returns_Final, portfolio = port_spec,
                          optimize_method = "random", search_size = 20000,
                          trace = TRUE)
opt2 <- optimize.portfolio(IBOV_Returns_Final, portfolio = port_spec,
                          optimize_method = "DEOptim",
                          trace = FALSE)
meanvar.ef<-create.EfficientFrontier(R=IBOV_Returns_Final,portfolio = port_spec,type="mean-StdDev", n.portfolios = 250)
meanvar.ef
summary(meanvar.ef, digits=2)
fronteira<-as.data.frame(meanvar.ef$frontier[,1:2])
#meanvar.ef$frontier
png("RandomPort.png")
chart.RiskReward(opt, risk.col = "StdDev", return.col = "mean",
                 chart.assets = TRUE, xlim=c(0,.4))
points(fronteira[,2],fronteira[,1], col="red")
dev.off()

# Vou sobrepor duas fronteiras eficientes - com e sem vendas a descoberto

port_spec2<-portfolio.spec(colnames(IBOV_Returns_Final))
port_spec2 <- add.constraint(portfolio = port_spec2,
                            type = "full_investment")
port_spec2 <- add.objective(portfolio = port_spec2,
                           type = "return",
                           name = "mean")
port_spec2 <- add.objective(portfolio = port_spec2,
                           type = "risk",
                           name = "StdDev")

opt22 <- optimize.portfolio(R=IBOV_Returns_Final, portfolio = port_spec2,
                           optimize_method = "DEOptim",
                           trace = FALSE)

portf.list<-combine.portfolios(list(port_spec, port_spec2))
legend.labels <- c("Long Only", "Long+Short")
chart.EfficientFrontierOverlay(R=IBOV_Returns_Final, portfolio_list=portf.list, type="mean-StdDev", 
                               match.col="StdDev", legend.loc="topleft", 
                               legend.labels=legend.labels, cex.legend=0.6,
                               labels.assets=FALSE, pch.assets=18)


###############################################
# Entendendo a Otimização das carteiras
###############################################
print(opt22)

opt2_rebal<-optimize.portfolio.rebalancing(R=IBOV_Returns_Final, portfolio = port_spec2,
                               optimize_method = "DEOptim", rebalance_on="years", training_period = 12,
                               trace = FALSE)

