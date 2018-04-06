####################################
# Fronteira Eficiente
# Claudio R. Lucinda
####################################
# Puxando os dados do IBOVESPA

#install.packages("ROI")
#install.packages("ROI.plugin.quadprog")
#install.packages("ROI.plugin.glpk")
install.packages("GenSA")

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

#opt <- optimize.portfolio(IBOV_Returns_Final, portfolio = port_spec,
#                          optimize_method = "random", search_size = 20000,
#                          trace = TRUE)
# opt2 <- optimize.portfolio(IBOV_Returns_Final, portfolio = port_spec,
#                           optimize_method = "GenSA",
#                           trace = TRUE, maxSR=TRUE)

opt3 <- optimize.portfolio(IBOV_Returns_Final, portfolio = port_spec,
                           optimize_method = "ROI",
                           trace = TRUE, maxSR=TRUE, message=TRUE)

chart.EfficientFrontier(opt3,match.col = "StdDev", 
                        n.portfolios = 25, 
                        xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, 
                        element.color = "darkgray",
                        main = "Efficient Frontier",
                        RAR.text = "SR", rf = 0.01, 
                        tangent.line = TRUE, 
                        cex.legend = 0.8,
                        chart.assets = TRUE, 
                        labels.assets = TRUE, 
                        pch.assets = 21,
                        cex.assets = 0.8)



port_spec2 <- portfolio.spec(colnames(IBOV_Returns_Final))
port_spec2 <- add.constraint(portfolio = port_spec2,
                            type = "full_investment")
port_spec2 <- add.constraint(portfolio = port_spec2,
                            type = "long_only")
port_spec2 <- add.objective(portfolio = port_spec2,
                           type = "return",
                           name = "mean")
port_spec2 <- add.objective(portfolio = port_spec2,
                           type = "risk",
                           name = "StdDev", risk_aversion=4)

opt4 <- optimize.portfolio(IBOV_Returns_Final, portfolio = port_spec2,
                           optimize_method = "ROI",
                           trace = TRUE, message=TRUE)


# Extraindo as infos das Fronteiras Eficientes
meanvar.ef<-create.EfficientFrontier(R=IBOV_Returns_Final,portfolio = port_spec,type="mean-StdDev", n.portfolios = 250)
meanvar.ef
summary(meanvar.ef, digits=2)

chart.EF.Weights(meanvar.ef, colorset = NULL,
                 n.portfolios = 25, by.groups = FALSE, match.col = "StdDev", main = "",
                 cex.lab = 0.8, cex.axis = 0.8, cex.legend = 0.25, legend.labels = NULL,
                 element.color = "darkgray", legend.loc = "right")

#########################################
# Backtesting
#########################################

port_spec3 <- portfolio.spec(colnames(IBOV_Returns_Final))
port_spec3 <- add.constraint(portfolio = port_spec3,
                             type = "full_investment")
port_spec3 <- add.constraint(portfolio = port_spec3,
                             type = "box", min=-.05, max=.1)
port_spec3 <- add.objective(portfolio = port_spec3,
                            type = "return",
                            name = "mean")
port_spec3 <- add.objective(portfolio = port_spec3,
                            type = "risk",
                            name = "StdDev", risk_aversion=4)

opt3_r <- optimize.portfolio.rebalancing(IBOV_Returns_Final, portfolio = port_spec3,
                           optimize_method = "random", rebalance_on = "years", training_period = 48,
                           trace = TRUE, message=TRUE)
