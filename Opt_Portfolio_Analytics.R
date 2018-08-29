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

load("Style_Data.RDS")

data_r<-Factor_Returns

riskfree = 0.0004

data_r<-data_r[,!colnames(data_r) %in% c("Size1", "Size2","Size3", "IDA_DI", "IDA_Geral")]


######################################
# Especificando
######################################
Sys.setlocale("LC_ALL","English")

port_spec<-portfolio.spec(colnames(data_r))
port_spec <- add.constraint(portfolio = port_spec,
                            type = "full_investment")
port_spec <- add.objective(portfolio = port_spec,
                             type = "return",
                             name = "mean")
port_spec <- add.objective(portfolio = port_spec,
                             type = "risk",
                             name = "StdDev")


opt3 <- optimize.portfolio(data_r, portfolio = port_spec,
                           optimize_method = "ROI",
                           trace = TRUE, maxSR=TRUE, message=TRUE)

chart.EfficientFrontier(opt3,match.col = "StdDev", 
                        n.portfolios = 25, 
                        xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, 
                        element.color = "darkgray",
                        main = "Efficient Frontier",
                        RAR.text = "SR", rf = riskfree, 
                        tangent.line = TRUE, 
                        cex.legend = 0.8,
                        chart.assets = TRUE, 
                        labels.assets = TRUE, 
                        pch.assets = 21,
                        cex.assets = 0.8)
chart.Weights(opt3)

opt4 <- optimize.portfolio.rebalancing(data_r, portfolio = port_spec,
                           optimize_method = "ROI",
                           trace = TRUE, maxSR=TRUE, message=TRUE, rebalance_on = "quarters")

chart.Weights(opt4)

port_spec.ES<-portfolio.spec(colnames(data_r))
port_spec.ES <- add.constraint(port_spec.ES, type="weight_sum",
                               min_sum=0.99, max_sum=1.01)
port_spec.ES <- add.objective(port_spec.ES, type="return",
                              name="mean", multiplier=0)
port_spec.ES <- add.objective(port_spec.ES, type="risk", name="ES")

opt.minES <- optimize.portfolio(data_r, port_spec.ES, 
                                optimize_method="ROI")

chart.RiskBudget(opt.minES, risk.type = "percentage", match.col="ES")
