##############################################
# Claudio R. Lucinda
# Portfolio Allocation with Matrix Algebra
# Claudio R. Lucinda
# FEA/USP
##############################################


library(plyr)
library(xlsx)
library(tidyverse)
library(quantmod)
library(tidyquant)
library(PortfolioAnalytics)
library("SIT")

rm(list=ls())
graphics.off()

load("Style_Data.RDS")

data_r<-Factor_Returns

data_r<-data_r[,!colnames(data_r) %in% c("Size1", "Size2","Size3", "IDA_DI", "IDA_Geral")]

asset.names<-colnames(data_r)
mu.vec<-colMeans(data_r)
sigma.mat<-cov(data_r,use="complete.obs")
n.asset<-length(mu.vec)
sd.vec = sqrt(diag(sigma.mat))
plot(sd.vec, mu.vec,  ylim=c(2e-4, 8e-4), xlim=c(0, 0.015), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=16, col="blue", cex=2.5, cex.lab=1.75)     
text(sd.vec, mu.vec, labels=asset.names, pos=4, cex = 1)

#
# equally weighted portfolio
#
x.vec = rep(1,n.asset)/n.asset
names(x.vec) = asset.names
sum(x.vec)
mu.p.x = crossprod(x.vec,mu.vec)
sig2.p.x = t(x.vec)%*%sigma.mat%*%x.vec
sig.p.x = sqrt(sig2.p.x)
mu.p.x
sig.p.x

# long-short portfolio
y.vec = c(0.3, 0.3, -0.2, 0.4, 0.4, -0.2)
names(y.vec) = asset.names
sum(y.vec)
mu.p.y = crossprod(y.vec,mu.vec)
sig2.p.y = t(y.vec)%*%sigma.mat%*%y.vec
sig.p.y = sqrt(sig2.p.y)
mu.p.y
sig.p.y

# covariance and correlation between equally weighted and long-short portfolios
sig.xy = t(x.vec)%*%sigma.mat%*%y.vec
sig.xy
rho.xy = sig.xy/(sig.p.x*sig.p.y)
rho.xy

#
# show assets and portfolios in mean-sd space
#
cex.val = 1.5
plot(sd.vec, mu.vec,  ylim=c(2e-4, 8e-4), xlim=c(0, 0.015), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=16, col="blue", cex=2.5, cex.lab=1.75)     
text(sd.vec, mu.vec, labels=asset.names, pos=4, cex = 1)
points(sig.p.x, mu.p.x, pch=16, col="black", cex=2.5)
text(sig.p.x, mu.p.x, labels="EQUAL WEIGHT", pos=4, cex = 1)
points(sig.p.y, mu.p.y, pch=16, col="black", cex=2.5)
text(sig.p.y, mu.p.y, labels="LONG-SHORT", pos=4, cex = cex.val)

#
# Compute and plot random portfolios to fill space
#
# 100 random Portfolios
set.seed(2001)
x.DOL = runif(100, min=-1.1, max=1.1)
x.IMA_PLUS = runif(100, min=-1.1, max=1.1)
x.Size1_BM1 = runif(100, min=-1.1, max=1.1)
x.Size1_BM2 = runif(100, min=-1.1, max=1.1)
x.Size2_BM1 = runif(100, min=-1.1, max=1.1)
x.Size2_BM2 = 1 - x.DOL - x.IMA_PLUS - x.Size1_BM1 - x.Size1_BM2 - x.Size2_BM1

plot(sd.vec, mu.vec,  ylim=c(-2e-4, 1.3e-3), xlim=c(0, 0.03), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=16, col="blue", cex=2.5, cex.lab=1.75, main="100 Random Portfolios")     
text(sd.vec, mu.vec, labels=asset.names, pos=4, cex = cex.val)
for (i in 1:length(x.DOL)) {
  z.vec = c(x.DOL[i], x.IMA_PLUS[i], x.Size1_BM1[i], x.Size1_BM2[i], x.Size2_BM1[i], x.Size2_BM2[i])
  mu.p = crossprod(z.vec,mu.vec)
  sig.p = sqrt(t(z.vec)%*%sigma.mat%*%z.vec)
  points(sig.p, mu.p, pch=16, col="grey", cex=1.5)
}

# 10K Random Portfolios
set.seed(2001)
x.DOL = runif(10000, min=-1.1, max=1.1)
x.IMA_PLUS = runif(10000, min=-1.1, max=1.1)
x.Size1_BM1 = runif(10000, min=-1.1, max=1.1)
x.Size1_BM2 = runif(10000, min=-1.1, max=1.1)
x.Size2_BM1 = runif(10000, min=-1.1, max=1.1)
x.Size2_BM2 = 1 - x.DOL - x.IMA_PLUS - x.Size1_BM1 - x.Size1_BM2 - x.Size2_BM1

plot(sd.vec, mu.vec,  ylim=c(-2e-4, 1.3e-3), xlim=c(0, 0.03), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=16, col="blue", cex=2.5, cex.lab=1.75, main="10K Random Portfolios")     
text(sd.vec, mu.vec, labels=asset.names, pos=4, cex = cex.val)
for (i in 1:length(x.DOL)) {
  z.vec = c(x.DOL[i], x.IMA_PLUS[i], x.Size1_BM1[i], x.Size1_BM2[i], x.Size2_BM1[i], x.Size2_BM2[i])
  mu.p = crossprod(z.vec,mu.vec)
  sig.p = sqrt(t(z.vec)%*%sigma.mat%*%z.vec)
  points(sig.p, mu.p, pch=16, col="grey", cex=1.5)
}

#
# compute global minimum variance portfolio
#

# method 1: use full system matrix algebra
top.mat = cbind(2*sigma.mat, rep(1, n.asset))
bot.vec = c(rep(1, n.asset), 0)
Am.mat = rbind(top.mat, bot.vec)
b.vec = c(rep(0, n.asset), 1)
z.m.mat = solve(Am.mat)%*%b.vec
m.vec = z.m.mat[1:n.asset,1]
m.vec
# compute expected return, variance and sd
mu.gmin = as.numeric(crossprod(m.vec, mu.vec))
mu.gmin
sig2.gmin = as.numeric(t(m.vec)%*%sigma.mat%*%m.vec)
sig.gmin = sqrt(sig2.gmin)
sig2.gmin  
sig.gmin

# method 2: direct calculation of m using matrix algebra
one.vec = rep(1, n.asset)
sigma.inv.mat = solve(sigma.mat)
top.mat = sigma.inv.mat%*%one.vec
bot.val = as.numeric((t(one.vec)%*%sigma.inv.mat%*%one.vec))
m.mat = top.mat/bot.val
m.mat[,1]


# plot global minimum variance portfolio
plot(sd.vec, mu.vec,  ylim=c(-2e-4, 1.3e-3), xlim=c(0, 0.03), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=16, col="blue", cex=2.5, cex.lab=1.75, main="Global Minimum variance Portfolio")     
text(sd.vec, mu.vec, labels=asset.names, pos=4, cex = cex.val)
points(sig.gmin, mu.gmin, pch=16, cex=2.5, col="green")
text(sig.gmin, mu.gmin, labels="GLOBAL MIN", pos=2.5, cex = cex.val)
for (i in 1:length(x.DOL)) {
  z.vec = c(x.DOL[i], x.IMA_PLUS[i], x.Size1_BM1[i], x.Size1_BM2[i], x.Size2_BM1[i], x.Size2_BM2[i])
  mu.p = crossprod(z.vec,mu.vec)
  sig.p = sqrt(t(z.vec)%*%sigma.mat%*%z.vec)
  points(sig.p, mu.p, pch=16, col="black", cex=1.5)
}


# 
# find efficient portfolio with same ER as IMA_Plus
#
top.mat = cbind(2*sigma.mat, mu.vec, rep(1, n.asset))
mid.vec = c(mu.vec, 0, 0)
bot.vec = c(rep(1, n.asset), 0, 0)
Ax.mat = rbind(top.mat, mid.vec, bot.vec)
bmsft.vec = c(rep(0, n.asset), mu.vec["IMA_Plus"], 1)
z.mat = solve(Ax.mat)%*%bmsft.vec
x.vec = z.mat[1:n.asset,]
x.vec
# compute mean, variance and std deviation
mu.px = as.numeric(crossprod(x.vec, mu.vec))
mu.px
sig2.px = as.numeric(t(x.vec)%*%sigma.mat%*%x.vec)
sig.px = sqrt(sig2.px)
sig.px
mu.vec["IMA_Plus"]
sd.vec["IMA_Plus"]

# 
# find efficient portfolio with same ER as Size2_BM2
#

bsbux.vec = c(rep(0, n.asset), mu.vec["Size2_BM2"], 1)
z.mat = solve(Ax.mat)%*%bsbux.vec
y.vec = z.mat[1:n.asset,]
y.vec
# compute mean, variance and std deviation
mu.py = as.numeric(crossprod(y.vec, mu.vec))
sig2.py = as.numeric(t(y.vec)%*%sigma.mat%*%y.vec)
sig.py = sqrt(sig2.py)
mu.py
sig.py
mu.vec["Size2_BM2"]
sd.vec["Size2_BM2"]


# covariance and correlation between two portfolio returns
sigma.xy = as.numeric(t(x.vec)%*%sigma.mat%*%y.vec)
rho.xy = sigma.xy/(sig.px*sig.py)
sigma.xy
rho.xy

# #
# show assets and portfolios in mean-sd space
#
sd.vec = sqrt(diag(sigma.mat))
plot(sd.vec, mu.vec,  ylim=c(2e-4, 8e-4), xlim=c(0, 0.015), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=16, col="blue", cex=2, main="Global Min and 2 Eff. Port.")
points(sig.gmin, mu.gmin, pch=16, cex=2, col="green")
points(sig.px, mu.px, pch=16, cex=2, col="green")
points(sig.py, mu.py, pch=16, cex=2, col="green")      
text(sd.vec, mu.vec, labels=asset.names, pos=4, cex = cex.val)
text(sig.gmin, mu.gmin, labels="GLOBAL MIN", pos=2, cex = cex.val)        
text(sig.px, mu.px, labels="E1", pos=2, cex = cex.val) 
text(sig.py, mu.py, labels="E2", pos=2, cex = cex.val) 


#
# find efficient portfolio from two efficient portfolios
#
a = 0.5
z.vec = a*x.vec + (1-a)*y.vec
z.vec
# compute mean, variance and std deviation
sigma.xy = as.numeric(t(x.vec)%*%sigma.mat%*%y.vec)
mu.pz = as.numeric(crossprod(z.vec, mu.vec))
sig2.pz = as.numeric(t(z.vec)%*%sigma.mat%*%z.vec)
sig.pz = sqrt(sig2.pz)
mu.pz
sig.pz


#
# compute and plot efficient frontier
#
a = seq(from=1, to=-1, by=-0.1)
n.a = length(a)
z.mat = matrix(0, n.a, n.asset)
colnames(z.mat) = names(mu.vec)
mu.z = rep(0, n.a)
sig2.z = rep(0, n.a)
sig.mx = t(m.vec)%*%sigma.mat%*%x.vec
for (i in 1:n.a) {
  z.mat[i, ] = a[i]*m.vec + (1-a[i])*x.vec
  mu.z[i] = a[i]*mu.gmin + (1-a[i])*mu.px
  sig2.z[i] = a[i]^2 * sig2.gmin + (1-a[i])^2 * sig2.px + 2*a[i]*(1-a[i])*sig.mx
}
plot(sqrt(sig2.z), mu.z, type="b",ylim=c(2e-4, 8e-4), xlim=c(0, 0.015), 
     pch=16, col="green", cex = cex.val, ylab=expression(mu[p]), xlab=expression(sigma[p]), main="Eff. Frontier")
points(sd.vec, mu.vec, pch=16, cex=2, lwd=2, col="blue")
points(sig.gmin, mu.gmin, pch=16, col="green", cex=2)
points(sig.px, mu.px, pch=16, col="green", cex=2)
text(sig.gmin, mu.gmin, labels="GLOBAL MIN", pos=2, cex = cex.val)
text(sd.vec, mu.vec, labels=asset.names, pos=4, cex = cex.val)
text(sig.px, mu.px, labels="E1", pos=2, cex = cex.val)

#
# plot weights as stacked barchart
#

# for some reason xlab doesn't show up
chart.StackedBar(z.mat, xaxis.labels=round(sqrt(sig2.z),digits=3), 
                 xlab="Portfolio SD", ylab="Weights", main="Asset Classes Weights")


#
# compute and plot efficient frontier and random portfolios
#
a = seq(from=1, to=-2, by=-0.1)
n.a = length(a)
z.mat = matrix(0, n.a, n.asset)
mu.z = rep(0, n.a)
sig2.z = rep(0, n.a)
sig.mx = t(m.vec)%*%sigma.mat%*%x.vec
for (i in 1:n.a) {
  z.mat[i, ] = a[i]*m.vec + (1-a[i])*x.vec
  mu.z[i] = a[i]*mu.gmin + (1-a[i])*mu.px
  sig2.z[i] = a[i]^2 * sig2.gmin + (1-a[i])^2 * sig2.px + 2*a[i]*(1-a[i])*sig.mx
}
plot(sqrt(sig2.z), mu.z, type="b", ylim=c(-2e-4, 1.3e-3), xlim=c(0, 0.03), 
     pch=16, col="green", cex = cex.val, ylab=expression(mu[p]), xlab=expression(sigma[p]))
points(sd.vec, mu.vec, pch=16, cex=2, col="blue")

for (i in 1:length(x.DOL)) {
  z.vec = c(x.DOL[i], x.IMA_PLUS[i], x.Size1_BM1[i], x.Size1_BM2[i], x.Size2_BM1[i], x.Size2_BM2[i])
  mu.p = crossprod(z.vec,mu.vec)
  sig.p = sqrt(t(z.vec)%*%sigma.mat%*%z.vec)
  points(sig.p, mu.p, pch=16, col="black", cex=1.5)
}

text(sig.gmin, mu.gmin, labels="GLOBAL MIN", pos=2, col="green", cex = cex.val)
text(sd.vec, mu.vec, labels=asset.names, pos=4, col="blue", cex = cex.val)


#
# compute tangency portfolio
#

rf = 0.0004
sigma.inv.mat = solve(sigma.mat)
one.vec = rep(1, n.asset)
mu.minus.rf = mu.vec - rf*one.vec
top.mat = sigma.inv.mat%*%mu.minus.rf
bot.val = as.numeric(t(one.vec)%*%top.mat)
t.vec = top.mat[,1]/bot.val
t.vec

# compute mean, var and sd
mu.t = as.numeric(crossprod(t.vec, mu.vec))
sig2.t = as.numeric(t(t.vec)%*%sigma.mat%*%t.vec)
sig.t = sqrt(sig2.t)
mu.t
sig.t

# sharpe ratio on tangency portfolio
sr.t = (mu.t - rf)/sig.t
sr.t


#
# Efficient portfolios of T-bills and tangency portfolio
#
x.t = seq(0, 2, by=0.1)
mu.pe = rf + x.t*(mu.t - rf)
sig.pe = x.t*sig.t 
slope.t = (mu.t - rf)/sig.t

# plot efficient portfolios
plot(sqrt(sig2.z), mu.z, type="b", ylim=c(-2e-4, 1.3e-3), xlim=c(0, 0.03), 
     pch=16, col="blue", cex=2, ylab=expression(mu[p]), xlab=expression(sigma[p]))
abline(a=rf, b=slope.t, col="green", lwd=2)
points(sig.t, mu.t, pch=16, col="green", cex=2)
points(sd.vec, mu.vec, pch=16, cex=2, col="black")
text(sig.gmin, mu.gmin, labels="Global min", pos=4, cex=2)
text(sd.vec, mu.vec, labels=asset.names, pos=4, cex=2)
text(sig.t, mu.t, labels="tangency", pos=4,cex=2)
text(0, rf, labels="Rf", pos=2, cex=2)

# plot weights
t.mat = x.t %o% t.vec
e.mat = cbind(1-x.t, t.mat)
colnames(e.mat)[1] = "T-Bill"
chart.StackedBar(e.mat, xaxis.labels=round(sig.pe,digits=3), 
                 xlab="Portfolio SD", ylab="Weights", main="Weights - Efficient Frontier with rf")
