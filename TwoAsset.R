##########################################
# Portifólios Ótimos Arriscados
# Baseado em Capítulo 07 BKM
# Claudio Ribeiro de Lucinda
# FEA-RP/USP
##########################################

##########################################
# Parte 1: Two Asset Portfolio
#########################################################
# Função que retorna os retornos esperados e variâncias
#########################################################

rm(list=ls())
graphics.off()

two_asset <- function(.w,.rho,.mu.A,.mu.B,.sig.A,.sig.B) {
  
  sig2.A = .sig.A^2
  sig2.B = .sig.B^2
  sig.AB <- .rho*.sig.A*.sig.B
  mu.p1 <- .w*.mu.A + (1-.w)*.mu.B
  sig2.p1 <- (.w^2) * sig2.A + ((1-.w)^2)* sig2.B + 2*.w*(1-.w)*sig.AB
  sig.p1 <- sqrt(sig2.p1)
  w0<-1
  VaR.p1 <- (mu.p1 + sig.p1*qnorm(0.05))*w0
  results<-data.frame(mu.p1,sig.p1,VaR.p1)

}

port_1<-plyr::ldply(seq(0,1,.01), function(x) two_asset(x,.rho=1,.mu.A=.175,.mu.B=.055,.sig.A=.258,.sig.B=.115))

port_0<-plyr::ldply(seq(0,1,.01), function(x) two_asset(x,.rho=0,.mu.A=.175,.mu.B=.055,.sig.A=.258,.sig.B=.115))

port_minus_1<-plyr::ldply(seq(0,1,.01), function(x) two_asset(x,.rho=-1,.mu.A=.175,.mu.B=.055,.sig.A=.258,.sig.B=.115))
library(ggplot2)
library(reshape)
new_data<-merge(port_1,port_0,by="mu.p1")
colnames(new_data)<-c("mu.p1","sig_1","Var_1","sig_2","Var_2")
new_data<-merge(new_data,port_minus_1,by="mu.p1")
colnames(new_data)<-c("mu.p1","sig_1","Var_1","sig_2","Var_2","sig_3","Var_3")



p<-ggplot() +geom_path(data=new_data,aes(x=sig_1,y=mu.p1),color="red") +
  geom_path(data=new_data,aes(x=sig_2,y=mu.p1),color="blue") +
  geom_path(data=new_data,aes(x=sig_3,y=mu.p1),color="green") +
  labs(x = "Std. Dev.", y = "Exp. Return") +
  scale_x_continuous(limits=c(0,.3)) +
  geom_point(aes(x=.258,y=.175), col="blue") +
  geom_point(aes(x=.115,y=.055), col="red")

