rm(list=ls())
cat("\014")
set.seed(111)

#######################################

#MC
n=10000
u=runif(n)
x=exp(u)
theta_MC=mean(x)
var_MC=var(x)/n

#DVMC

v=runif(n/2)
a=cov(exp(v),exp(1-v))
b=var(exp(v))+var(exp(1-v))+2*a
theta_DVMC=mean((exp(v)+exp(1-v))/2)
var_DVMC=b/4/n

#compare

c=(var_MC-var_DVMC)/var_MC*100

#display
cat('theta_MC = ',theta_MC)
cat('\nvar_MC = ',var_MC)
cat('\ntheta_DVMC = ',theta_DVMC)
cat('\nvar_DVMC = ',var_DVMC)
cat('\nVRP = ',c,'%')


