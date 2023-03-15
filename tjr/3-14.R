rm(list=ls())
cat("\014")
set.seed(111)

#######################################


cho=function(n,mu,sigma){
  d=length(mu)
  q=chol(sigma)
  z=matrix(rnorm(n*d),nrow = n,ncol = d)
  x=z%*%q+matrix(mu,n,d,byrow = T)
  return(x)
}

###############################################
n=200
mu=c(0,1,2)
sigma=cbind(c(1,-0.5,0.5),c(-0.5,1,-0.5),c(0.5,-0.5,1))

x=cho(n,mu,sigma)

pairs(x)