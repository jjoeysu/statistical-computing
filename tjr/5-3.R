rm(list=ls())
cat("\014")
set.seed(111)

#######################################

n=1000
u=runif(n,0,0.5)
theta1=mean(exp(-u))/2
var1=var(exp(-u))*0.5^2/n
cat('theta1 = ',theta1)
cat('\nvar1 = ',var1)


v=rexp(n,1)
k=rep(0,n)
k[which(v<0.5)]=1
theta2=mean(k)
var2=var(k)/n
cat('\ntheta2 = ',theta2)
cat('\nvar2 = ',var2)






