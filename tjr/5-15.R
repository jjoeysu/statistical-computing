rm(list=ls())
cat("\014")
set.seed(111)

#######################################

n=10000
m=n/5
theta=rep(0,5)
var=rep(0,5)
for (j in 1:5)
{
  u=runif(m)
  x=-log(exp(-.2*(j-1))-(exp(-.2*(j-1))-exp(-.2*j))*u)
  y=(exp(-.2*(j-1))-exp(-.2*j))/(1+x^2)
  
  theta[j]=mean(y)
  var[j]=var(y)/m
  
}
###################################
theta_hat=sum(theta)

var_SIM=sum(var)

sd_SIM=sqrt(var_SIM)

print(cbind(theta,var))
print(cbind(theta_hat,var_SIM,sd_SIM))



