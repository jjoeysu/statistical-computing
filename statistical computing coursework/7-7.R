rm(list=ls())
cat("\014")
set.seed(111)

#######################################
library(bootstrap)
B=2000
n=nrow(scor)

################################
r=function(x){
sigma_hat=cov(x)

ev=eigen(sigma_hat)$values

theta_hat=ev[1]/sum(ev)

return(theta_hat)
}
################################

theta_hat=r(scor)

theta_b=numeric(B)

for (b in 1:B) {
  i=sample(1:n,size = n,replace = T)
  theta_b[b]=r(scor[i,])
}

bias=mean(theta_b-theta_hat)

se.se=sd(theta_b)

print(list(theta_hat=theta_hat,bias=bias,se.se=se.se))













