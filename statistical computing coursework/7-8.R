rm(list=ls())
cat("\014")
set.seed(111)

#######################################
library(bootstrap)
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

theta_jack=numeric(n)

for (i in 1:n) {
  theta_jack[i]=r(scor[-i])
}

bias=(n-1)*mean(mean(theta_jack)-theta_hat)

se=sqrt((n-1)*mean(theta_jack-mean(theta_jack))^2)

print(list(bias=bias,se=se))


















