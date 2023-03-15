#rm(list=ls())
cat("\014")
set.seed(111)

#######################################

B=1000
n=c(1000,5000,10000,50000,100000)

pi_hat=matrix(0,nrow = B,ncol = 5)
pi.hat=var_hat=numeric(length(n))

for (i in 1:length(n)) {
  for (j in 1:B) {
    y=runif(n[i])
    pi_hat[j,i]=4*mean(sqrt(1-y^2))
  }
  pi.hat[i]=mean(pi_hat[,i])
  var_hat[i]=var(pi_hat[,i])
}

print(list(pi.hat=pi.hat,var_hat=var_hat))