rm(list=ls())
cat("\014")
set.seed(111)

#######################################

n=100
m=10000
mu0=1
alpha=.05

ahpha_hat_chisq=replicate(m,expr = {
  x=rchisq(n,1)
  return(abs(mean(x)-mu0)*sqrt(n/var(x))>qt(1-alpha/2,df=n-1))
})

ahpha_hat_u=replicate(m,expr = {
  x=runif(n,0,2)
  return(abs(mean(x)-mu0)*sqrt(n/var(x))>qt(1-alpha/2,df=n-1))
})

ahpha_hat_exp=replicate(m,expr = {
  x=rexp(n)
  return(abs(mean(x)-mu0)*sqrt(n/var(x))>qt(1-alpha/2,df=n-1))
})

print(rbind(n=n,m=m,alpha=alpha,ahpha_hat_chisq=mean(ahpha_hat_chisq),
        ahpha_hat_u=mean(ahpha_hat_u),ahpha_hat_exp=mean(ahpha_hat_exp)))






















