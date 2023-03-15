rm(list=ls())
cat("\014")
set.seed(111)

#######################################

n=20
m=10000

alpha=.05

p=rep(0,m)

for (i in 1:m) {
  x=rchisq(n,2)
  if(abs(mean(x)-2)*sqrt(n/var(x))<qt(1-alpha/2,df=n-1)){p[i]=1}
}

mp=mean(p)

cat('(1-alpha)_hat = ',mp)















