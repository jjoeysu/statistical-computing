rm(list=ls())
cat("\014")
set.seed(111)

#######################################

n=100
m=10000
mu=5
sigma=1
alpha=.05

p=rep(0,m)

for (i in 1:m) {
      x=rlnorm(n,meanlog = mu,sdlog = sigma)
      l=mean(log(x))-qnorm(1-alpha/2)/sqrt(n)
      r=mean(log(x))+qnorm(1-alpha/2)/sqrt(n)
      if(mu>l&&mu<r){p[i]=1}
}

mp=mean(p)

cat('(1-alpha)_hat = ',mp)


























