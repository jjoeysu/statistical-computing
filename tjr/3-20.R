rm(list=ls())
cat("\014")
set.seed(111)

#######################################
s=1
r=2
n=1000
m=5000
lambda=2

x1=replicate(m,expr = {
t0=10
tn=rexp(100,lambda)
sn=cumsum(tn)
nn=min(which(sn>t0))
y=rgamma(n,s,r)
sum(y[1:nn-1])
})

mvx10=c(s/r*lambda*10,(s^2+s)/r^2*lambda*10)
mvx1=c(mean(x1),var(x1))
cx1=rbind(mvx10,mvx1)
rownames(cx1)=c('T_value','R_value')

print(cx1)
#######################################

s=2
r=1
n=1000
lambda=2

x2=replicate(m,expr = {
  t0=10
  tn=rexp(100,lambda)
  sn=cumsum(tn)
  nn=min(which(sn>t0))
  y=rgamma(n,s,r)
  sum(y[1:nn-1])
})

mvx20=c(s/r*lambda*10,(s^2+s)/r^2*lambda*10)
mvx2=c(mean(x2),var(x2))
cx2=rbind(mvx20,mvx2)
rownames(cx2)=c('T_value','R_value')

print(cx2)
############################################

s=2
r=2
n=1000
lambda=3

x3=replicate(m,expr = {
  t0=10
  tn=rexp(100,lambda)
  sn=cumsum(tn)
  nn=min(which(sn>t0))
  y=rgamma(n,s,r)
  sum(y[1:nn-1])
})

mvx30=c(s/r*lambda*10,(s^2+s)/r^2*lambda*10)
mvx3=c(mean(x3),var(x3))
cx3=rbind(mvx30,mvx3)
rownames(cx3)=c('T_value','R_value')

print(cx3)