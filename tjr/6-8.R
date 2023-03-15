rm(list=ls())
cat("\014")
set.seed(111)

#######################################
count5test=function(x,y){
  X=x-mean(x)
  Y=y-mean(y)
  outx=sum(X>max(Y))+sum(X<min(Y))
  outy=sum(Y>max(X))+sum(Y<min(X))
  return(as.integer(max(c(outx,outy))>5))
}

ftest=function(x,y,n,alpha){
  f=var(x)/var(y)
  return(as.integer(f>qf(1-alpha/2,n,n)||f<qf(1-alpha/2,n,n)^(-1)))
}
##########################################

m=10000
n=c(20,100,1000)
sigma1=1
sigma2=2
alpha=.055
power_cf=power_f=rep(0,length(n))

for (i in 1:length(n)) {
  power1=replicate(m,expr = {
    x=rnorm(n[i],0,sigma1)
    y=rnorm(n[i],0,sigma2)
    count5test(x,y)
  })
  power_cf[i]=mean(power1)
  
  
  power2=replicate(m,expr = {
    x=rnorm(n[i],0,sigma1)
    y=rnorm(n[i],0,sigma2)
    ftest(x,y,n-1,alpha)
  })
  power_f[i]=mean(power2)
}

print(cbind(n,power_cf,power_f))
























