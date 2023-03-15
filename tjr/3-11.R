rm(list=ls())
cat("\014")
set.seed(111)

#######################################

mnn=function(p1){
  n=1000
  p2=1-p1
  x=1:n
  
  u=runif(n)
  for(i in 1:n ){
    if (u[i]<=p1){
      x[i]=rnorm(1,0,1)
    }
    else{
      x[i]=rnorm(1,3,1)
    }
  }
  
  return(x)
}
#################################################
split.screen(c(2,2))
##################################################
p1=0.5

screen(1)

x=mnn(p1)
hist(x,prob=T,xlim = c(min(x),max(x)),breaks = 20)
y=seq(min(x),max(x),by=0.01)
lines(y,p1/sqrt(2*pi)*exp(-y^2/2))
lines(y,(1-p1)/sqrt(2*pi)*exp(-(y-3)^2/2))

##################################################
p1=0.6

screen(2)

x=mnn(p1)
hist(x,prob=T,xlim = c(min(x),max(x)),breaks = 20)
y=seq(min(x),max(x),by=0.01)
lines(y,p1/sqrt(2*pi)*exp(-y^2/2))
lines(y,(1-p1)/sqrt(2*pi)*exp(-(y-3)^2/2))
##################################################
p1=0.7

screen(3)

x=mnn(p1)
hist(x,prob=T,xlim = c(min(x),max(x)),breaks = 20)
y=seq(min(x),max(x),by=0.01)
lines(y,p1/sqrt(2*pi)*exp(-y^2/2))
lines(y,(1-p1)/sqrt(2*pi)*exp(-(y-3)^2/2))
##################################################
p1=0.8

screen(4)

x=mnn(p1)
hist(x,prob=T,xlim = c(min(x),max(x)),breaks = 20)
y=seq(min(x),max(x),by=0.01)
lines(y,p1/sqrt(2*pi)*exp(-y^2/2))
lines(y,(1-p1)/sqrt(2*pi)*exp(-(y-3)^2/2))








