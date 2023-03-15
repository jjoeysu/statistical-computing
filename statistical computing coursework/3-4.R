rm(list=ls())
cat("\014")
set.seed(111)

split.screen(c(2,2))

############################################
screen(1)

sigma=1

u=runif(1000)

x=sqrt(-2*sigma^2*log(1-u))

hist(x,prob=T,xlim = c(min(x),max(x)),breaks = 50)

y=seq(min(x),max(x),by=0.01)

lines(y,y/sigma^2*exp(-y^2/(2*sigma^2)))
#############################################
screen(2)

sigma=2

u=runif(1000)

x=sqrt(-2*sigma^2*log(1-u))

hist(x,prob=T,xlim = c(min(x),max(x)),breaks = 50)

y=seq(min(x),max(x),by=0.01)

lines(y,y/sigma^2*exp(-y^2/(2*sigma^2)))
#############################################
screen(3)

sigma=3

u=runif(1000)

x=sqrt(-2*sigma^2*log(1-u))

hist(x,prob=T,xlim = c(min(x),max(x)),breaks = 50)

y=seq(min(x),max(x),by=0.01)

lines(y,y/sigma^2*exp(-y^2/(2*sigma^2)))
#############################################
screen(4)

sigma=4

u=runif(1000)

x=sqrt(-2*sigma^2*log(1-u))

hist(x,prob=T,xlim = c(min(x),max(x)),breaks = 50)

y=seq(min(x),max(x),by=0.01)

lines(y,y/sigma^2*exp(-y^2/(2*sigma^2)))


