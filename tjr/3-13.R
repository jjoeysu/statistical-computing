rm(list=ls())
cat("\014")
set.seed(111)

#######################################

u=runif(1000)

x=2*(1-u)^(-1/4)-2

hist(x,prob=T,xlim = c(min(x),max(x)),breaks = 50)

y=seq(min(x),max(x),by=0.01)

lines(y,64*(2+y)^(-5))