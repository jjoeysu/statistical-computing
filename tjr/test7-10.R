rm(list=ls())
cat("\014")
set.seed(111)

#######################################
library(DAAG)
library(lattice)
attach(ironslag)

a=seq(10,40,.1)

L1=lm(magnetic~chemical)
plot(chemical,magnetic,main="Linear",pch=16)
yhat1=L1$coef[1]+L1$coef[2]*a
lines(a,yhat1,lwd=2)

L2=lm(magnetic~chemical+I(chemical^2))
plot(chemical,magnetic,main="Quadratic",pch=16)
yhat2=L2$coef[1]+L2$coef[2]*a+L2$coef[3]*a^2
lines(a,yhat2,lwd=2)  

L3=lm(log(magnetic)~chemical)
plot(chemical,magnetic,main="Exponential",pch=16)
logyhat3=L3$coef[1]+L3$coef[2]*a
yhat3=exp(logyhat3)
lines(a,yhat3,lwd=2)

L4=lm(magnetic~chemical+I(chemical^2)+I(chemical^3))
plot(chemical,magnetic,main="Quadratic",pch=16)
yhat4=L4$coef[1]+L4$coef[2]*a+L4$coef[3]*a^2+L4$coef[4]*a^3
lines(a,yhat4,lwd=2)


print(L2)
