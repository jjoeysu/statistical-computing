rm(list=ls())
cat("\014")
set.seed(111)

#######################################
library(DAAG)
library(lattice)
attach(ironslag)
###############################################################
a=seq(10,40,.1)

L1=lm(magnetic~chemical)
yhat1=L1$coef[1]+L1$coef[2]*a

L2=lm(magnetic~chemical+I(chemical^2))
yhat2=L2$coef[1]+L2$coef[2]*a+L2$coef[3]*a^2

L3=lm(log(magnetic)~chemical)
logyhat3=L3$coef[1]+L3$coef[2]*a
yhat3=exp(logyhat3)

L4=lm(magnetic~chemical+I(chemical^2)+I(chemical^3))
yhat4=L4$coef[1]+L4$coef[2]*a+L4$coef[3]*a^2+L4$coef[4]*a^3

###############################################################
n=length(magnetic)
e1=e2=e3=e4=numeric(n)

for (k in 1:n) {
  y=magnetic[-k]
  x=chemical[-k]
  
  J1=lm(y~x)
  yhat1=J1$coef[1]+J1$coef[2]*chemical[k]
  e1[k]=magnetic[k]-yhat1
  
  J2=lm(y~x+I(x^2))
  yhat2=J2$coef[1]+J2$coef[2]*chemical[k]+J2$coef[3]*chemical[k]^2
  e2[k]=magnetic[k]-yhat2
  
  J3=lm(log(y)~x)
  logyhat3=J3$coef[1]+J3$coef[2]*chemical[k]
  yhat3=exp(logyhat3)
  e3[k]=magnetic[k]-yhat3
  
  J4=lm(y~x+I(x^2)+I(x^3))
  yhat4=J4$coef[1]+J4$coef[2]*chemical[k]
  +J4$coef[3]*chemical[k]^2+J4$coef[4]*chemical[k]^3
  e4[k]=magnetic[k]-yhat4
}

print(list(mean_e2_1=mean(e1^2),mean_e2_2=mean(e2^2),
           mean_e2_3=mean(e3^2),mean_e2_4=mean(e4^2)))


adjr2=c(summary(L1)$adj.r.squared,summary(L2)$adj.r.squared,
        summary(L3)$adj.r.squared,summary(L4)$adj.r.squared)

print(list(adjr2_1=adjr2[1],adjr2_2=adjr2[2],
           adjr2_3=adjr2[3],adjr2_4=adjr2[4]))












