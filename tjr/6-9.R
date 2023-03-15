rm(list=ls())
cat("\014")
set.seed(111)

#######################################


n=100
m=10000
k50=ceiling(50*(m/100))
k10=ceiling(10*(m/100))

g_ln=replicate(m,expr = {
  x=rlnorm(n)
  X=sort(x)
  gt=0
  for (i in 1:n) {
    gt=gt+(2*i-n-1)*X[i]
  }
  gt/(n^2*mean(x))
})

gs=sort(g_ln)

print(c(g_mean=mean(gs),g_p50=gs[k50],g_p10=gs[k10]))
########################################################
g_u=replicate(m,expr = {
  x=runif(n)
  X=sort(x)
  gt=0
  for (i in 1:n) {
    gt=gt+(2*i-n-1)*X[i]
  }
  gt/(n^2*mean(x))
})

gs=sort(g_u)

print(c(g_mean=mean(gs),g_p50=gs[k50],g_p10=gs[k10]))

##########################################################
g_b=replicate(m,expr = {
  x=rbinom(n,1,0.1)
  X=sort(x)
  gt=0
  for (i in 1:n) {
    gt=gt+(2*i-n-1)*X[i]
  }
  gt/(n^2*mean(x))
})

gs=sort(g_b)

print(c(g_mean=mean(gs),g_p50=gs[k50],g_p10=gs[k10]))

##############################################################
split.screen(c(1,3))

screen(1)
hist(g_ln,prob=T,xlim = c(min(g_ln),max(g_ln)),breaks = 50)

screen(2)
hist(g_u,prob=T,xlim = c(min(g_u),max(g_u)),breaks = 50)

screen(3)
hist(g_b,prob=T,xlim = c(min(g_b),max(g_b)),breaks = 50)














