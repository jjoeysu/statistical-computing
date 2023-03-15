rm(list=ls())
cat("\014")
set.seed(111)

#######################################

n=20

x=rnorm(n,1,2)
delta=rnorm(n,2,16)
y=4*x+delta
cat('cor_xy = ',cor(x,y))
print(cor.test(x,y))
print(cor.test(x,y,method='spearman'))
print(cor.test(x,y,method='kendall'))





















