rm(list=ls())
cat("\014")
set.seed(111)

#######################################


library(boot)

data=as.matrix(aircondit)

lambda_MLE=1/mean(data)
###############################################
B=2000
n=nrow(data)
lambda_b=numeric(B)

for (b in 1:B) {
  i=sample(1:n,size = n,replace = T)
  lambda_b[b]=1/mean(data[i])
}

bias=mean(lambda_b-lambda_MLE)

se.se=sd(lambda_b)

print(list(lambda_MLE=lambda_MLE,bias=bias,se.se=se.se))










