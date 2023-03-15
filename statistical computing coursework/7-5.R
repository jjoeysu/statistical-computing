rm(list=ls())
cat("\014")
set.seed(111)

#######################################


library(boot)

data=as.matrix(aircondit)

lambda_MLE=mean(data)
###############################################


r=function(x,ind){
  return(mean(x[ind]))
}

boot.obj=boot(data,statistic = r,R=2000)
print(boot.ci(boot.obj,type = c("norm","basic","perc","bca")))








