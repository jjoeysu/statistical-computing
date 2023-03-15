rm(list=ls())
cat("\014")
set.seed(111)

#######################################
library(bootstrap)

boot.t.ci=function(x,B=500,R=100,level=.95,statistic){
  x=as.matrix(x)
  n=nrow(x)
  stat=numeric(B)
  se=numeric(B)
  boot.se=function(x,R,f){
    x=as.matrix(x)
    m=nrow(x)
    th=replicate(R,expr = {
      i=sample(1:m,size=m,replace = T)
      f(x[i,])
    })
    return(sd(th))
  }
  
  for (b in 1:B) {
    j=sample(1:n,size = n,replace = T)
    y=x[j,]
    stat[b]=statistic(y)
    se[b]=boot.se(y,R=R,f=statistic)
  }
  
  stat0=statistic(x)
  t.stats=(stat-stat0)/se
  se0=sd(stat)
  
  alpha=1-level
  Qt=quantile(t.stats,c(alpha/2,1-alpha/2),type=1)
  names(Qt)=rev(names(Qt))
  CI=rev(stat0-Qt*se0)
}

data=cbind(law$LSAT,law$GPA)

stat=function(data){
  cor(data[,1],data[,2])
}

ci=boot.t.ci(data,statistic = stat,B=2000,R=200)
print(ci)






















