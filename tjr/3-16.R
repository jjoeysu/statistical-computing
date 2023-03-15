rm(list=ls())
cat("\014")
set.seed(111)

#######################################

data=readxl::read_excel("Table7_1.xlsx")

st=function(b){
  a=as.matrix(b)
  m=length(a[,1])
  n=length(a[1,])
  x=0
  s=0
  for(i in 1:n){
    x=mean(a[,i])
    s=sd(a[,i])
    for(j in 1:m){
      a[j,i]=(a[j,i]-x)/s
    }
  }
  return(a)
}

x=st(data)
