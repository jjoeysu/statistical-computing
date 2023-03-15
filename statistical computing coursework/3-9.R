rm(list=ls())
cat("\014")
set.seed(111)

#######################################
dg=function(n){
  u1=runif(n,-1,1)
  u2=runif(n,-1,1)  
  u3=runif(n,-1,1)
  x=1:n
  
  for(i in 1:n){
    if(abs(u3[i])>=abs(u2[i])&&abs(u3[i])>=abs(u1[i])){
      x[i]=u2[i]
    }
    else{
      x[i]=u3[i]
    }
  }
  return(x)
}
############################################
x=dg(1000)

hist(x,prob=T,xlim = c(min(x),max(x)),breaks = 50)

y=seq(min(x),max(x),by=0.01)

lines(y,3/4*(1-y^2))


