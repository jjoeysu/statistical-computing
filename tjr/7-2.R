rm(list=ls())
cat("\014")
set.seed(111)

#######################################

library(bootstrap)
n=nrow(law)
B=2000
theta.b=numeric(B)
indices=matrix(0,nrow = B,ncol = n)

for (b in 1:B) {
  i=sample(1:n,size = n,replace = T)
  theta.b[b]=cor(law$LSAT[i],law$GPA[i])
  indices[b,]=i
}

se.jack=numeric(n)
for (i in 1:n) {
  keep=(1:B)[apply(indices,MARGIN=1,FUN=function(k){!any(k==i)})]
  se.jack[i]=sd(theta.b[keep])
}

se.se=sqrt((n-1)*mean((se.jack-mean(se.jack))^2))
print(list(se.se=se.se))