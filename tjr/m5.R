#rm(list=ls())
cat("\014")
set.seed(111)

#######################################

B=1000
n=c(1000,5000,10000,50000,100000)

a=c(2*sqrt(2)-3,sqrt(5)-2*sqrt(2),-sqrt(5))
b=c((21-2*sqrt(2))/6,(6+2*sqrt(2)-sqrt(5))/2,(18+5*sqrt(5))/6)

pi_hat=I=I1=I2=I3=matrix(0,nrow = B,ncol = 5)
pi.hat=I_hat=I1_hat=I2_hat=I3_hat=var_hat=numeric(length(n))

for (i in 1:length(n)) {
  for (j in 1:B) {
      u=runif(n[i]/3)
      x1=(-b[1]+sqrt(b[1]^2+2*a[1]*u))/a[1]
      x2=(-b[2]+sqrt(b[2]^2+2*a[2]*(u+a[2]/18+b[2]/3)))/a[2]
      x3=(-b[3]+sqrt(b[3]^2+2*a[3]*(u+2*a[3]/9+2*b[3]/3)))/a[3]
      I1[j,i]=mean(sqrt(1-x1^2)/(a[1]*x1+b[1]))
      I2[j,i]=mean(sqrt(1-x2^2)/(a[2]*x2+b[2]))
      I3[j,i]=mean(sqrt(1-x3^2)/(a[3]*x3+b[3]))
      I[j,i]=I1[j,i]+I2[j,i]+I3[j,i]
      pi_hat[j,i]=4*I[j,i]
      

  }
  pi.hat[i]=mean(pi_hat[,i])
  I1_hat[i]=mean(I1[,i])
  I2_hat[i]=mean(I2[,i])
  I3_hat[i]=mean(I3[,i])
  I_hat[i]=mean(I[,i])
  var_hat[i]=var(pi.hat)
}

print(list(I1_hat=I1_hat,I2_hat=I2_hat,I3_hat=I3_hat,
           I_hat=I_hat,pi.hat=pi.hat,var_hat=var_hat))


