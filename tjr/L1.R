rm(list=ls())
cat("\014")
set.seed(111)

#######################################
p1=.2
p2=.3
p3=1-p1-p2
mu1=1
mu2=12
mu3=25
sigma=3

N=2000
x0=numeric(N)

for (i in 1:N) {
  temp=runif(1)
  if(temp<.2){
    x0[i]=rnorm(1,mu1,sigma)
  }
  else if(temp>=.2&&temp<.5){
    x0[i]=rnorm(1,mu2,sigma)
  }
  else{
    x0[i]=rnorm(1,mu3,sigma)
  }
}

plot(density(x0))

#########################################
m=1000
p10=.1
p20=.25
p30=1-p10-p20
mu10=1.5
mu20=11
mu30=23
sigma0=2

para=c(p10,p20,p30,mu10,mu20,mu30,sigma0)
tol=1e-20
para.old=para+1

B=100
para_b=matrix(0,nrow = B,ncol = 7)
for (b in 1:B) {
  
  para=c(p10,p20,p30,mu10,mu20,mu30,sigma0)
  para.old=para+1

  if(b==1)
    x=x0
  else {
    index=sample(1:N,N,replace = T)
    x=x0[index]
  }
  
for (j in 1:m) {
  
zd=para[1]*dnorm(x,para[4],para[7])+para[2]*dnorm(x,para[5],para[7])+
  para[3]*dnorm(x,para[6],para[7])

z10=para[1]*dnorm(x,para[4],para[7])/zd
z20=para[2]*dnorm(x,para[5],para[7])/zd
z30=para[3]*dnorm(x,para[6],para[7])/zd

phi1=sum(z10)
phi2=sum(z20)
phi3=sum(z30)

p11=phi1/N
p21=phi2/N
p31=phi3/N

phix1=sum(x*z10)
mu11=phix1/phi1
phix2=sum(x*z20)
mu21=phix2/phi2
phix3=sum(x*z30)
mu31=phix3/phi3

phixmu=numeric(3)

phixmu[1]=sum((x-mu11)^2*z10)
phixmu[2]=sum((x-mu21)^2*z20)
phixmu[3]=sum((x-mu31)^2*z30)
sigma1=sqrt(sum(phixmu)/N)

para=c(p11,p21,p31,mu11,mu21,mu31,sigma1)

if(sqrt(sum((para-para.old)^2))/sqrt(sum(para.old^2))<tol) {break}

para.old=para

}

para_b[b,]=para

if(b==1) iter=j

}
para_cov=cov(para_b)
para_sd_hat=sqrt(diag(para_cov))

print(list(estimate=para_b[1,],para_sd_hat=para_sd_hat,iter=iter,tol=tol))





















