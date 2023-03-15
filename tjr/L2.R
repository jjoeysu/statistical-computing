rm(list=ls())
cat("\014")
set.seed(111)

#######################################

theta=.125
n=1000
m=500
t=6

y=numeric(n)
z=numeric(m)

y=rexp(n,theta)
z=rexp(m,theta)

r=0
for (i in 1:m) {
  if(z[i]<=6)
    r=r+1
}

print(r)
##########################################

theta0=.1
N=1000
tol=1e-20

theta.new=theta0
theta.old=theta0+1

for (i in 1:N) {
  
u=m/theta.new+t*(m-r)-r*t/(exp(theta.new*t)-1)
theta.new=(n+m)/(sum(y)+u)
  
if(sqrt(sum((theta.new-theta.old)^2))/sqrt(sum(theta.old^2))<tol) {break} 

theta.old=theta.new

}

print(list(estimate=theta.new,iter=i,tol=tol))




















