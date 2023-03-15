rm(list=ls())
cat("\014")
set.seed(111)

#######################################
n=c(10,20,30,40,50)
m=1000
mu0=500
sigma=100
mu=c(seq(450,650,10))
M=length(mu)
power=matrix(0,length(n),M)
for (i in 1:length(n)) {
  for (j in 1:M) {
    mu1=mu[j]
    pvalues=replicate(m,expr={
      x=rnorm(n[i],mean = mu1,sd = sigma)
      ttest=t.test(x,alternative='greater',mu=mu0)
      ttest$p.value})
    power[i,j]=mean(pvalues<=.05)
    
  }
}

color=c('red','blue','green','yellow','black')

plot(mu,power[1,],col=color[1],type = 'b',ylab = 'power',xlab='theta')
lines(mu,power[2,],col=color[2],type = 'b')
lines(mu,power[3,],col=color[3],type = 'b')
lines(mu,power[4,],col=color[4],type = 'b')
lines(mu,power[5,],col=color[5],type = 'b')
legend('topright',legend = paste('n=',n),lwd = 1,col = color)








