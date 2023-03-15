
temp=c(1,2,3,4,5)

color=c('red','blue','green','yellow','black')
plot(density(p1),col=color[1],type = 'b',ylab = 'density',xlab='pi_hat')
lines(density(p2),col=color[2])
lines(density(p3),col=color[3])
lines(density(p4),col=color[4])
lines(density(p5),col=color[5])
legend('topright',legend = paste('方法：',temp,'(按文中顺序)'),lwd = 1,col = color)