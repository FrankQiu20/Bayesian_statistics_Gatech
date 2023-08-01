###Problem 1
###Equal tail interval
c(qbeta(0.025,shape1 = 17,shape2 = 283),qbeta(0.975,shape1 = 17,shape2 = 283))


c(qbeta(0.025,shape1 = 25,shape2 = 275),qbeta(0.975,shape1 = 25,shape2 = 275))

####HPD
library(HDInterval)

hdi(rbeta(1000000,shape1=17,shape2=283),credMass = 0.95)
hdi(rbeta(1000000,shape1 = 25, shape2 = 275),credMass = 0.95)

############Problem 3
library(invgamma)
thetas = rep(0,100000)
tau_squre = rep(0,100000)
sum_y = -0.63
n = 10
###initial
theta = 5
tau_squre = 1
####
for (i in 1:100000){
  mean_theta = (tau_squre*sum_y)/((n*tau_squre)+1)
  var_theta = tau_squre/((n*tau_squre+1))
  
  shape = 3/2
  rate = ((theta^2)+2)/2

  newtheta = rnorm(1,mean=mean_theta,sd=sqrt(var_theta))
  new_tau_squre = rinvgamma(n=1,shape=shape,rate = rate)
  
  
  thetas[i] = newtheta
  tau_squre[i] = new_tau_squre
  
  theta = newtheta
  tau_squre = new_tau_squre
  
}

hist(thetas[1000:100000],breaks = 30,xlab = "Generated theta",main = "")
mean(thetas[1000:100000])
var(thetas[1000:100000])

quantile(thetas[1000:100000],0.025)
quantile(thetas[1000:100000],0.975)

