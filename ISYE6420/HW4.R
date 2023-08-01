#

N=5000
X<-rep(0,N)
Acp = 0
for (i in 2:N) {
  proposal <- rgamma(1,shape = 1,scale = (1/3))
  U <- runif(1)
  if(U<=1){
    X[i]=proposal
    Acp = Acp+1
  }else{
      X[i] = X[i-1]}
}

hist(X,main = "Posterior distribution of theta",breaks = 30)
mean(X)
plot(X,type = "s",las=1)
Acp/(N-1)

qgamma(0.03,shape = 1,scale = (1/3),lower.tail = F)

############
thetas = rep(0,50000)
lambdas = rep(0,50000)
lambda = 1
theta = 110
for (i in 1:50000){
  mean_theta = (105.5*120)/(120+lambda*90/10)+(lambda*90*110)/(10*120+lambda*90)
  var_theta = (120*90)/(10*120+lambda*90)
  lamba_mean = (120+(theta-110)^2)/(2*120)
  
  newtheta = rnorm(1,mean=mean_theta,sd=sqrt(var_theta))
  newlambda = rexp(1,rate = lamba_mean)
  
  thetas[i] = newtheta
  lambdas[i] = newlambda
  
  theta = newtheta
  lambda = newlambda
  
}

mean(thetas[1000:50000])
var(thetas[1000:50000])
summary(thetas[1000:50000])  
quantile(thetas[1000:50000],0.03)
quantile(thetas[1000:50000],0.97)
hist(thetas,breaks = 30)
