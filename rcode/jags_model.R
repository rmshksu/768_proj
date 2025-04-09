###########################
### rough ass jags code ### 
###########################

library(dclone)
library(rjags) # this fucking pains me to do

data=read.csv("result.csv") # read in data
data$YEAR=data$YEAR-2013 # set 2014 at year 1
data$GROSSDOMESTICPRODUCT=data$GROSSDOMESTICPRODUCT/(10^5) # scale gdp to millions
dtr=subset(data,data$YEAR<5) # training data set
dts=subset(data,data$YEAR>4) # testing data set
y1tr=dtr$DEATHS # death counts (train)
y1ts=dts$DEATHS # death counts (test)
X=matrix(1,nrow(dtr),5) # empty design matrix
# scaling the entire design matrix (sans intercept) by 10
X[,2]=as.numeric(as.factor(dtr$STATE))/10 # add in states as numerics of factors
X[,3]=dtr$GROSSDOMESTICPRODUCT/10 # add in gdp
X[,4]=dtr$PERCENTBACHELORSDEGREEORHIGHER/10 # add in bachelors degree rate
X[,5]=dtr$YEAR/10 # add in years
Y=y1tr # current response

# this was just a proof of concept with poisson n mix
# again, i hate this

nmm1 <- function(){
  
  # Process model  
  for(i in 1:n){
    y[i] ~ dbinom(p[i], N[i])
    N[i] ~ dpois(lambda[i])
    p[i] ~ dbeta(a,b)
    lambda[i] <- exp(beta0 + beta1*x[i])
  }
  
  # Priors (i.e., parameter model)
  beta0 ~ dnorm(0,1/10^6)
  beta1 ~ dnorm(0,1/10^6)
  a <- 1
  b <- 4
}

# i stole this all from trevor if you havent realized that yet

data.for.jags <- list(y = Y,x = X[,5], n = length(Y))
p_init <- rep(0.3, length(Y))
jags.inits <- list(beta0 = 5,beta1 = 1,p = p_init)
nmm1.mcmc <- jags.fit(data.for.jags, c("beta0","beta1","p","N"), nmm1, jags.inits, 
                      n.adapt = 5000,n.update = 5000, n.iter = 30000, 
                      thin = 1,n.chains=1)

# negative binomial is a bitch in jags
# so i just used a poisson where lambda is a gamma r.v.

nmm2 <- function(){
  
  # Process model  
  for(i in 1:n){
    y[i] ~ dbinom(p[i], N[i])
    N[i] ~ dpois(lambda_N[i])  
    lambda_N[i] ~ dgamma(r, r/lambda[i]) 
    p[i] ~ dbeta(a, b)
    lambda[i] <- exp(beta0 + beta1*x[i])
  }
  
  # Priors (i.e., parameter model)
  beta0 ~ dnorm(0, 1/10^6)
  beta1 ~ dnorm(0, 1/10^6)
  r ~ dunif(0.1, 30)
  a <- 1
  b <- 4
}

data.for.jags <- list(y = Y,x = X[,5], n = length(Y))
p_init <- rep(0.3, length(Y))
jags.inits <- list(beta0 = 5,beta1 = 1,p = p_init, r = 5)
nmm2.mcmc <- jags.fit(data.for.jags, c("beta0","beta1","p","N","r"), nmm2, jags.inits, 
                      n.adapt = 5000,n.update = 5000, n.iter = 100000, 
                      thin = 1,n.chains=1)


samples <- nmm2.mcmc[[1]]

# god damn jags really does use the WORST mcmc imaginable
K <- 100000
plot(1:K,samples[,1],xlab="k",ylab="[beta0|y]",typ="l",main="")
plot(1:K,samples[,2],xlab="k",ylab="[beta1|y]",typ="l",main="")
plot(1:K,samples[,3],xlab="k",ylab="[p|y]",typ="l",main="")
plot(1:K,samples[,4],xlab="k",ylab="[N|y]",typ="l",main="")
plot(1:K,samples[,5],xlab="k",ylab="[r|y]",typ="l",main="")
