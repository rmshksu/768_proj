# i'm actually so angry that this was this easy

library(dclone) # ffffffffffFFFFFFFFUUUUUUUUU
library(rjags) # aaaaaaaaaaaAAAAAaaaAAAAAAAA

data=read.csv("result.csv") # read in data
data$YEAR=data$YEAR-2013 # set 2014 at year 1
data$GROSSDOMESTICPRODUCT=data$GROSSDOMESTICPRODUCT/(10^5) # scale gdp to millions
dtr=subset(data,data$YEAR<6) # training data set
dts=subset(data,data$YEAR>5) # testing data set
y1tr=dtr$DEATHS # death counts (train)
y1ts=dts$DEATHS # death counts (test)
X=matrix(1,nrow(dtr),5) # empty design matrix
# scaling the entire design matrix (sans intercept) by 10
X[,2]=as.numeric(as.factor(dtr$STATE))/10 # add in states as numerics of factors
X[,3]=dtr$GROSSDOMESTICPRODUCT/10 # add in gdp
X[,4]=dtr$PERCENTBACHELORSDEGREEORHIGHER/10 # add in bachelors degree rate
X[,5]=dtr$YEAR/10 # add in years
Y=y1tr # current response


nmm2 <- function(){
  
  # Process model  
  for(i in 1:n){
    y[i] ~ dbinom(p, N[i])
    N[i] ~ dpois(lambda_N[i])  
    lambda_N[i] ~ dgamma(r, r/lambda[i]) 
    lambda[i] <- exp(beta0 + beta1*x[i])
  }
  
  # Priors (i.e., parameter model)
  p ~ dbeta(a, b)
  beta0 ~ dnorm(0, 1/10^6)
  beta1 ~ dnorm(0, 1/10^6)
  r ~ dunif(0.1, 30)
  a <- 1
  b <- 4
}

jags_data <- list(y = Y,x = X[,5], n = length(Y))
jags_inits <- list(beta0 = 5,beta1 = 1,p = 0.3, r = 5)
nmm2_mcmc <- jags.fit(jags_data, c("beta0","beta1","p","N","r","y"), nmm2, jags_inits, 
                      n.adapt = 5000,n.update = 5000, n.iter = 100000, 
                      thin = 100,n.chains=1)


samples <- nmm2_mcmc[[1]]

# livid, absolutely livid
K <- 1000
plot(1:K,samples[,251],xlab="k",ylab="[beta0|y]",typ="l",main="")
plot(1:K,samples[,252],xlab="k",ylab="[beta1|y]",typ="l",main="")
plot(1:K,samples[,253],xlab="k",ylab="[p|y]",typ="l",main="")
plot(1:K,samples[,1],xlab="k",ylab="[N1t1|y]",typ="l",main="")
plot(1:K,samples[,5],xlab="k",ylab="[N1t5|y]",typ="l",main="")
plot(1:K,samples[,51],xlab="k",ylab="[N50t1|y]",typ="l",main="")
plot(1:K,samples[,254],xlab="k",ylab="[r|y]",typ="l",main="")
