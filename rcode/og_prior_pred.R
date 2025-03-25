# i plan on vectorizing in v3
set.seed(78) # reproducibility
library(mvtnorm)

# so from what i can see we have opiod deaths
# it's fair to assume that the data we have is not exactly fent deaths

# additionally, fentanyl deaths are rising yoy not falling
# this is an exploratory study, but if it were to be built
# towards a forecasting model, that statement may become important

# we assume in one time step that the # of fent deaths = z
# the data we have is = y (some subset of z)
# thus [z|y,p]=binom(y,p)

# y is count data, and it can have errors in it
# rossen et al. claim 19% expected under-reporting
# for synthetic opioids, but the ranges are a little wide
# and our data is not strictly synthetic
# somewhere between 17% and 22%
# so, we could say [y|tau,phi]=nbinom(tau,phi)
# where tau = lambda*theta
# set [theta]=1-unif(0.17,0.22)
# and [phi]=unif(0,20)

# lambda is estimated with some covariates
# for the time being i think it's hard to work with this
# we assume if lambda=e^X*B
# and X = design matrix
# since its log link the beta term is very sensative
# especially since our parameters are probably categorical
# [B]=N(0,1.5)

# p is a probability, so classically:
# [p|a,b]=beta(a,b)
# using marginal means:
# a=mu*v
# b=(1-mu)*v
# so now [p|mu,v]=beta(mu*v,(1-mu)*v)

# we estimate mu=1/1+e^-(Z*gamma)
# [gamma]=N(0,1)
# v = some dispersion constant

###########################
# prior predictive checks #
###########################

# i lack initial understanding of data
# so ill make some wild assumptions

# p is estimated with average least costed path
Z=runif(50,1,20) # bounding lbar between 1 and 20
# i think some baseline effect is rational
gamma=rnorm(2) # and lbar should have a pretty high interaction
mu=1/(1+exp(-(gamma[1]+gamma[2]*Z)))
v=10 # idk tbh
alpha=mu*v # calculate alpha
beta=(1-mu)*v # calculate beta
p=rbeta(1,alpha,beta) # simulate p

# lambda is estimated with demographic variables
# im ngl i dont know how to simulate these
# ill just use binomial
x1=rbinom(50,1,0.5)+1 # some yes/no
x2=rbinom(50,2,0.8)+1 # estimating factors
x3=rbinom(50,5,0.1)+1 # categorical stuff
X=cbind(1,x1,x2,x3) # design matrix
betas=cbind(rnorm(4,0,1.5)) # simulate beta
lambda=exp(X%*%betas) # calc lambda
theta=1-runif(1,0.17,0.22) # simulate theta
tau=lambda*theta # calculate tau
phi=runif(1,0,20) # simulate phi
y=rnbinom(50,phi,,tau) # simulate y
z=rbinom(50,y,p) # simulate z

# so now lets actually sample the priors
k=10000 # iterations
n=50 # sample size
# resimulate our data
Z=cbind(1,runif(50,1,20)) # bounding lbar between 1 and 20
x1=rbinom(50,1,0.5)+1 # some yes/no
x2=rbinom(50,2,0.8)+1 # estimating factors
x3=rbinom(50,5,0.1)+1 # categorical stuff
X=cbind(1,x1,x2,x3) # design matrix
v=10
s_gamma=matrix(,k,2)
s_betas=matrix(,k,4)
s_mu=matrix(,k,n)
s_lambda=matrix(,k,n)
s_alpha=matrix(,k,n)
s_beta=matrix(,k,n)
s_theta=matrix(,k,n)
s_phi=matrix(,k,n)
s_tau=matrix(,k,n)
s_p=array(,dim=c(k,n,n))
s_y=array(,dim=c(k,n,n))
s_z=array(,dim=c(k,n,n))
for(i in 1:k){
  s_gamma[i,]=rnorm(2)
  s_betas[i,]=rnorm(4)
  s_mu[i,]=1/(1+exp(-(Z%*%s_gamma[i,])))
  s_lambda[i,]=exp(X%*%s_betas[i,])
  s_alpha[i,]=s_mu[i,]*v
  s_beta[i,]=(1-s_mu[i,])*v
  s_theta[i,]=1-runif(50,0.17,0.22)
  s_phi[i,]=runif(50,0,1)
  s_tau[i,]=s_lambda[i,]*s_theta[i,]
  for(j in 1:n){
    s_p[i,j,]=rbeta(50,s_alpha[i,j],s_beta[i,j])
    s_y[i,j,]=rnbinom(50,s_phi[i,j],,s_tau[i,j])
    s_z[i,j,]=rbinom(50,s_y[i,j,],s_p[i,j,])
  }
}
