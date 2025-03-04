set.seed(69) # nice
# data
T=100 # structure against time steps
t=1:T # index time
Phi=10 # some random phi
Mu=rnorm(T,6,1) # and mu to generate count data from nb
y=rnbinom(T,size=Phi,mu=Mu) # data should be counts
x1=rnorm(T,6,1) # covariates
x2=rbeta(T,1,1) # btw all of this data is bad
x3=(rnorm(T)+runif(T))^2 # no shot this is a good approximation
X=cbind(1,x1,x2,x3) # placed into design matrix
# the model works though

# initialized variables and priors
k=10000 # number of mcmc
p=rep(0.20,k) # prior p vector
phi=rep(10,k) # prior phi vector
z=matrix(10,k,T) # z prior matrix
alpha=1 # alpha prior
beta=4 # beta prior
b_init=solve(t(X)%*%X)%*%t(X)%*%log(y) # initial beta
b=matrix(rep(b_init,k),k,dim(X)[2],T) # beta prior matrix
mu_init=exp(X%*%b_init) # inital mu
mu=matrix(mu_init,k,T) # mu prior matrix
b_mn=0 # beta mean hyper prior
b_sd=10 # beta sd hyper prior
phi_min=0.1 # phi min hyper prior
phi_max=10 # phi max hyper prior

for(i in 2:k){
  # zt|yt ~ binom(yt,pt)
  # propose value of zt
  zstar=z[i-1,] # initialize zstar as current z
  for(t in 1:T){ # across time
    # given that our values are within support and non-error
    if(!is.na(y[t])&&!is.na(p[i-1])&&y[t]>=0&&p[i-1]>0&&p[i-1]<1){
      zprop=rbinom(1,y[t],p[i-1]) # propose z
      if(zprop>=0&&zprop<=y[t]){ # if proposal is within support
        mh1=dbinom(zprop,y[t],p[i-1],log=T) # density of z at proposed value
        mh2=dbinom(z[i-1,t],y[t],p[i-1],log=T) # density of current z

        if(log(runif(1)<mh1-mh2)){ # check against mh ratio
          zstar[t]=zprop # save value
        }
      }
    }
  }
  z[i,]=zstar # save new z values

  # yt|mu_t,phi_t ~ nb(mu_t,phi_t) (feeties)
  # b|b_mu,b_sig ~ n(b_mu,b_sig)
  bstar=rnorm(dim(X)[2],b_mn,b_sd) # propose value of b
  mustar=exp(X%*%bstar) # propose value of mu
  mh1=sum(dbinom(z[i,],y,p[i-1],log=T))+ # density of zt at saved values
    sum(dnbinom(y,size=phi[i-1],mu=mustar,log=T))+ # density of yt at proposed value of mu
    sum(dnorm(bstar,b_mn,b_sd,log=T)) # density of proposed beta values
  mh2=sum(dbinom(z[i,],y,p[i-1],log=T))+ # density of zt at saved values
    sum(dnbinom(y,size=phi[i-1],mu=mu[i-1,],log=T))+ # density of yt at current mu
    sum(dnorm(b[i-1,],b_mn,b_sd,log=T)) # density of current beta
  mh=exp(mh1-mh2) # total mh ratio
  b[i,]=ifelse(mh>runif(1),bstar,b[i-1,]) # save b
  mu[i,]=ifelse(mh>runif(1),mustar,mu[i-1,]) # save mu

  # phi_t|a_phi,b_phi ~ unif(a_phi,b_phi) (feeties)
  phistar=runif(1,phi_min,phi_max) # propose value of phi
  mh1=sum(dbinom(z[i,],y,p[i-1],log=T))+ # density of zt at saved values
    sum(dnbinom(y,size=phistar,mu=mu[i,],log=T))+ # density of yt at proposed value of phi
    dunif(phistar,phi_min,phi_max,log=T) # density of proposed phi
  mh2=sum(dbinom(z[i,],y,p[i-1],log=T))+ # density of zt at saved values
    sum(dnbinom(y,size=phi[i-1],mu=mu[i,],log=T))+ # density of yt at current value of phi
    dunif(phi[i-1],phi_min,phi_max) # density of current phi
  mh=exp(mh1-mh2) # total mh ratio
  phi[i]=ifelse(mh>runif(1),phistar,phi[i-1]) # save phi

  # pt|alpha,beta ~ beta(alpha,beta)
  pstar=rbeta(1,alpha,beta) # propose value of p
  mh1=sum(dbinom(z[i,],y,pstar,log=T))+ # density of zt at proposed p
    sum(dnbinom(y,size=phi[i],mu=mu[i,],log=T))+ # density of yt at saved values
    dbeta(pstar,alpha,beta,log=T) # density of proposed p
  mh2=sum(dbinom(z[i,],y,p[i-1],log=T))+ # density of zt at current p
    sum(dnbinom(y,size=phi[i],mu=mu[i,],log=T))+ # density of yt at saved values
    dbeta(p[i-1],alpha,beta,log=T) # density of current p
  mh=exp(mh1-mh2) # total mh ratio
  p[i]=ifelse(mh>runif(1),pstar,p[i-1]) # save p
}

burn=1000 # burn discards
# save outputs into list structure
save=list(Z_tild=z[-burn],B_tild=b[-burn],Mu_tild=mu[-burn],Phi_tild=phi[-burn],P_tild=p[-burn])
# histograms and traceplots of output
par(mfrow=c(2,2))
plot(save$Z_tild,type="l")
hist(save$Z_tild)
plot(save$Phi_tild,type="l")
hist(save$Phi_tild)

par(mfrow=c(2,2))
plot(save$B_tild,type="l")
hist(save$B_tild)
plot(save$P_tild,type="l")
hist(save$P_tild)

plot(save$Mu_tild,type="l")
hist(save$Mu_tild)
