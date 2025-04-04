#######################################
### bayesian heirarchical modeling ####
##### of fentanyl overdose deaths #####
#######################################

library(latex2exp)

##########################
### data prep/cleaning ###
##########################

data=read.csv("result.csv") # read in data
data$YEAR=data$YEAR-2014 # set 2014 at year 0
data$GROSSDOMESTICPRODUCT=data$GROSSDOMESTICPRODUCT/(10^5) # scale gdp to millions
dtr=subset(data,data$YEAR<5) # training data set
dts=subset(data,data$YEAR>4) # testing data set
y1tr=dtr$DEATHS # death counts (train)
y1ts=dts$DEATHS # death counts (test)
X=matrix(1,nrow(dtr),4) # empty design matrix
# scaling the entire design matrix (sans intercept) by 10
X[,2]=as.numeric(as.factor(dtr$STATE))/10 # add in states as numerics of factors
X[,3]=dtr$GROSSDOMESTICPRODUCT/10 # add in gdp
X[,4]=dtr$PERCENTBACHELORSDEGREEORHIGHER/10 # add in bachelors degree rate
Y=y1tr # current response

################################
### mcmc/data structure prep ###
################################

n=length(Y) # sample size
P=ncol(X) # number of parameters

mcmc=10^4 # mcmc iterations

# empty save vectors
phisave=rep(0,mcmc) 
psave=rep(0,mcmc)

# empty save matrices
musave=matrix(0,n,mcmc) 
gsave=matrix(0,P,mcmc)
Nsave=matrix(0,n,mcmc) 

#####################################
### priors and initial conditions ### 
#####################################

gmean=rep(P,0) # mean of gamma
gsd=sqrt(100000) # stdv of gamma
alphaphi=1 # lower bound of phi
theta=10 # upper bound of phi
p=0.3 # initial p
alpha0=1 # initial alpha 
beta0=4 # initial beta
phi=10 # initial phi
phitune=0.05 # tuning parameter of phi
N=round((Y+1)/p) # initial N
gamma=solve(t(X)%*%X)%*%t(X)%*%log(N) # initial gamma
gtune=0.05 # tuning parameter of gamma
mu=exp(X%*%gamma) # initial mu
const=1 # constant value added in poisson random walk
# alpha parameter for sampling p
alphastar=sum(Y)+alpha0 # outside of loop as it doesnt depend on any r.v.s
ymax=max(Y) # max value of Y to constrain N

##############################
### mcmc and gibbs sampler ###
##############################

set.seed(78) # reproducibility seed

for(i in 1:mcmc){
  # propose N
  Nstar=rpois(n,N+const)
  
  # mh ratio for N
  mhn1=dbinom(Y,Nstar,p,T)+dnbinom(Nstar,phi,,mu,T)+dpois(N,Nstar+const,T)  # numer
  mhn2=dbinom(Y,N,p,T)+dnbinom(N,phi,,mu,T)+dpois(Nstar,N+const,T)  # denom
  mhn=mhn1-mhn2 # total
  keep=((mhn>runif(n)) & (Nstar>=ymax)) # mh/bounds test for N
  N[keep]=Nstar[keep] # keep those within test
  
  # propose gamma
  gammastar=rnorm(P,gamma,gtune)
  # calculate resulting mu
  mustar=exp(X%*%gammastar)
  
  # mh ratio for gamma 
  mhg1=sum(dnbinom(N,phi,,mustar,T))+sum(dnorm(gammastar,gmean,gsd,T)) # numer
  mhg2=sum(dnbinom(N,phi,,mu,T))+sum(dnorm(gamma,gmean,gsd,T)) # denom
  mhg=mhg1-mhg2 # total
  if(mhg > runif(1)){ # mh test for gamma
    gamma=gammastar # keep gamma
    mu=mustar # keep mu
  }
  
  # propose phi
  phistar=rnorm(1,phi,phitune)
  
  # mh ratio for phi
  mhphi1=sum(dnbinom(N,phistar,,mu,T))+sum(dgamma(phistar,alphaphi,,theta,T)) # numer
  mhphi2=sum(dnbinom(N,phi,,mu,T))+sum(dgamma(phi,alphaphi,,theta,T)) # denom
  mhphi=mhphi1-mhphi2 # total
  if(mhphi > runif(1)){ # mh test for phi
    phi=phistar # keep phi
  }
  
  # new beta, inside loop as it depends on N
  betastar=(sum(N-Y))+beta0 
  p=rbeta(1,alphastar,betastar) # alpha parameter for sampling p
  
  # retain outputs
  psave[i]=p
  phisave[i]=phi
  Nsave[,i]=N
  gsave[,i]=gamma
  musave[,i]=mu
}

##################################
### finalization/visualization ###
##################################

# throw out burn in samples
burn_in=(1/10)*mcmc # generalization, can be changed

# package outputs into one list
output=list(gsave=gsave,Nsave=Nsave,psave=psave,musave=musave,phisave=phisave)

# probability of fentanyl specific OD
plot(output$psave,type="l",xlab="Iterations",ylab=TeX("$\\[p|\\cdot\\]$"))
quantile(output$psave,c(0.025,0.975))

# dispersion parameter
plot(output$phisave,type="l",xlab="Iterations",ylab=TeX("$\\[\\phi|\\cdot\\]$"))
quantile(output$phisave,c(0.025,0.975))

# regression coefficients
plot(output$gsave[1,],type="l",xlab="Iterations",ylab=TeX("$\\[\\gamma_0|\\cdot\\]$"))
plot(output$gsave[2,],type="l",xlab="Iterations",ylab=TeX("$\\[\\gamma_1|\\cdot\\]$"))
plot(output$gsave[3,],type="l",xlab="Iterations",ylab=TeX("$\\[\\gamma_2|\\cdot\\]$"))
plot(output$gsave[4,],type="l",xlab="Iterations",ylab=TeX("$\\[\\gamma_3|\\cdot\\]$"))
apply(output$gsave,1,mean)
apply(output$gsave,1,quantile,c(0.025,0.975))


apply(output$Nsave,1,mean)
apply(output$musave,1,mean)
