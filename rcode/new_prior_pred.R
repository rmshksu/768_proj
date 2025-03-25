library(mvtnorm)
data=read.csv("result.csv")


x1=rnorm(250,10000,10000)
x2=rnorm(250,30000,100000)
x3=rnorm(250,50000,1000)
x4=rnorm(250,50,10)
x5=rnorm(250,30,20)
X=cbind(1,x1,x2,x3,x4,x5)
y=rnbinom(250,1,,10000)
y

t=5
n=dim(X)[2]
k=1000
alpha=1
beta=4
a=0.01
b=10
phi_save=matrix(,k,250)
mu_save=matrix(,k,250)
p_save=matrix(,k,250)
y_save=matrix(,k,250)
z_save=matrix(,k,250)
betas_save=matrix(,k,n)
for(i in 1:k){
 phi_save[i,]=runif(250,a,b)
 p_save[i,]=rbeta(250,alpha,beta)
 betas_save[i,]=rnorm(6,0,0.5)
 mu_save[i,]=exp(scale(X)%*%betas_save[i,])
 y_save[i,]=rnbinom(250,phi_save[i,],,mu_save[i,])
 z_save[i,]=rbinom(250,y_save[i,],p_save[i,])
}
