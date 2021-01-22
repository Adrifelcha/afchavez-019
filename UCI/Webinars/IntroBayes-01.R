## Sample distribution
set.seed(1234565)
y=rnorm(12,.05,.08)
ybar <- mean(y); ybar




x <- seq(-1,1,.001)
Null <- dnorm(x,0,0.08)
plot(x,Null, type="l")

AltMod <- dnorm(x,c(-.20:0.20),0.08)
plot(x,AltMod, type="l")



modalt=function(x,ybar,s)
  dnorm(x,ybar,s))



```{r,echo=T}
prior=function(mu) 
  dunif(mu,-0.2,0.2)
p.data.g.theta = function(theta,y,n) 
  dbinom(y,n,theta)
integrand=function(theta,y,n)
  prior(theta)*p.data.g.theta(theta,y,n)
p.data=function(y,n)
  integrate(integrand,
            lower=0,
            upper=1,
            y=y,n=n)$value

print(p.data(7,10))


pred0=function(ybar,n) dnorm(ybar,0,.08/sqrt(n))
ybar=seq(-.1,.1,.001)
plot(ybar,pred0(ybar,100),typ='l')
lines(ybar,pred0(ybar,10))



prior=function(mu) 
  dunif(mu,-0.02,0.02)
pred1=function(mu,ybar,n) dnorm(ybar,mu,.08/sqrt(n))
integrand=function(mu,ybar,n)
  prior(mu)*pred1(mu,ybar,n)
predAlt=function(ybar,n)
  integrate(integrand,
            lower=-0.02,
            upper=0.02,
            ybar=ybar,n=n)$value
print(predAlt(mean(y),12))

y_bar <- seq(-0.1,.1,0.001)
R<- length(y_bar)
pred<- 1:R
for(r in 1:R){
  pred[r] <- predAlt(y_bar[r],12)
}

plot(y_bar,pred, type="l", ann=F)


ybar=seq(-.1,.1,.001)
plot(ybar,pred1(ybar,100),typ='l')
lines(ybar,pred1(ybar,10))


###InverseGamma
library(mcmc)







