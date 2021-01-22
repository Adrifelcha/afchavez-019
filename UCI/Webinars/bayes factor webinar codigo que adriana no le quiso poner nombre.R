## Relative Strength of Models ($sigma^2=1$):

```{r}
bf=function(ybar,n,b)
  dnorm(ybar,0,sqrt(b+1/n))/dnorm(ybar,0,sqrt(1/n))


par(cex=1.5)
ybar=seq(-1,1,.01)
plot(ybar,log10(bf(ybar,1,1)),typ='l',ylim=c(-3,3),
     axes=F,xlab="Sample Mean",ylab="Bayes Factor",lwd=2)
lines(ybar,log10(bf(ybar,5,1)),lwd=2,col='darkgreen')
lines(ybar,log10(bf(ybar,10,1)),lwd=2,col='red')
lines(ybar,log10(bf(ybar,20,1)),lwd=2,col='blue')
lines(ybar,log10(bf(ybar,30,1)),lwd=2,col='darkblue')
lines(ybar,log10(bf(ybar,60,1)),lwd=2,col='black')
lines(ybar,log10(bf(ybar,100,1)),lwd=2,col='gray')
abline(h=log10(c(1/3,1,3)),lty=2)
abline(v=mean(y))
axis(1)
axis(2,at=-3:3,lab=10^(-3:3))
par(xpd=T)
text(1.03,3.4,"n=1",adj=0)
text(1.03,2.8,"n=100",adj=0,col="darkblue")
text(1.03,2.0,"n=1e4",adj=0,col="darkgreen")
text(1.03,3.4,"n=1",adj=0)
text(1.03,2.8,"n=100",adj=0,col="darkblue")
text(1.03,2.0,"n=1e4",adj=0,col="darkgreen")
text(1.03,3.4,"n=1",adj=0)
```

```{r}
bf=function(ybar,n,b)
  dnorm(ybar,0,sqrt(b+1/n))/dnorm(ybar,0,sqrt(1/n))

par(cex=1.5)
n=seq(0,100,.1)
plot(n,log10(bf(-.5,n,1)),typ='l',ylim=c(-3,3),
     axes=F,xlab="Sample Size",ylab="Bayes Factor",lwd=2)
lines(n,log10(bf(-.5,n,100)),lwd=2,col='darkblue')
lines(n,log10(bf(-.5,n,10000)),lwd=2,col='darkgreen')
abline(h=log10(c(1/3,1,3)),lty=2)
axis(1)
axis(2,at=-3:3,lab=10^(-3:3))
par(xpd=T)
text(1.03,3.4,"b=1",adj=0)
text(1.03,2.8,"b=100",adj=0,col="darkblue")
text(1.03,2.0,"b=1e4",adj=0,col="darkgreen")

## Your Turn

- The above plot is for fixed sample size as a function of sample mean for a few values of $b$.

- Let's fix $\bar{Y}= -.5$, $\sigma^2=1$.

- Make plots showing the effects of $n$ (sample size) for a few values of $b$.

