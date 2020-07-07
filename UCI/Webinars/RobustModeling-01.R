criteria <- seq(-6,6,0.1)

prob <- function(mean,sd,criteria){
  Tabla <- NULL
  Tabla[1] <- pnorm(criteria[1],mean,sd)
 for(a in 1:length(criteria)){
  Tabla[1+a]<- pnorm(criteria[a+1],mean,sd) - pnorm(criteria[a],mean,sd)}
 Tabla[length(criteria)+1] <- 1 - pnorm(criteria[length(criteria)],mean,sd)
 return(Tabla)
}


SDT <- function(dprime,sd_signal,criteria){
  Tabla_N <- NULL
  Tabla_N[1] <- pnorm(criteria[1],0,1)
  Tabla_S <- NULL
  Tabla_S[1] <- pnorm(criteria[1],dprime,sd_signal)
  for(a in 1:length(criteria)){
    Tabla_N[1+a]<- pnorm(criteria[a+1],0,1) - pnorm(criteria[a],0,1)
    Tabla_S[1+a]<- pnorm(criteria[a+1],dprime,sd_signal) - pnorm(criteria[a],dprime,sd_signal)}
  Tabla_N[length(criteria)+1] <- 1 - pnorm(criteria[length(criteria)],0,1)
  Tabla_S[length(criteria)+1] <- 1 - pnorm(criteria[length(criteria)],dprime,sd_signal)
  TotalTable <- rbind(Tabla_N,Tabla_S)
  return(TotalTable)
  }

d1 <- 1
d2 <- 2
d3 <- 3
sds <- 0.5
X <- SDT(d1,1,criteria)
X2 <- SDT(d2,1,criteria)
X3 <- SDT(d3,1,criteria)
Y <- SDT(d1,sds,criteria)
Y2 <- SDT(d2,sds,criteria)
Y3 <- SDT(d3,sds,criteria)

plot(cumsum(X[2,]), cumsum(X[1,]), type="l", col="red", lwd=2, 
     ylab="F.A. rate", xlab="Hit rate")
lines(cumsum(X2[2,]), cumsum(X2[1,]), col="blue", lwd=2)
lines(cumsum(X3[2,]), cumsum(X3[1,]), col="green", lwd=2)
lines(cumsum(Y[2,]), cumsum(Y[1,]), col="red", lwd=2, lty=2)
lines(cumsum(Y2[2,]), cumsum(Y2[1,]), col="blue", lwd=2, lty=2)
lines(cumsum(Y3[2,]), cumsum(Y3[1,]), col="green", lwd=2, lty=2)
lines(seq(0,1,.01),seq(0,1,.01), lty=2)
lines(c(.7,.75),c(0.4,0.4), col="red")
lines(c(.7,.75),c(0.35,0.35), col= "blue")
lines(c(.7,.75),c(0.3,0.3), col= "green")
text(0.8,0.4, paste("d' =", d1))
text(0.8,0.35, paste("d' =", d2))
text(0.8,0.3, paste("d' =", d3))
lines(c(.6,.65),c(0.25,0.25), col= "black", lty=2)
lines(c(.6,.65),c(0.2,0.2), col= "black")
text(0.8,0.25, paste("SD(Signal) =", sds))
text(0.8,0.2, paste("SD(Signal) = 1"))






### Gamma SDT


SDT <- function(shape, rate_s,rate_n,criteria){
  Tabla_N <- NULL
  Tabla_N[1] <- pgamma(criteria[1],shape,rate_s)
  Tabla_S <- NULL
  Tabla_S[1] <- pgamma(criteria[1],shape,rate_n)
  for(a in 1:length(criteria)){
    Tabla_N[1+a]<- pgamma(criteria[a+1],shape,rate_n) - pgamma(criteria[a],shape,rate_n)
    Tabla_S[1+a]<- pgamma(criteria[a+1],shape,rate_s) - pgamma(criteria[a],shape,rate_s)}
  Tabla_N[length(criteria)+1] <- 1 - pgamma(criteria[length(criteria)],shape,rate_n)
  Tabla_S[length(criteria)+1] <- 1 - pgamma(criteria[length(criteria)],shape,rate_s)
  TotalTable <- rbind(Tabla_N,Tabla_S)
  return(TotalTable)
}

criteria <- seq(0,6,0.1)
shape <- 1.2
X <- SDT(shape,0.1,1,criteria)
X2 <- SDT(shape,0.3,1,criteria)
X3 <- SDT(shape,0.5,1,criteria)
X4 <- SDT(shape,1,1.1,criteria)
X5 <- SDT(shape,1,1.3,criteria)
X6 <- SDT(shape,1,1.5,criteria)

plot(cumsum(X[2,]), cumsum(X[1,]), type="l", col="red", lwd=2, 
     ylab="F.A. rate", xlab="Hit rate")
lines(cumsum(X2[2,]), cumsum(X2[1,]), col="blue", lwd=2)
lines(cumsum(X3[2,]), cumsum(X3[1,]), col="green", lwd=2)
lines(cumsum(X4[2,]), cumsum(X4[1,]), col="purple", lwd=2, lty=2)
lines(cumsum(X5[2,]), cumsum(X5[1,]), col="orange", lwd=2, lty=2)
lines(cumsum(X6[2,]), cumsum(X6[1,]), col="gray", lwd=2, lty=2)
lines(seq(0,1,.01),seq(0,1,.01), lty=2)
lines(seq(0,1,.01),1-seq(0,1,.01), lty=3)
lines(c(.7,.75),c(0.4,0.4), col="red")
lines(c(.7,.75),c(0.35,0.35), col= "blue")
lines(c(.7,.75),c(0.3,0.3), col= "green")
text(0.8,0.4, paste("d' =", d1))
text(0.8,0.35, paste("d' =", d2))
text(0.8,0.3, paste("d' =", d3))
lines(c(.6,.65),c(0.25,0.25), col= "black", lty=2)
lines(c(.6,.65),c(0.2,0.2), col= "black")
text(0.8,0.25, paste("SD(Signal) =", sds))
text(0.8,0.2, paste("SD(Signal) = 1"))







