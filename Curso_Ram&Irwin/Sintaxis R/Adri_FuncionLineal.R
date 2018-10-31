layout(matrix(1:2,ncol=2))

a <- 1
b <- 0

x <- seq(0, 1, by=0.01)   
f_a <- (a*x) + 0
f_b <- (1*x) + b
plot(x, f_a, type="l", lwd=2, col="red", main="Pendiente (a) Variable", xlab="Variable independiente", ylab="Variable dependiente")  
mtext("b = 0")
a<- 2
f_a <- (a*x) + 0
lines(x,f_a, lwd=2, col="blue")
a<- 0.5
f_a <- (a*x) + 0
lines(x,f_a, lwd=2, col="forestgreen")
legend(0.05, 0.9, c("a = 1", "a = 2", "a = 0.5"), lty=c(1,1), lwd=2, col=c("red", "blue", "forestgreen"))     

plot(x, f_b, type="l", lwd=2, col="red", main="Intercepto (b) Variable", xlab="Variable independiente", ylab="Variable dependiente")  
mtext("a = 1")
b<- 0.1
f_b <- (1*x) + b
lines(x,f_b, lwd=2, col="blue")
b<- 0.2
f_b <- (1*x) + b
lines(x,f_b, lwd=2, col="forestgreen")
legend(0.05, 0.9, c("b = 0", "b = 0.1", "b = 0.2"), lty=c(1,1), lwd=2, col=c("red", "blue", "forestgreen"))    
