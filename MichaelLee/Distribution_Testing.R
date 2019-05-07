
# 
#distrib <- rgamma(1000000,1,11)
distrib <- rnorm(1000000,0,0.5)
#distrib <- rexp(1000000,)
plot(density(distrib))
#lines(c(0,0),c(0,10))