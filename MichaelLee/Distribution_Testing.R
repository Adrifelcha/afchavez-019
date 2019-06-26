
# 
#distrib <- rgamma(1000000,1,11)
distrib <- rnorm(1000000,0.8,0.5)
#distrib <- rexp(1000000,)
plot(density(distrib))
#lines(c(0,0),c(0,10))




lambdad_B <- rgamma(1000000,.001,.001)
sigmac_A <- 1/sqrt(lambdad_B)
plot(density(sigmac_A), xlim=c(0,5))






density <- ddirichlet(c(.1,.2,.7), c(1,1,1))
draws <- rdirichlet(20, c(1,1,1) )

library(extraDistr)
unif_dis <- rdunif(1000000,1,3)
plot(density(unif_dis), xlim=c(0,5))


