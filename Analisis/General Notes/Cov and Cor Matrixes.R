##############################################
##############################################
# Correlation and Covariance matrix
# Original by José Luis Baroja
# Actual version by: Adriana F. Chávez
rm(list=ls())
###############################################
K <- 5

dta <- array(dim=c(10,5))  

for(ii in 1:K){
dta[,ii] <- sample(c(1:4),size=5,replace = T,prob = c(.2,.4,.6,.8))}
  # Sample #n_obs responses within the range 1:4 with prob_vector probability per level, allowing replacement per extraction
scores <- apply(dta,MARGIN=1,FUN=sum)    #We obtain the total scores by summing each row

# Cronbach's alpha from scratch 2
cor_mat <- cor(dta) # correlation matrix based on our data
cov_mat <- cov(dta) # covariance matrix based on our data
non_redundant_cor <- NULL 
non_redundant_cov <- NULL
for(ci in 1:(nrow(cor_mat)-1)){
  non_redundant_cor <- append(non_redundant_cor,
                              cor_mat[ci,(ci+1):K])
  non_redundant_cov <- append(non_redundant_cov,
                              cov_mat[ci,(ci+1):K])
}

cov_mat
non_redundant_cov
cor_mat
non_redundant_cor

cov2cor(cov_mat)
cor_mat