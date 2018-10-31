##############################################
##############################################
# Cronbach's alpha
rm(list=ls())
###############################################


###############################################
# Simulate data
###############################################
set.seed(123)  #We fix our environment so it always generates the same random values

K <- 100         # items
n_obs <- 200      # participants
dta <- array(dim=c(n_obs,K))      #Empty array to hold all k's and n_obs

######## Fixing the probabilities of getting a specific response within a 4-level scale
for(ii in 1:K){                        # iterate to fill BY ITEM
  if(ii %in% c(5,10,15)){                     # these are "strange" items
   prob_vector <- c(.28,.27,.23,.32)           # with lower probabilities
    prob_abnormal <- prob_vector
  }else{
    prob_vector <- c(.03,.07,.11,.79)  #probabilities for 'regular' items
    prob_normal <- prob_vector
  }
  dta[,ii] <- sample(c(1:4),size=n_obs,replace = T,prob = prob_vector)
  # Sample #n_obs responses within the range 1:4 with prob_vector probability per level, allowing replacement per extraction
}
scores <- apply(dta,MARGIN=1,FUN=sum)    #We obtain the total scores by summing each row


###############################################
# We plot the proportion of 1,2,3,4 per subject 
###############################################
# 17 "high" items, 3 "low" items:
plot(NULL,xlim=c(1,4),ylim=c(0,1), ylab="Proportion", xlab="Response", axes=FALSE) #Empty plot
for(ii in 1:K){               #Insert a line per item
  tb <- table(dta[,ii])       #Frequency of 1,2,3,4 observed (per item)
  lines(as.numeric(names(tb)),tb/n_obs,       #Lines that show the relation between each step and the proportion at which it appears
        lwd=1.5,col=c('red','blue')[(!ii%in%c(5,10,15))+1])
  axis(1, c(1,2,3,4), c("1", "2", "3", "4"))
  axis(2, c(0,0.25,0.5,0.75,1), c("0","0.25", "0.5", "0.75", "1"))
}

layout(matrix(1:2,ncol=2))
barplot(prob_normal, ylim=c(0,1), main="Usual probabilities", col="turquoise",
        ylab="Probabilities", xlab="Responses", cex.lab=1.5, font.lab=6)
axis(1, c(0.65,1.88,3.1,4.25), c("1", "2", "3", "4"))
barplot(prob_abnormal, ylim=c(0,1), main="Atypical probabilities", col="indianred", 
        xlab="Responses", cex.lab=1.5, font.lab=6) 
axis(1, c(0.65,1.88,3.1,4.25), c("1", "2", "3", "4"))

############################################
# Cronbach's alpha as implemented in 'psych'
############################################
library('psych')             #Library for personality, psychometric theory and experimental psychology

# Cronbach's alpha from the package
alpha_psych <- alpha(dta, check.keys=TRUE)    #We run a Cronbach's alpha on our data set (all responses)
alpha_psych