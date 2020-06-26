# A jewelry store that serves just one customer at a time is concerned about the safety of its isolated customers.
# The store does some research and learns that:
# 10% of the times that a jewelry store is robbed, a customer is in the store.
# A jewelry store has a customer on average 20% of each 24-hour day.
# The probability that a jewelry store is being robbed (anywhere in the world) is 1 in 2 million.
# What is the probability that a robbery will occur while a customer is in the store?
  

robbery <- 1/2000000
C_R <- 0.1
customer <- 0.2

upper <- robbery * C_R

down <- (robbery * C_R) + ((1-robbery) * customer)
down_2 <- 0.2

a <- 1/500000
b <- 1/2000000
c <- 1/4000000
d <- 1/5000000
print(c(a,"-",b,"-",c,"-",d))

upper/down
upper/down_2

############################
#If I flip a fair coin, with heads and tails, ten times in a row, what is the probability that I will get exactly six heads?
bin_coef <- (factorial(10)) / ((factorial(4)) * (factorial(6)))
bin_1 <- bin_coef * (0.5)^6 * (0.5)^4 
bin_1


#############################
#If a coin is bent so that it has a 40\%40% probability of coming up heads, what is the probability of getting exactly 6 heads in 10 throws?
bin_coef <- (factorial(10)) / ((factorial(4)) * (factorial(6)))
bin_2 <- bin_coef * (0.4)^6 * (0.6)^4 
bin_2

################################
#A bent coin has 40\%40% probability of coming up heads on each independent toss. If I toss the coin ten times, what is the probability that I get at least 8 heads?
bin_coef <- (factorial(10)) / ((factorial(2)) * (factorial(8)))
bin_a <- bin_coef * (0.4)^8 * (0.6)^2 
bin_a
bin_coef <- (factorial(10)) / ((factorial(1)) * (factorial(9)))
bin_b <- bin_coef * (0.4)^9 * (0.6)^1 
bin_coef <- (factorial(10)) / ((factorial(0)) * (factorial(10)))
bin_c <- bin_coef * (0.4)^10 * (0.6)^0 

bin_sum <- bin_a + bin_b + bin_c
bin_sum


###############################
# Suppose I have a bent coin with a 60\%60% probability of coming up heads. I throw the coin ten times and it comes up heads 8 times.
# What is the value of the "likelihood" term in Bayes' Theorem -- the conditional probability of the data given the parameter.
bin_coef <- (factorial(10)) / ((factorial(2)) * (factorial(8)))
bin_2 <- bin_coef * (0.6)^8 * (0.4)^2 
bin_2


##################################
#We have the following information about a new medical test for diagnosing cancer.
#Before any data are observed, we know that 5\%5% of the population to be tested actually have Cancer.
#Of those tested who do have cancer, 90\%90% of them get an accurate test result of "Positive" for cancer. The other 10% get a false test result of "Negative" for Cancer.
#Of the people who do not have cancer, 90\%90% of them get an accurate test result of "Negative" for cancer. The other 10% get a false test result of "Positive" for cancer.
#What is the conditional probability that I have Cancer, if I get a "Positive" test result for Cancer?
#  **Formulas in the feedback section are very long, and do not fit within the standard viewing window. Therefore, the font is a bit smaller and the word "positive test" has been abbreviated as PT.

cancer <- 0.05
TPos <- 0.9
FPos <- 0.1
TNeg <-0.9
FNeg <- 0.1

Upper <- cancer * TPos
Down_1 <- ((TPos * cancer) + (FPos * (1-cancer)))

  
Upper/Down_1



##################################
#We have the following information about a new medical test for diagnosing cancer.
#Before any data are observed, we know that 8\%8% of the population to be tested actually have Cancer.
#Of those tested who do have cancer, 90\%90% of them get an accurate test result of "Positive'' for cancer.
#The other 10\%10% get a false test result of "Negative'' for Cancer.
#Of the people who do not have cancer, 95\%95% of them get an accurate test result of "Negative'' for cancer.
#The other 5\%5% get a false test result of "Positive'' for cancer.
cancer <- 0.08
TPos <- 0.9
FPos <- 0.05
TNeg <-0.95
FNeg <- 0.1

Upper <- cancer * FNeg
Down_1 <- ((FNeg * cancer) + (TNeg * (1-cancer)))

Upper/Down_1


#############################
#An urn contains 50 marbles - 40 blue and 10 white. After 50 draws, exactly 40 blue and 10 white are observed.
#You are not told whether the draw was done "with replacement" or "without replacement."
#What is the probability that the draw was done with replacement?



###############################
#According to Department of Customs Enforcement Research: 99\%99% of people crossing into the United States are not smugglers.
#The majority of all Smugglers at the border (65\%65%) appear nervous and sweaty.
#Only 8\%8% of innocent people at the border appear nervous and sweaty.
#If someone at the border appears nervous and sweaty, what is the probability that they are a Smuggler?

Smuggler <- 1-0.99
Nerv_Smugg <- 0.65
Nerv_Inocent <- 0.08

Upper <- Smuggler * Nerv_Smugg
Down <- (Smuggler * Nerv_Smugg) + ((1-Smuggler) * Nerv_Inocent)

Upper/Down

