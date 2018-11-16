# DIF

library(mirt)

#simulate data where group 2 has a smaller slopes and more extreme intercepts
set.seed(12345)
a1 <- a2 <- matrix(abs(rnorm(15,1,.3)), ncol=1)
d1 <- d2 <- matrix(rnorm(15,0,.7),ncol=1)
a2[1:2, ] <- a1[1:2, ]/3
d1[c(1,3), ] <- d2[c(1,3), ]/4
head(data.frame(a.group1 = a1, a.group2 = a2, d.group1 = d1, d.group2 = d2))
itemtype <- rep('2PL', nrow(a1))
N <- 1000
dataset1 <- simdata(a1, d1, N, itemtype)
dataset2 <- simdata(a2, d2, N, itemtype, mu = .1, sigma = matrix(1.5))
dat <- rbind(dataset1, dataset2)
group <- c(rep('D1', N), rep('D2', N))

#### no anchors, all items tested for DIF by adding item constrains one item at a time.
# define a parallel cluster (optional) to help speed up internal functions
mirtCluster()

# Information matrix with Oakes' identity (not controlling for latent group differences)
# NOTE: Without properly equating the groups the following example code is not testing for DIF,
# but instead reflects a combination of DIF + latent-trait distribution effects
model <- multipleGroup(dat, 1, group, SE = TRUE)

#test whether adding slopes and intercepts constraints results in DIF. Plot items showing DIF
resulta1d <- DIF(model, c('a1', 'd'), plotdif = TRUE)
resulta1d

#same as above, but using Wald tests with Benjamini & Hochberg adjustment
resulta1dWald <- DIF(model, c('a1', 'd'), Wald = TRUE, p.adjust = 'fdr')
resulta1dWald
round(resulta1dWald$adj_pvals, 4)

#test whether adding only slope constraints results in DIF for all items
resulta1 <- DIF(model, 'a1')
resulta1

#following up on resulta1d, to determine whether it's a1 or d parameter causing DIF
(a1s <- DIF(model, 'a1', items2test = 1:3))
(ds <- DIF(model, 'd', items2test = 1:3))

####
# using items 4 to 15 as anchors to test for DIF after adjusting for latent-trait differences
itemnames <- colnames(dat)
model_anchor <- multipleGroup(dat, model = 1, group = group,
                              invariance = c(itemnames[4:15], 'free_means', 'free_var'))
anchor <- DIF(model_anchor, c('a1', 'd'), items2test = 1:3)
anchor

### drop down approach (freely estimating parameters across groups) when
### specifying a highly constrained model with estimated latent parameters
model_constrained <- multipleGroup(dat, 1, group,
                                   invariance = c(colnames(dat), 'free_means', 'free_var'))
dropdown <- DIF(model_constrained, 'd', scheme = 'drop')
dropdown

### sequential searches using SABIC as the selection criteria
# starting from completely different models
model <- multipleGroup(dat, 1, group)
stepup <- DIF(model, c('a1', 'd'), scheme = 'add_sequential')
stepup

#step down procedure for highly constrained model
model <- multipleGroup(dat, 1, group, invariance = itemnames)
stepdown <- DIF(model, c('a1', 'd'), scheme = 'drop_sequential')
stepdown

## End(Not run)