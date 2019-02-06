#############################################################################
# EXAMPLE 2: Examples based on dataset sim.dina
#############################################################################
# DINA Model
d1 <- CDM::din(sim.dina, q.matr=sim.qmatrix, rule="DINA",
               conv.crit=0.01, maxit=500, progress=TRUE)
summary(d1)
# DINA model with hierarchical skill classes (Hierarchical DINA model)
# 1st step: estimate an initial full model to look at the indexing
# of skill classes
d0 <- CDM::din(sim.dina, q.matr=sim.qmatrix, maxit=1)
d0$attribute.patt.splitted
# [,1] [,2] [,3]
# [1,] 0 0 0
# [2,] 1 0 0
# [3,] 0 1 0
# [4,] 0 0 1
# [5,] 1 1 0
# [6,] 1 0 1
# [7,] 0 1 1
# [8,] 1 1 1
#
# In this example, following hierarchical skill classes are only allowed:
# 000, 001, 011, 111
# We define therefore a vector of indices for skill classes with
# zero probabilities (see entries in the rows of the matrix
# d0$attribute.patt.splitted above)
zeroprob.skillclasses <- c(2,3,5,6) # classes 100, 010, 110, 101
# estimate the hierarchical DINA model
d1a <- CDM::din(sim.dina, q.matr=sim.qmatrix,
                zeroprob.skillclasses=zeroprob.skillclasses )
summary(d1a)
# Mixed DINA and DINO Model
d1b <- CDM::din(sim.dina, q.matr=sim.qmatrix, rule=
                  c(rep("DINA", 7), rep("DINO", 2)), conv.crit=0.01,
                maxit=500, progress=FALSE)
summary(d1b)
# DINO Model
d2 <- CDM::din(sim.dina, q.matr=sim.qmatrix, rule="DINO",
               conv.crit=0.01, maxit=500, progress=FALSE)
summary(d2)
# Comparison of DINA and DINO estimates
lapply(list("guessing"=rbind("DINA"=d1$guess[,1],
                             "DINO"=d2$guess[,1]), "slipping"=rbind("DINA"=
                                                                      d1$slip[,1], "DINO"=d2$slip[,1])), round, 2)
# Comparison of the information criteria
c("DINA"=d1$AIC, "MIXED"=d1b$AIC, "DINO"=d2$AIC)
# following estimates:
d1$coef # guessing and slipping parameter
d1$guess # guessing parameter
d1$slip # slipping parameter
d1$skill.patt # probabilities for skills
d1$attribute.patt # skill classes with probabilities
d1$subj.pattern # pattern per subject
# posterior probabilities for every response pattern
d1$posterior
# Equal guessing parameters
d2a <- CDM::din( data=sim.dina, q.matrix=sim.qmatrix,
                 guess.equal=TRUE, slip.equal=FALSE )
d2a$coef
# Equal guessing and slipping parameters
d2b <- CDM::din( data=sim.dina, q.matrix=sim.qmatrix,
                 guess.equal=TRUE, slip.equal=TRUE )
d2b$coef