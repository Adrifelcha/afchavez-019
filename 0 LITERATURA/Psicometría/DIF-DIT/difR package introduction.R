#########################################################
#####  DIF analysis an the difR package
#####  Reference:
#####             Title: A general framework and an R package for the detection of dichotomous differential item functioning 
#####             Authors: David Magis, Sébastien Meland, Francis Tuerlinckx & Paul De Boeck

rm(list=ls())
library("difR", lib.loc="~/R/R-3.5.0/library")   

data("verbal")

Mantel_Haeszen <- difMH(Data=verbal, group="Gender",
      focal.name=1, purify=TRUE, nrIter=20)
print(Mantel_Haeszen)

plot(Mantel_Haeszen)