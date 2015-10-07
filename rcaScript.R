## Total of 820 unique tech mix and 2877 seller remain after cleaning###
## too many as of now. MST might take care of reducing the sample##
source('~/Documents/R/bootstrap.R')
source('~/Documents/R/clustering/fnsBipartite.R')
source('~/Documents/R/clustering/fnsRCA.R')
library(vegan)
load("CSIplus_yearly_tech.Rdata")
yearly_tech[,length(unique(tech_mix)),by=year]
yearly_tech[,length(unique(seller)),by=year]

## Time periods - between 2003-04,05-06, 07-08,09-10,11-12##
##RCA matrix one ##
sim_mat = lapply(c(2003,2005,2007,2009,2011),function(timep) getRCAmatrix(yearly_tech,timep))

library(reshape2)

A = melt(sim_mat[[5]])
A = A[order(A$value,decreasing = T),]
component = c(1:length(unique(A$Var1)))
max_tree  = A[1,]
for(i in 1:nrow(A)){
  if( component[as.numeric(A$Var1[i])]!= component[A$Var2[i]] & nrow(max_tree)<=length(unique(A$Var2))){
    max_tree = rbind(max_tree,A[i,])
    component[c(as.numeric(A$Var1[i]),A$Var2[i])][
      which.max(component[c(as.numeric(A$Var1[i]),A$Var2[i])])] = min(component[c(as.numeric(A$Var1[i]),A$Var2[i])])
  }
}

max_tree = max_tree[-1,]
max_graph = rbind(max_tree,A[as.numeric(A$Var1)!=A$Var2&A$value>0.5,])
max_graph$Var1 = as.numeric(max_graph$Var1)
