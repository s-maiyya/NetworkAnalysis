###Relevance detection algorithm in bipartite graphs ###
##First routine - exact relevance ###
graph = delete.vertices(graph,which(V(graph)$name==""&V(graph)$name=="_"))
E(graph)$weight <- 1 
w_graph = simplify(graph, edge.attr.comb=list(weight="sum"))
w_graph = delete.vertices(w_graph,degree(w_graph)==0)
# edge_list = get.edgelist(w_graph)
# edge_list = as.data.frame(edge_list)
# edge_list$V1 = as.character(M$V1)
# edge_list$V2 = as.character(M$V2)
# edge_list$weight = E(w_graph)$weight
# M = matrix(0,nrow =length(V(w_graph)[V(w_graph)$type]),
#            ncol = length(V(w_graph)[!V(w_graph)$type]))
years = c("y_2007","y_2008","y_2009","y_2010","y_2011","y_2012","y_2013","y_2014")
for(yr in years){ for(j in 1:5 ) getResemblance(yr, yearly_graphs,j)}
getResemblance <- function(y_name,yearly_graphs,n){
w_graph = yearly_graphs[[y_name]][[n]]
Ma = get.adjacency(w_graph,type="both",attr = "weight")
M = Ma[which(rowSums(as.matrix(Ma))!=0),]
M = M[,which(colSums(as.matrix(M))!=0)]
###for finding tech_mix similarities use M = t(M)##
M = (as.matrix(M))
P = as.matrix(M)/rowSums(as.matrix(M))
Pt = t(as.matrix(M))/colSums(as.matrix(M))
rm(Ma)

exactRelevance <- function(a,P,Pt){
  c_prob = 0.15
  eps = 0.005
  ua = rep(0.2,ncol(P)+nrow(P))
  ua_new = rep(0.3,ncol(P)+nrow(P))
  qa = rep(0,ncol(P)+nrow(P))
  qa[a] = 1
  while(mean(abs(ua-ua_new))>eps)
  {
    ua = ua_new
    ua_new = (1-c_prob)*c((P)%*%as.matrix(ua[nrow(P)+1:ncol(P)],ncol=1), (Pt)%*%as.matrix(ua[1:nrow(P)])) +
      c_prob*qa
    print(range(ua_new))
  }
  return(ua_new[1:nrow(P)])
}

library(doParallel)
library(plyr)

nodes <- detectCores()
cl <- makeCluster(nodes)
registerDoParallel(cl)
exact_res = ldply(1:nrow(P),.fun = function(a){exactRelevance(a,P,Pt)},.parallel = T,
                  .paropts=list(.export=c('P','Pt','exactRelevance'),
                                       .packages=.packages()))
stopCluster(cl)

names(exact_res) = rownames(M)
write.csv(exact_res,paste0(y_name,"Res_",n,".csv"))
return(exact_res)
}



thresh = 0.05
res_order = lapply(exact_res_tech,function(x){order(x,decreasing=T)[sort(x,decreasing=T)>thresh][-1]})
Ncol = max(sapply(res_order,length))
similar_sellers = ldply(res_order,function(x)
{if(length(x)==0) return(rep(as.character(NA),5))
 return(c(names(res_order)[x],rep(as.character(NA), 5-length(x))))})
similar_sellers$seller = NULL
  
write.csv(similar_sellers,
          file="/Users/sindhumaiyya/Documents/Work/Graph_based/graph_similarity_clustring/results/similar_sellers.csv")
similar_tech = ldply(res_order,function(x)
{if(length(x)==0) return(rep(as.character(NA),Ncol))
 return(c(names(res_order)[x],rep(as.character(NA), Ncol-length(x))))})
similar_tech$tech_mix = NULL

write.csv(similar_tech,
          file="/Users/sindhumaiyya/Documents/Work/Graph_based/graph_similarity_clustring/results/similar_tech.csv")


###Anomaly calculation
names(exact_res_tech)[order(exact_res_tech$SunPower_SunPower,decreasing = T)[1:10]]
NS = as.matrix(exact_res_tech[neighbors(w_graph,v =8,mode="out")-2610,neighbors(w_graph,v =8,mode="out")-2610])
NS = as.matrix(exact_res_tech[neighbors(w_graph,v =8,mode="out")-2610,neighbors(w_graph,v =8,mode="out")-2610])
diag(NS) <- NA
mean(NS,na.rm=T)
####
mst = minimum.spanning.tree(w_graph,algorithm = "prim")
###clustering based on similarity matrix
for(yr in years){ for(j in 1:5 ) getResemblance(yr, yearly_graphs,j)}
findResemblanceClusters <- function(y_name,n,clustNum){
exact_res = read.csv(paste0(y_name,"Res_",n,".csv"))
sim_sell_clusters = kmeans(exact_res,)
#View(data.frame(name=names(exact_res),
 #               cluster=sim_sell_clusters$cluster)[(which(sim_sell_clusters$cluster>1)),])
#yearly_tech[,res_clust:=sim_sell_clusters$cluster[which(names(exact_res)==seller)],by=seller]

}
