getRCAmatrix = function(yearly_tech,time_period){
  total_tech = unique(yearly_tech$tech_mix)
  total_seller = unique(as.character(yearly_tech$seller))
  data_set = yearly_tech[year%in%c(time_period,time_period+1)]
  products = unique(data_set$tech_mix)
  places = unique(data_set$seller)
  edge_list = data_set[,data.frame(seller,tech_mix)]
  st_graph = graph.empty() +vertices(total_seller) +vertices(total_tech)+
    edges(c(t(as.matrix(edge_list))))
  st_graph = delete.vertices(st_graph,which(is.na(V(st_graph)$name)||
                                              V(st_graph)$name==""||V(st_graph)$name=="_"))
  
  E(st_graph)$weight = 1
  
  V(st_graph)$type <- V(st_graph)$name %in% total_seller
  bi_mat = getBipartiteMatrix(st_graph)
  ##an installer is a "net importer" of a techmix if she installs at least 10% 
  #of that techmix among total tech mixes that time period ##
  bi_mat[bi_mat > 0] = 1 
  product_sim = sapply(1:ncol(bi_mat),function(i) colSums(bi_mat&bi_mat[,i])/sum(bi_mat[,i]))
  return(product_sim)
}
getTPgraph <- function(yearly_tech,time_period){
  total_tech = unique(yearly_tech$tech_mix)
  total_seller = unique(as.character(yearly_tech$seller))
  data_set = yearly_tech[year%in%c(time_period,time_period+1)]
  products = unique(data_set$tech_mix)
  places = unique(data_set$seller)
  edge_list = data_set[,data.frame(seller,tech_mix)]
  st_graph = graph.empty() +vertices(total_seller) +vertices(total_tech)+
    edges(c(t(as.matrix(edge_list))))
  st_graph = delete.vertices(st_graph,which(is.na(V(st_graph)$name)||
                                              V(st_graph)$name==""||V(st_graph)$name=="_"))
  
  E(st_graph)$weight = 1
  
  V(st_graph)$type <- V(st_graph)$name %in% total_seller
 
  return(st_graph)
}