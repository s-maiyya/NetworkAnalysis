#graph.strength.distribution <- function (graph, cumulative = FALSE, ...)

library(igraph)
getBipartiteMatrix <- function(graph){
  E(graph)$weight <- 1 
  w_graph = simplify(graph, edge.attr.comb=list(weight="sum"))
  w_graph = delete.vertices(w_graph,degree(w_graph)==0)
  Ma = get.adjacency(w_graph,type="both",attr = "weight")
  M = Ma[which(rowSums(as.matrix(Ma))!=0),]
  M = M[,which(colSums(as.matrix(M))!=0)]
  return(as.matrix(M))}

nameChop = function(variable, value){
  substr(value,1,19)
}
getYearlyTech <- function(CSIdata){
  CSIdata[grep("Small",Program),program:="small",]
  CSIdata[grep("Large",Program),program:="large",]
  CSIdata[grep("Multi",Program),program:="multi",]
  yearly_tech = CSIdata[,data.table(seller =Seller.Company.Name, tech_mix,size=Nameplate.Rating,program,
                                    pv_mod, inv, pvType, invType,
                                    zipcode=Host.Customer.Physical.Zip.Code),by=year]
  yearly_tech = yearly_tech[as.character(yearly_tech$seller)!="",]
  #yearly_tech$seller = as.character(yearly_tech$seller)
  yearly_tech = yearly_tech[!is.na(year)]
  yearly_tech[,seller_install:=length(tech_mix),by=seller]
  yearly_tech = yearly_tech[seller_install>35,]
  yearly_tech[floor(size)<16,bucket:="small",]
  yearly_tech[floor(size)>15,bucket:="large",]
  return(yearly_tech)
  
  
}