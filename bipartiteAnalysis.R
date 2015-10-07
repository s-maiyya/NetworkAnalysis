library(igraph)
#library(statnet)
#library(sna)
library(lubridate)
# CSIdata = read.csv("/Users/sindhumaiyya/Documents/Work/Graph_based/WorkingDataSet_2-18-2015.csv",header = T)
# CSIdata = data.table(CSIdata)
# CSIdata$First.Confirmed.Reservation.Date = ymd(CSIdata$First.Confirmed.Reservation.Date,tz ="America/Tijuana" )
# CSIdata$First.New.Reservation.Request.Date = ymd(CSIdata$ymd(CSIdata$First.Confirmed.Reservation.Date,tz ="America/Tijuana" ),tz ="America/Tijuana" )
# CSIdata$year = year(CSIdata$First.Confirmed.Reservation.Date)
# CSIdata$tech_mix = paste(CSIdata$PV.Module.1.Manufacturer,CSIdata$Inverter.1.Manufacturer,sep="_")
# CSIdata = CSIdata[CSIdata$tech_mix!="_",]
# CSIdata[is.na(year)&!is.na(First.New.Reservation.Request.Date),year:=year(First.New.Reservation.Request.Date)]
# CSIdata = CSIdata[-grep("TBD|Self|Obsolete",Seller.Company.Name)]
CSIdata = CSIdata[-grep("Cance|Withd|Site|Reserved|Removed",Current.Incentive.Application.Status)]
CSIdata$program = "small"
CSIdata[grep("Large",Program),program:="large"]
####yearly  division
load("CSIdata.Rdata")
CSIdata$pv_mod = CSIdata$PV.Module.1.Manufacturer
CSIdata$inv = CSIdata$Inverter.1.Manufacturer

###Yearly bipartite graphs####

####Graph analysis

layer2 = paste(as.character(CSIdata$PV.Module.1.Manufacturer),as.character(CSIdata$Inverter.1.Manufacturer),
      sep="_")
layer1 = CSIdata$Seller.Company.Name
edgelist <- data.frame(layer1,layer2,layer3 = CSIdata$Host.Customer.Physical.Zip.Code)
edgelist <- edgelist[edgelist$layer1!="",]
for(i in 1:3){
  for(j in c(2,3,1)[i]){
graph <- graph.data.frame(edgelist[,c(i,j)],directed = T)
graph = delete.vertices(graph,which(is.na(V(graph)$name)))
graph = delete.vertices(graph,which(is.na(V(graph)$name)))
graph = delete.vertices(graph,which(V(graph)$name==""||V(graph)$name=="_"))
V(graph)$type <- V(graph)$name %in% edgelist[,i]
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
Ma = get.adjacency(w_graph,type="both",attr = "weight")
M = Ma[which(rowSums(as.matrix(Ma))!=0),]
M = M[,which(colSums(as.matrix(M))!=0)]
write.table(data.frame(verices=c(rownames(M),"XXXX",colnames(M))),file=paste0("vertices_",i,j,".txt"),row.names=F,sep = "\t")
}
}
V(graph)$type <- V(graph)$name %in% edgelist[,1]

bipartite.projection(graph)
inlayout.bipartite(graph)


E(graph)$weight
graph.strength(graph)
plot(degree(graph,grep("_",V(graph)$name)),ylab="degree",xlab="tech_mix")
plot(degree(graph,c(1:length(V(graph)))[-grep("_",V(graph)$name)]),ylab="degree",xlab="installer")
plot(degree.distribution(graph,cumulative = T)[1:150])
length(unique(neighbors(graph,V(graph)[2619])))
length(unique(neighbors(graph,8)))
######
graph06 = graph.data.frame(yearly_tech[year==2006,c(2:3),with=F],directed = T)
graph06 = delete.vertices(graph06,which(is.na(V(graph06)$name)))
graph07 = graph.data.frame(yearly_tech[year==2007,c(2:3),with=F],directed = T)
graph07 = delete.vertices(graph07,which(is.na(V(graph07)$name)))
graph08 = graph.data.frame(yearly_tech[year==2008,c(2:3),with=F],directed = T)
graph08 = delete.vertices(graph08,which(is.na(V(graph08)$name)))
graph09 = graph.data.frame(yearly_tech[year==2009,c(2:3),with=F],directed = T)
graph09 = delete.vertices(graph09,which(is.na(V(graph09)$name)))
graph10 = graph.data.frame(yearly_tech[year==2010,c(2:3),with=F],directed = T)
graph10 = delete.vertices(graph10,which(is.na(V(graph10)$name)))
graph11 = graph.data.frame(yearly_tech[year==2011,c(2:3),with=F],directed = T)
graph11 = delete.vertices(graph11,which(is.na(V(graph11)$name)))
graph12 = graph.data.frame(yearly_tech[year==2012,c(2:3),with=F],directed = T)
graph12 = delete.vertices(graph12,which(is.na(V(graph12)$name)))
graph13 = graph.data.frame(yearly_tech[year==2013,c(2:3),with=F],directed = T)
graph13 = delete.vertices(graph13,which(is.na(V(graph13)$name)))
graph14 = graph.data.frame(yearly_tech[year==2014,c(2:3),with=F],directed = T)
graph14 = delete.vertices(graph14,which(is.na(V(graph14)$name)))

#yearly_graph_uniq = list(graph06,graph07,graph08,graph09,graph10,graph11,graph12,graph13,graph14)
#names(yearly_graph_uniq) = c("2006","2007","2008","2009","2010","2011","2012","2013","2014")
max_degrees = lapply(yearly_graph, function(x){
  c(which.max(degree(x,mode="out")),max(degree(x,mode="out")))
})

yearly_graph = list(graph06,graph07,graph08,graph09,graph10,graph11,graph12,graph13,graph14)
names(yearly_graph) = c("2006","2007","2008","2009","2010","2011","2012","2013","2014")
max_degrees = lapply(yearly_graph, function(x){
  c(which.max(degree(x,mode="out")),max(degree(x,mode="out")))
})


yearly_proj = lapply(yearly_graph,function(x){V(x)$type <- V(x)$name%in%edgelist[,1];
                     bipartite.projection(x)})


plot(yearly_proj[[2]]$proj2,edge.width=E(yearly_proj[[2]]$proj2)$weight^0.5,
     vertex.label=V(yearly_proj[[2]]$proj2)$name)
adj_matrix = get.adjacency(yearly_proj[[2]]$proj2,sparse=FALSE,attr="weight")
##plot degree distributions for each year
lapply(1:length(yearly_graph), function(i) {png(paste0("unique_deg_distribution_",i,".png") ,width = 1500, height = 800);
  plot(graph.strength.distribution(yearly_graph[[i]],cumulative = F),
                                      typ="l",ylim=c(0,0.1),ylab="probability",xlab="degree");
  dev.off()})
plot(graph.strength.distribution(yearly_proj[[1]],cumulative = F),
     typ="l",ylim=c(0,0.1),ylab="probability",xlab="degree")
lapply(1:length(yearly_graph), function(i) {
V(yearly_graph[[i]])$label[grep("_",V(yearly_graph[[i]])$name)] <- NA;
V(yearly_graph[[i]])$color[c(1:length(V(yearly_graph[[i]])$name))
                           [-grep("_",V(yearly_graph[[i]])$name)]] <-  rgb(1,0,0,.1);
V(yearly_graph[[i]])$size[grep("_",V(yearly_graph[[i]])$name)] <- 0.5;
E(yearly_graph[[i]])$width <- .3;
E(yearly_graph[[i]])$color <- rgb(.5,.5,0,.1);

pdf(paste0("bipartite_graph_",i,".pdf"));
plot(yearly_graph[[i]], layout=layout.fruchterman.reingold.grid);
dev.off();
})
##get yearly degrees of different installers
yearly_tech[,length(unique(seller)),by=year]
above5 = lapply(yearly_graph,function(x) which(degree(x,mode = "out")>10))
above5_deg = lapply(yearly_graph,function(x) degree(x,mode = "out")[which(degree(x,mode = "out")>10)])
above5cos = lapply(above5,function(x) names(x))
yearly_tech_above5 = yearly_tech[seller%in%unique(unlist(above5cos))&!is.na(year),]
yearly_tech_above5[,seller,by=list(seller,year)]
yrly_stats = yearly_tech_above5[,data.table(num_manufacturer =length(unique(tech_mix)),num_install = length(tech_mix))
                               ,by=list(seller,year)]
seller_stats = yrly_stats[,data.frame(year,num_manufacturer,num_install),by=seller]

seller_stats[,ggplot(data = data.frame(year,num_manufacturer,num_install))+
             geom_line(aes(x=year,y=num_install))+
             geom_line(aes(x=year,y=num_manufacturer),col="blue"),by=seller]

plots = lapply(unique(seller_stats$seller),function(a) ggplot(data = seller_stats[seller==a,])+
         geom_line(aes(x=as.integer(year),y=num_install))+
         geom_line(aes(x=as.integer(year),y=num_manufacturer),col="blue")+ggtitle(as.character(a))
)
for(i in seq(from=1,to=length(plots),by=20)){
png(paste0("deg_plot_",i,".png") ,width = 1500, height = 800)
Multiplot(plotlist = plots[i+0:19],cols=5)
dev.off()
}