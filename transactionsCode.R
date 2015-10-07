##Tripartite graph
library(arules)
library(shapefiles)
require(rgdal)
library(RgoogleMaps)
library(ggmap)
CenterOfMap <- geocode("Modesto, California")
Calif <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat),zoom = 6, maptype = "terrain", source = "google")
zip_shape = readOGR(".","tl_2010_06_zcta510")
total_zip = unique(as.character(yearly_tech$zipcode))
subzip_shape = zip_shape[zip_shape$ZCTA5CE10%in%total_zip,]
subzip_shape = spTransform(subzip_shape, CRS("+proj=longlat +datum=WGS84"))
CaliMap <- ggmap(Calif) +geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.2,color='green', data=subzip_shape, alpha=0)
CaliMap

library(ggplot2)
zip_shape<- fortify(zip_shape)
yearly_tech[,tech_mix,by=zipcode]
length(unique(yearly_tech$tech_mix))
length(unique(yearly_tech$zipcode))
plot(sort(yearly_tech[,length(zipcode),by=tech_mix]$V1),ylim=c(0,0.5e3))
trans_list = (yearly_tech[,list(list(unique((tech_mix)))),by=(zipcode)])$V1
names(trans_list) = (yearly_tech[,list(list(as.factor(tech_mix))),by=(zipcode)])$zipcode
trans1 <- as((trans_list),"transactions")
image(trans1)
freq_tech <- apriori(trans1,parameter = list( supp=0.25,conf = 0.9,
                                             target = "frequent itemsets"))
total_zip = unique(yearly_tech[!year%in%c(2006,2015),zipcode])
total_tech = unique(yearly_tech[!year%in%c(2006,2015),tech_mix])

trans_list = lapply(2007:2014,function(yr){l=(yearly_tech[year==yr,list(list(unique((tech_mix)))),by=list(zipcode,year)])$V1;
                                           l = c(l,rep("_",length(total_zip)-length(l)),list(total_tech));
                                           names(l)= c((yearly_tech[year==yr,list(list(unique((tech_mix)))),by=list(zipcode,year)])$zipcode,
                                                  total_zip[!total_zip%in%(yearly_tech[year==yr,list(list(unique((tech_mix)))),by=list(zipcode,year)])$zipcode],
                                                  "wild_card");
                                           
                                           return(l)})

transes <- lapply(trans_list,function(tL) as(tL,"transactions"))
###another tack

yearly_tech$seller_install = NULL
yearly_tech$size = NULL
yearly_tech$year = as.factor(yearly_tech$year)
yearly_tech$tech_mix = as.factor(yearly_tech$tech_mix)
yearly_tech$program = as.factor(yearly_tech$program)
yearly_tech$zipcode = as.factor(yearly_tech$zipcode)
load("zipDB.Rdata")
region_county = list("Jefferson"= c("MODOC|LASSEN|PLUMAS|BUTTE|LAKE|GLENN|COLUSA|MENDOCINO|HUMBOLDT|TRINITY|TEHAMA|SHASTA|DEL NORTE|SISKIYOU"),
     "WestCa" = c( "LOS ANGELES|VENTURA|SANTA BARBARA|SAN LUIS OBISPO|KERN"),
     "SiliconVa" = c("CONTRA COSTA|ALAMEDA|SANTA CLARA|SAN BENITO|MONTEREY|SANTA CRUZ|SAN MATEO|SAN FRANCISCO"),
     "CentralCa" = c("INYO|MONO|ALPINE|CALAVERAS|TUOLUMNE|MARIPOSA|MADERA|FRESNO|KINGS|TULARE|MERCED|STANISLAUS|SAN JOAQUIN"),
     "NorthCa" = c("SIERRA|NEVADA|PLACER|EL DORADO|AMADOR|SACRAMENTO|SOLANO|MARIN|NAPA|SONOMA|YOLO|SUTTER|YUBA"),
     "SouthCa" = c("SAN BERNARDINO|RIVERSIDE|IMPERIAL|SAN DIEGO|ORANGE"))

region_zip = lapply(region_county,function(x) zipDB$zip[grep(x,zipDB$county,ignore.case = T)])
names(region_zip) = names(region_county)
for(i in 1:6)
 yearly_tech[zipcode%in%region_zip[[i]],zipcode:= names(region_zip)[i]]
yearly_tech$zipcode = as.factor(as.character(yearly_tech$zipcode))
yearly_tech$inv = as.factor(yearly_tech$inv)
yearly_tech$pv_mod = as.factor(yearly_tech$pv_mod)
CSI_trans =yearly_tech[,data.table(seller,tech_mix,pv_mod,
                   inv,zipcode)]
CSI_dcm <- as(CSI_trans[,data.table(seller,tech_mix,zipcode)],"transactions")
rules <- apriori(CSI_dcm,parameter = list(maxlen=300,confidence=0.05))
inspect(rules)
rulesZipcodes <- subset(rules, subset = rhs %in% "zipcode=WestCa" )
inspect(rulesZipcodes)
region_install = (yearly_tech[zipcode%in%names(region_county),
                              list(list(unique(zipcode))),by=(tech_mix)])$V1
names(region_install) = (yearly_tech[zipcode%in%names(region_county),
                                     list(list(unique((zipcode)))),by=(tech_mix)])$tech_mix
trans2 <- as((region_install),"transactions")
param2<-NBMinerParameters(CSI_dcm, pi=0.95, rules=TRUE,plot=T)
rules<-NBMiner(CSI_dcm, parameter=param2, control = list(verb=TRUE, debug=FALSE))
inspect(head(rules,30))
