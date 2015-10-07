###Load CSI plus and process it###
library(stringr)
options(stringsAsFactors = F)
ex_data = read.csv("/Users/sindhumaiyya/Documents/Work/CSIplus/CSIplus.csv")
ex_data = data.table(ex_data)

yearly_tech = ex_data[col_customersegment=="RES"&col_groundmount!=1&col_appraised==F,
                      data.table(tech_mix1=col_manuf1_clean,tech_mix2=col_inv_man_clean,
                                  seller = col_installer_clean, selfinstall = col_selfinstall,
                                  year=col_year,size=col_sys_size, 
                                  price= col_install_price_real_w, p.cost = col_mod_cost_real,
                                  i.cost = col_inv_cost_real, costpW = col_cost_per_W_normalized,
                                  customer_segment = col_customersegment,
                                  tpo = col_3rdparty, mod_typ = col_tech_1,
                                  microinv =col_microinv, china_mod = col_china,
                                  ylong = col_ylong, xlat = col_xlat,
                                  county = col_county,
                                  state = col_state)]
yearly_tech = yearly_tech[seller!="NULL"&tech_mix1!="NULL"&tech_mix2!="NULL"&
                            tech_mix1!="-9999"&tech_mix2!="-9999"&seller!="null"]
yearly_tech = yearly_tech[seller!=-9999&size>0&size<=15]

yearly_tech$tech_mix2 = str_replace_all(yearly_tech$tech_mix2,ignore.case("sunpower"),"SunPower")
yearly_tech$tech_mix2[grep("sunpower.*fronius",yearly_tech$tech_mix2,ignore.case = T)] =
  "SunPower(Fronius)"
yearly_tech$tech_mix2[grep("sunpower.*xantrex",yearly_tech$tech_mix2,ignore.case = T)] =
  "SunPower(Xantrex)"
yearly_tech$tech_mix2[grep("sunpower.*pv powered",yearly_tech$tech_mix2,ignore.case = T)] =
  "SunPower(PV Powered)"
yearly_tech$tech_mix2[grep("sunpower.*SMA",yearly_tech$tech_mix2,ignore.case = T)] =
  "SunPower(SMA)"
yearly_tech$tech_mix2[grep("sunpower.*corp",yearly_tech$tech_mix2,ignore.case = T)]=
  "SunPower"
yearly_tech$tech_mix2[grep("sunpower |^sun.*power$",yearly_tech$tech_mix2,ignore.case = T)]=
  "SunPower"
yearly_tech$tech_mix2[grep("enphase",yearly_tech$tech_mix2,ignore.case = T)]=
  "Enphase"
yearly_tech$tech_mix2[grep("Fronius",yearly_tech$tech_mix2,ignore.case = T)]=
  "Fronius"
yearly_tech$tech_mix2[grep("sma.*america|^sma$|^sma $|sunnyboy|sunny.*boy",
                           yearly_tech$tech_mix2,ignore.case = T)]=
  "SMA America"
yearly_tech$tech_mix2[grep("power.*one",yearly_tech$tech_mix2,ignore.case = T)]=
  "Power-One"
yearly_tech$tech_mix2[grep("pv.*powered",yearly_tech$tech_mix2,ignore.case = T)]=
  "PV Powered"
yearly_tech$tech_mix2[grep("Xantrex|xantrax",yearly_tech$tech_mix2,ignore.case = T)]=
  "Xantrex"
yearly_tech$tech_mix2[grep("^kaco",yearly_tech$tech_mix2,ignore.case = T)]=
  "Kaco"
yearly_tech$tech_mix2[grep("schuco",yearly_tech$tech_mix2,ignore.case = T)]=
  "Schuco"
yearly_tech$tech_mix2[grep("sharp",yearly_tech$tech_mix2,ignore.case = T)]=
  "Sharp"
yearly_tech$tech_mix2[grep("magnatek|magnetek",yearly_tech$tech_mix2,ignore.case = T)]=
  "Magnetek"
yearly_tech$tech_mix2[grep("solectria",yearly_tech$tech_mix2,ignore.case = T)]=
  "Solectria"
yearly_tech$tech_mix2[grep("outback",yearly_tech$tech_mix2,ignore.case = T)]=
  "Outback"
yearly_tech$tech_mix2[grep("solaredge|solar.*edge",yearly_tech$tech_mix2,ignore.case = T)]=
  "SolarEdge"
yearly_tech$tech_mix2[grep("^advanced|advanced.*energy",yearly_tech$tech_mix2,ignore.case = T)]=
  "Advanced Energy"
yearly_tech = yearly_tech[-grep("unknown|conversion",yearly_tech$tech_mix2,ignore.case=T),]
#yearly_tech = yearly_tech[-grep("unknown",yearly_tech$tech_mix1,ignore.case=T),]
###cleaning up panel names##
# yearly_tech$tech_mix1[grep("kyocera",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Kyocera"
# yearly_tech$tech_mix1[grep("yingli",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Yingli"
# yearly_tech$tech_mix1[grep("^rec.*solar",yearly_tech$tech_mix1,ignore.case = T)]=
#   "REC Solar"
# yearly_tech$tech_mix1[grep("REC$|REC $",yearly_tech$tech_mix1,ignore.case = T)]=
#   "REC Solar"
# yearly_tech$tech_mix1[grep("rec.*scan",yearly_tech$tech_mix1,ignore.case = T)]=
#   "REC Scanmodule"
# yearly_tech$tech_mix1[grep("^bp",yearly_tech$tech_mix1,ignore.case = T)]=
#   "BP Solar"
# yearly_tech$tech_mix1[grep("^sharp",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Sharp"
# yearly_tech$tech_mix1[grep("^sanyo",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Sanyo"
# yearly_tech$tech_mix1[grep("sunpower",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Sunpower"
# yearly_tech$tech_mix1[grep("^sun.*power",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Sunpower"
# yearly_tech$tech_mix1[grep("^solar.*world",yearly_tech$tech_mix1,ignore.case = T)]=
#   "SolarWorld"
# yearly_tech$tech_mix1[grep("^suntech.*power",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Suntech Power"
# yearly_tech$tech_mix1[grep("^suntech$",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Suntech Power"
# yearly_tech$tech_mix1[grep("^schott|^rwe schott",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Schott"
# yearly_tech$tech_mix1[grep("^trina.*solar|^trina$",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Trina Solar"
# yearly_tech$tech_mix1[grep("^canadian|^canadien",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Canadian Solar"
# yearly_tech$tech_mix1[grep("^phono",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Phono Solar"
# yearly_tech$tech_mix1[grep("^centro",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Centro Solar"
# yearly_tech$tech_mix1[grep("^shell",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Shell Solar"
# yearly_tech$tech_mix1[grep("^LG",yearly_tech$tech_mix1,ignore.case = F)]=
#   "LG Electronics"
# yearly_tech$tech_mix1[grep("^schuco",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Schuco Solar"
# yearly_tech$tech_mix1[grep("^siliken",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Siliken"
# yearly_tech$tech_mix1[grep("^first.*solar",yearly_tech$tech_mix1,ignore.case = T)]=
#   "First Solar"
# yearly_tech$tech_mix1[grep("^GE",yearly_tech$tech_mix1,ignore.case = F)]=
#   "GE Energy"
# yearly_tech$tech_mix1[grep("^mage",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Mage Solar"
# yearly_tech$tech_mix1[grep("^ET.*solar",yearly_tech$tech_mix1,ignore.case = T)]=
#   "ET Solar"
# yearly_tech$tech_mix1[grep("^changzhou.*trina",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Trina Solar"
# yearly_tech$tech_mix1[grep("^ningbo",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Ningbo Solar"
# yearly_tech$tech_mix1[grep("^astropower",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Astropower"
# yearly_tech$tech_mix1[grep("^evergreen",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Evergreen Solar"
# yearly_tech$tech_mix1[grep("^suniva",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Suniva"
# yearly_tech$tech_mix1[grep("^hanwha",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Hanwha Solar"
# yearly_tech$tech_mix1[grep("^solar.*power",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Solar Power Industries"
# yearly_tech$tech_mix1[grep("conergy",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Conergy"
# yearly_tech$tech_mix1[grep("isofoton",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Isofoton"
# yearly_tech$tech_mix1[grep("hyundai",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Hyundai"
# yearly_tech$tech_mix1[grep("^mitsubishi",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Mitsubishi"
# yearly_tech$tech_mix1[grep("photowatt",yearly_tech$tech_mix1,ignore.case = T)]=
#   "Photowatt"
# yearly_tech$tech_mix1[grep("^CEEG",yearly_tech$tech_mix1,ignore.case = T)]=
#   "CEEG (Shanghai)"

yearly_tech$tech_mix = paste(yearly_tech$tech_mix1,yearly_tech$tech_mix2,sep="_")
yearly_tech$tech_mix1 = NULL;yearly_tech$tech_mix2 = NULL;
# #yearly_tech = yearly_tech[yearly_tech$tech_mix%in%names(which(cumsum(sort(table(yearly_tech$tech_mix),decreasing=T))< 
#  #       .9*sum(table(yearly_tech$tech_mix)))),]
# yearly_tech$seller[grep("^verengo",yearly_tech$seller,ignore.case = T)]=
#   "Verengo Solar"
# yearly_tech$seller[grep("akeena.*solar",yearly_tech$seller,ignore.case = T)]=
#   "Akeena Solar"
# yearly_tech$seller[grep("sungevity",yearly_tech$seller,ignore.case = T)]=
#   "Sungevity"
# yearly_tech$seller[grep("rec.*solar",yearly_tech$seller,ignore.case = T)]=
#   "REC Solar"
# yearly_tech$seller[grep("mohr",yearly_tech$seller,ignore.case = T)]=
#   "Mohr Power Solar"
# yearly_tech$seller[grep("helio.*power",yearly_tech$seller,ignore.case = T)]=
#   "Helio Power"
# yearly_tech$seller[grep("borrego.*solar",yearly_tech$seller,ignore.case = T)]=
#   "Borrego Solar Systems"
# yearly_tech$seller[grep("herca.*solar",yearly_tech$seller,ignore.case = T)]=
#   "Herca Solar"
# yearly_tech$seller[grep("sun.*light.*power",yearly_tech$seller,ignore.case = T)]=
#   "Sun Light & Power"
# yearly_tech$seller[grep("premier.*power",yearly_tech$seller,ignore.case = T)]=
#   "Premier Power Renewable Energy"
# yearly_tech$seller[grep("souther.*california.*solar",yearly_tech$seller,ignore.case = T)]=
#   "Southern California Solar"
# yearly_tech$seller[grep("petersen.*dean",yearly_tech$seller,ignore.case = T)]=
#   "Petersen-Dean Roofing and Solar"
# yearly_tech$seller[grep("real.*goods",yearly_tech$seller,ignore.case = T)]=
#   "Real Goods Solar"
# yearly_tech$seller[grep("^renewable.*energy",yearly_tech$seller,ignore.case = T)]=
#   "Renewable Energy Concepts"
# yearly_tech$seller[grep("energy.*efficiency.*solar",yearly_tech$seller,ignore.case = T)]=
#   "Energy Efficiency Solar"
# yearly_tech$seller[grep("^revco.*solar",yearly_tech$seller,ignore.case = T)]=
#   "Revco Solar Engineering"
# yearly_tech$seller[grep("america.*solar.*direct",yearly_tech$seller,ignore.case = T)]=
#   "American Solar Direct"
# yearly_tech$seller[grep("^advanced.*solar",yearly_tech$seller,ignore.case = T)]=
#   "Advanced Solar Electric"
# yearly_tech$seller[grep("jp.*sun",yearly_tech$seller,ignore.case = T)]=
#   "JP & Sun dba Solar Works"
# yearly_tech$seller[grep("^sharp.*elec",yearly_tech$seller,ignore.case = T)]=
#   "Sharp Electronics"
# yearly_tech$seller[grep("^sunpower",yearly_tech$seller,ignore.case = T)]=
#   "SunPower Corp."
yearly_tech = yearly_tech[price<=15&price>=1.5]
trim_tech = names(which(cumsum(sort(table(yearly_tech[,tech_mix]),decreasing=T))<.9*nrow(yearly_tech)))   
trim_tech = unique(c(trim_tech,yearly_tech[year%in%c(2003:2006),tech_mix]))
trim_data = yearly_tech[tech_mix%in%trim_tech]
yearly_tech = trim_data
yearly_tech = data.frame(yearly_tech)
yearly_tech[yearly_tech==-9999] = NA
yearly_tech = data.table(yearly_tech)
product_cost_w = yearly_tech[,data.frame(freq = length(size),mod_cost=median(p.cost/size,na.rm=T)/1e3,
                                         inv_cost=median(i.cost/size,na.rm=T)/1e3,
                                         mod_sd=sd(p.cost*1e-3/size,na.rm=T),inv_sd=sd(i.cost*1e-3/size,na.rm=T)),
                             by=tech_mix]
product_cost_w[,total_cost:= mod_cost+ inv_cost]
product_cost_w[,mod_sd:=mod_sd/mod_cost]; product_cost_w[,inv_sd:=inv_sd/inv_cost];
##should find a better way to this one line , explore dplyr##
product_cost_w[abs(total_cost-2)<=1,cost_grp:=1]
product_cost_w[abs(total_cost-4)<=1,cost_grp:=2]
product_cost_w[abs(total_cost-6)<=1,cost_grp:=3]
product_cost_w[abs(total_cost-8)<=1,cost_grp:=4]

yearly_tech = merge(yearly_tech,product_cost_w[,data.frame(tech_mix,cost_grp)],by="tech_mix")
yearly_tech[abs(size-2)<=2,size_grp:=1]
yearly_tech[abs(size-6)<=2,size_grp:=2]
yearly_tech[abs(size-8)<=2,size_grp:=3]
yearly_tech[abs(size-12)<=2,size_grp:=4]
####


save(yearly_tech,file="CSIplus_yearly_tech.Rdata")


# 
# pdf("Techmixcumulative_dist.pdf");
# plot(cumsum(sort(table(yearly_tech$tech_mix),decreasing=T)),typ="l",ylab="# installations",xlab="#tech mix")
# abline(h= .9*nrow(yearly_tech))
# abline(h= .8*nrow(yearly_tech))
# dev.off();
# pdf("Trimmed_echmix_cumulative_dist.pdf");
# plot(cumsum(sort(table(yearly_tech[year==2012,tech_mix]),decreasing=T)),typ="l",ylab="# installations",xlab="#tech mix",ylim=c(0,5e4))
# points(cumsum(sort(table(yearly_tech[year==2011,tech_mix]),decreasing=T)),typ="l",col="red")
# points(cumsum(sort(table(yearly_tech[year==2010,tech_mix]),decreasing=T)),typ="l",col="blue")
# points(cumsum(sort(table(yearly_tech[year==2009,tech_mix]),decreasing=T)),typ="l",col="green4")
# points(cumsum(sort(table(yearly_tech[year==2008,tech_mix]),decreasing=T)),typ="l",col="red")
# points(cumsum(sort(table(yearly_tech[year==2007,tech_mix]),decreasing=T)),typ="l",
#  ylab="# installations",xlab="#tech mix",ylim=c(0,5e3))
# points(cumsum(sort(table(trim_data[year==2012,tech_mix]),decreasing=T)),typ="l")
# points(cumsum(sort(table(trim_data[year==2011,tech_mix]),decreasing=T)),typ="l",col="red")
# points(cumsum(sort(table(trim_data[year==2010,tech_mix]),decreasing=T)),typ="l",col="blue")
# points(cumsum(sort(table(trim_data[year==2009,tech_mix]),decreasing=T)),typ="l",col="green4")
# points(cumsum(sort(table(trim_data[year==2008,tech_mix]),decreasing=T)),typ="l",col="red")
# points(cumsum(sort(table(trim_data[year==2007,tech_mix]),decreasing=T)),typ="l",
#        ylab="# installations",xlab="#tech mix",ylim=c(0,5e3))
# dev.off();
# 
# plot(cumsum(sort(table(yearly_tech[year==2006,tech_mix]),decreasing=T)),typ="l",
#      ylab="# installations",xlab="#tech mix",ylim=c(0,1.2e3))
# points(cumsum(sort(table(yearly_tech[year==2005,tech_mix]),decreasing=T)),typ="l",col="green4")
# points(cumsum(sort(table(yearly_tech[year==2004,tech_mix]),decreasing=T)),typ="l",col="red")
# points(cumsum(sort(table(yearly_tech[year==2003,tech_mix]),decreasing=T)),typ="l",col="blue")
# ##
# points(cumsum(sort(table(trim_data[year==2006,tech_mix]),decreasing=T)),typ="l",
#      ylab="# installations",xlab="#tech mix",ylim=c(0,5e3))
# points(cumsum(sort(table(trim_data[year==2005,tech_mix]),decreasing=T)),typ="l",col="green4")
# points(cumsum(sort(table(trim_data[year==2004,tech_mix]),decreasing=T)),typ="l",col="red")
# points(cumsum(sort(table(trim_data[year==2003,tech_mix]),decreasing=T)),typ="l",col="blue")
