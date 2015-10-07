##inverter and PV model details
library(xlsx)
panel_deets = read.csv("panel_CSI.csv")
names(panel_deets)[1:2] = c("Manufacturer","Model")
inverter_deets1 = read.csv("inverter_deets.csv")
inverter_deets2 = read.xlsx("solarhub_inverter_data.xlsx",sheetIndex = 2)
inverter_deets2$idx = paste(inverter_deets2$Manufacturer,
                            inverter_deets2$Model.Number.,sep = "_")
inverter_deets2$Manufacturer. = NULL; inverter_deets2$Model.Number.= NULL
inverter_deets1$idx = paste(inverter_deets1$Manufacturer.Name,
                            inverter_deets1$Inverter.Model.number,sep = "_")
inverter_deets1$Manufacturer.Name = NULL; inverter_deets1$Inverter.Model.number = NULL
inverter_deets = data.frame(idx= (inverter_deets2$idx),
                            Description = (inverter_deets2$Description.))
inverter_deets$idx = as.character(inverter_deets$idx) ; 
inverter_deets$Description = as.character(inverter_deets$Description)
inverter_deets$Description[which(inverter_deets2$Micro.Inverter.== "Yes")] = "micro"
inverter_deets$Description[which(!is.na(inverter_deets2[,13]))] = "MPPT"
inverter_deets = rbind(inverter_deets,data.frame(idx = inverter_deets1$idx[which(!inverter_deets1$idx%in%inverter_deets$idx)],Description=
  inverter_deets1$Description[which(!inverter_deets1$idx%in%inverter_deets$idx)]))

##data connection type; micro or not; string; mppt; transformer/less; optimizers; grid tie; 
##utility interactive; 

type = rep("Mono",nrow(panel_deets))
type[grep("Poly",panel_deets$Description)] = "Poly"
type[grep("ACPV",panel_deets$Description)] = "ACPV"
panel_deets$Description = type;rm(type)
type = rep("regular",nrow(inverter_deets))
for(i in 1:length(inv_types))
{
  type[grep(inv_types[i],inverter_deets$Description,ignore.case = T)] = i
}
type[!is.na(inv_names[as.numeric(type)])] = na.omit(inv_names[as.numeric(type)])
type[grep("Enphase",inverter_deets$idx)] = "micro"
inverter_deets$Type= type; rm(type)
rm(inverter_deets1,inverter_deets2)
inverter_deets = inverter_deets[!duplicated(inverter_deets$idx),]
panel_idx = paste(panel_deets$Manufacturer,panel_deets$Model,sep = "_")


ex_data[, invModel:= paste(Inverter.1.Manufacturer,Inverter.1.Model,sep="_")]
CSIdata[, pvModel:=paste(PV.Module.1.Manufacturer,PV.Module.1.Model,sep="_")]
CSIdata[,pvType:=panel_deets$Description[which(panel_idx==pvModel)],by=pvModel]
ex_data[,invType:=inverter_deets$Type[which(inverter_deets$idx==invModel)],by=invModel]
CSIdata[grep("Enphase|micro",invModel,ignore.case = T),invType:= "micro"]
ex_data[is.na(CSIdata$invType),invType:="regular"]
CSIdata[is.na(CSIdata$pvType),pvType:="Mono"]
####
inv_types = c("Utility Interactive Inverter|Utility-interactive|U-I",
  "Three Phase|3-phase",
  "Tigo Management Unit|optimize",
 "MPPT",
 "Grid Tie|Grid-Tied|Grid-tie|DC Disconnect",
 "arc detector",
 "Transformer-less|Transformerless",
 "Low DC Voltage",
 "Power Conversion System",
 "string",
 "bi-directional inverter",
 "battery backup",
 "mobile|sealed|vented",
 "multi-mode|multi.*mode",
 "ACPV",
 "micro|modular"
 )
inv_names = c("U-I","3-phase", "optimizer",
              "MPPT", "grid-tied",
              "arc-detector","transformerless",
              "low-DCV", "power-conversion", "string", 
              "bi-directional","battery", "mobile-sealed",
              "multi-mode","ACPV","micro")
