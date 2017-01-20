df<-read.csv("./Data/NETN_Water_Data_RViz.csv")
units<-read.csv("./Data/tlu_Units.csv") ## table with units for lableing plots


ParkList<-unique(levels(df$ParkCode))

SiteList<-unique(levels(df$Description))

VarList<-unique(levels(df$Local.Characteristic.Name))

# ACAD_var<- unique(levels(df$Local.Characteristic.Name[df$ParkCode == "ACAD"]))
# MABI_var<- unique(levels(df$Local.Characteristic.Name[df$ParkCode == "MABI"]))
