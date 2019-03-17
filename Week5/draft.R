library(GISTools)
library(rgdal)
library(sp)
library(dplyr)

flood50 <- readOGR(dsn = "./data", layer = "flood50", encoding="utf8")
flood50 <- spTransform(flood50, CRS("+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
Taipei_Vill <- readOGR(dsn = "./data", layer = "Taipei_Vill", encoding="utf8")
flood50.attr <- data.frame(flood50)
Taipei_Vill.attr <- data.frame(Taipei_Vill)

Overlay_Layer <- gIntersection(Taipei_Vill, flood50, byid = T)
plot(Overlay_Layer, lwd = 1)

name_list <- strsplit(names(Overlay_Layer), " ")
Vill.id <- as.numeric(unlist(lapply(name_list, function(x) x[1]))) #村里代號
flood.id <- as.numeric(unlist(lapply(name_list, function(x) x[2]))) #淹水代號

df <- data.frame(Vill.id, flood.id)

for (i in 1:length(df)){
  df$grid_code[i] <- flood50$grid_code[as.numeric(df$flood.id[i]) + 1]#df從1開始，flood50從0開始
}