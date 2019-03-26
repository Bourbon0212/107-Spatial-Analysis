library(GISTools)
library(rgdal)
library(dplyr)
library(sp)
library(ggplot2)

#1
MRT <- readOGR(dsn = "./data", layer = "MRT", encoding="utf8") #TWD97
TPE_LI <- readOGR(dsn = "./data", layer = "TPE_LI", encoding="utf8") #WGS84
TPE_LI <- spTransform(TPE_LI, CRS("+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs "))
TPE_LI$CENSUS <- as.numeric(as.character(TPE_LI$CENSUS))
MRT.attr <- MRT@data
TPE_LI.attr <- TPE_LI@data

TPE_LI$AREA <- poly.areas(TPE_LI)/1000000 #單位：平方公里
TPE_LI$DENSITY <- TPE_LI$CENSUS/1000/TPE_LI$AREA #單位：千人/平方公里

#ggplot fortify
TPE_LI.f <- fortify(TPE_LI, region = "V_ID")
TPE_LI.f <- merge(TPE_LI.f, TPE_LI@data, by.x = "id", by.y = "V_ID") #For SpatialPolygonDataframe
MRT.f <- data.frame(MRT) #For SpatiaPointDataframe
ggplot() +
  geom_polygon(data = TPE_LI.f, aes(x = long, y = lat, group = group, fill = DENSITY), color = 'white') +
  scale_fill_continuous("人口密度(千人/平方公里)") +
  geom_point(data = MRT.f, aes(x = coords.x1, y = coords.x2, col = LINE)) +
  scale_color_discrete("捷運路線", labels = c("藍線", "棕線", "綠線", "橘線", "紅線", "轉運站")) +
  ggtitle("台北人口密度與捷運站點分布") +
  coord_fixed(1.0) + theme_minimal()

#2-1
MRT.1 <- gBuffer(MRT, width = 500, byid = T) #buffer500
MRT.2 <- gBuffer(MRT, width = 500)
MRT.near <- gUnion(MRT.1, MRT.2)

TPE.MRT <- gIntersection(TPE_LI, MRT.near, byid = T)#做交集
TPE.MRT_name=strsplit(names(TPE.MRT), " ") 
TPE.MRT$TPE_id=unlist(lapply(TPE.MRT_name,function(x) x[1]))%>%as.numeric
TPE.MRT$MRT_id=unlist(lapply(TPE.MRT_name,function(x) x[2]))%>%as.numeric

TPE.MRT$area=as.vector(poly.areas(TPE.MRT))
TPE.MRT$TPE.area=poly.areas(TPE_LI)[TPE.MRT$TPE_id+1]
TPE.MRT$TPE.pop=TPE_LI$CENSUS[TPE.MRT$TPE_id+1]

TPE.MRT$pop=round(TPE.MRT$area/TPE.MRT$TPE.area*TPE.MRT$TPE.pop) #四捨五入
sum(TPE.MRT$pop)

#2-2
STN_POP <- function(id, dist) {
  buff <- gBuffer(MRT[MRT$MRT_ID==id,], width=dist, byid = T) #選出指定車站周邊特定距離
  inter <- gIntersection(TPE_LI, buff, byid = T)#做交集
  
  name <- strsplit(names(inter), " ")
  inter$TPE_id <- unlist(lapply(name,function(x) x[1]))%>%as.numeric
  inter$MRT_id <- unlist(lapply(name,function(x) x[2]))%>%as.numeric
  
  inter$area <- as.vector(poly.areas(inter))
  inter$TPE.area <- poly.areas(TPE_LI)[inter$TPE_id+1]
  inter$TPE.pop <- TPE_LI$CENSUS[inter$TPE_id+1]
  
  inter$pop <- round(inter$area/inter$TPE.area*inter$TPE.pop)
  return(sum(inter$pop))
}

#buff <- gBuffer(MRT[MRT$MRT_ID==38,], width=500, byid = T) #選出指定車站周邊特定距離
#inter <- gIntersection(TPE_LI, buff, byid = T)#做交集

#name <- strsplit(names(inter), " ")
#inter$TPE_id <- unlist(lapply(name,function(x) x[1]))%>%as.numeric
#inter$MRT_id <- unlist(lapply(name,function(x) x[2]))%>%as.numeric

#inter$area <- as.vector(poly.areas(inter))
#inter$TPE.area <- poly.areas(TPE_LI)[inter$TPE_id+1]
#inter$TPE.pop <- as.numeric(as.character(TPE_LI$CENSUS))[inter$TPE_id+1]

#inter$pop <- round(inter$area/inter$TPE.area*inter$TPE.pop)
#sum(inter$pop)

#3
SCHOOL <- readOGR(dsn = "./data", layer = "SCHOOL", encoding="utf8") #TWD97
SCHOOL.attr <- SCHOOL@data
JUNIOR <- SCHOOL[SCHOOL$TYPE=="國中",]

distance <- gDistance(MRT, JUNIOR, byid=T)
nearest <- apply(distance, 1, min) #欄位是MRT，列位是國中，找離每所國中捷運站的最近距離

Fn <- ecdf(nearest)
plot(Fn, main = "國中至最近捷運站距離的累積曲線", xlab = "距離(公尺)", ylab = "學校比例")
