rm(list=ls())
library(GISTools);library(rgdal);library(sp);library(dplyr);library(magrittr)
KH=readOGR(dsn = ".", layer = "KH_vill", encoding="unicode",verbose = F)
RC.data=read.csv("RescueCorps.csv")
#1-1
#資料來源是經緯度，因此用經緯度座標來讀取，再轉換成TWD97TM2。
RC=SpatialPointsDataFrame(cbind(RC.data$LON,RC.data$LAT),RC.data,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"))
RC=spTransform(RC,KH@proj4string)
#1-2
KH.town=gUnaryUnion(KH,KH$TOWN)
#1-3
pop=xtabs(KH$A65UP_CNT~KH$TOWN)
pop=data.frame(pop=as.vector(pop), row.names =names(pop))
KH.town=SpatialPolygonsDataFrame(KH.town,pop)
#1-4
KH.town$counts=poly.counts(RC,KH.town)
KH.town$score=KH.town$counts/KH.town$pop*10000
#1-5, 1-6
par(mar=c(0,0,1,0))
quant5=auto.shading(KH.town$score,cutter=quantileCuts,n=5,col=brewer.pal(5,"GnBu"))
choropleth(KH.town,KH.town$score,quant5)
title("高雄原市區與鳳山區人均救護資源量")
choro.legend(187000,2515322,quant5,between = "~",fmt="%.2f",cex=0.8,title='人均救護資源分數')
map.scale(170000,2495983,8000, "公里",4,2) 
north.arrow(168508,2513620,800,col= '#AAAAAA') 

#2-1
RC2.5=gBuffer(RC,width = 2500,byid = T)
KH.RC=gIntersection(KH,RC2.5,byid=T)
KH.RC_name=strsplit(names(KH.RC), " ") 
KH.RC$KH_id=unlist(lapply(KH.RC_name,function(x) x[1]))%>%as.numeric
KH.RC$RC_id=unlist(lapply(KH.RC_name,function(x) x[2]))%>%as.numeric

KH.RC$area=as.vector(poly.areas(KH.RC))
KH.RC$KH.area=poly.areas(KH)[KH.RC$KH_id+1]
KH.RC$KH.pop=KH$A65UP_CNT[KH.RC$KH_id+1]

KH.RC$pop=round(KH.RC$area/KH.RC$KH.area*KH.RC$KH.pop)
RC$pop=xtabs(KH.RC$pop~KH.RC$RC_id)
#2-2
RC$supply[substring(RC$NAME,3,3)=="大"]=10
RC$supply[substring(RC$NAME,3,3)=="中"]=7
RC$supply[substring(RC$NAME,3,3)=="分"]=5
#2-3
RC$Rj=RC$supply/RC$pop%>%as.numeric*10000
#2-4
data.frame(NAME=RC$NAME,ENG=RC$ENG_NAME,Rj=RC$Rj)

#3-1
KH.pt=gCentroid(KH,byid = T)
KH.RC_dist=gWithinDistance(RC,KH.pt,dist = 2500,byid = T)
sumRj=function(x) sum(RC$Rj[x])#只加True
KH$Ai=apply(KH.RC_dist,1,sumRj)