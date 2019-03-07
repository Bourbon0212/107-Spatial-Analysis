library(GISTools)
library(rgdal)
library(ggplot2)
library(sp)
library(gridExtra)
EPA.STN <- readOGR(dsn = "./data", layer = "EPA_STN1", encoding="utf8")
EPA.STN <- spTransform(EPA.STN, CRS("+proj=longlat +datum=WGS84 +no_defs "))#轉成WGS84，因為EPA欄位只有經緯度
Popn.TWN <- readOGR(dsn = "./data", layer = "Popn_TWN2", encoding="utf8")
Popn.TWN <- spTransform(Popn.TWN, CRS("+proj=longlat +datum=WGS84 +no_defs "))#轉成WFGS84
EPA.data <- EPA.STN@data
Popn.data <- Popn.TWN@data

#Q1
Pollution_map = function(arg1){
  #data preparation
  #statistics calculation
  PSI.c <- qnorm(arg1, mean(EPA.data$PSI), sd(EPA.data$PSI), lower.tail = F)
  print(PSI.c)
  red <- subset(EPA.data, EPA.data$PSI >= PSI.c)#severe pollution
  blue <- subset(EPA.data, EPA.data$PSI < PSI.c)
  #plot map
  #fortify the vertex
  Popn.f <- fortify(Popn.TWN, region="TOWN")
  # merge(x, y, by.x, by.y): merge two data frames
  Popn.f <- merge(Popn.f, Popn.data, by.x = "id", by.y = "TOWN")
  map1 <- ggplot() +
    geom_polygon(data = Popn.f, aes(x = long, y = lat, group = group), 
                 fill = '#D2B48C', color = 'white') +
    geom_point(data = red, aes(x = TWD97Lon, y = TWD97Lat),  color = 'red') +
    geom_point(data = blue, aes(x = TWD97Lon, y = TWD97Lat),  color = 'blue') +
    coord_fixed(1.0) + theme_minimal()
  #box plot
  EPA.data <- subset(EPA.data, PSI > PSI.c)
  EPA.data <- subset(EPA.data, SiteType %in% c('一般測站', '工業測站', '交通測站'))
  map2 <- ggplot() +
    geom_boxplot(data = EPA.data, aes(x = SiteType, y = PSI)) + theme_minimal()
  #result by gridExtra
  grid.arrange(map1, map2, ncol=2)
}
Pollution_map(0.3)
Pollution_map(0.5)

#Q2-1

Popn.TWN <- spTransform(Popn.TWN, CRS("+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "))#轉回TWD97
#calculate cencus total
Popn.TWN$CENCUS <- as.numeric(as.character(Popn.TWN$A0A14_CNT + Popn.TWN$A15A64_CNT + Popn.TWN$A65UP_CNT))
#calcuate town area
Popn.TWN$AREA <- poly.areas(Popn.TWN) / 10^6
Popn.TWN$DENSITY <- Popn.TWN$CENCUS / Popn.TWN$AREA
#plot map
vacant.shades = auto.shading(Popn.TWN$DENSITY, n = 6, cutter = quantileCuts)
choropleth(Popn.TWN, Popn.TWN$DENSITY, shading=vacant.shades)
choro.legend(-220000,2600000,vacant.shades)
map.scale(302000,2339000,300000, "300km",3,1) 
north.arrow(386000,2850000,10000,col= 'black') 
title('台灣人口密度地圖')

#Q2-2
### OpenStreepMap ###
library(OpenStreetMap)
library(dplyr)

#elder proportion
Popn.data <- Popn.TWN@data
Popn.data$E.Prop = Popn.data$A65UP_CNT / Popn.data$CENCUS
#mark high elder prop
Popn.TAIPEI <- subset(Popn.data, Popn.data$COUNTY %in% c('臺北市', '新北市', '桃園市', '基隆市', '宜蘭縣')) %>% #選出大台北
  arrange(desc(E.Prop))

old.TOWN <- Popn.TAIPEI[1:15,]$TOWN
old.TOWN <- subset(Popn.TWN, Popn.TWN$TOWN %in% old.TOWN & Popn.TWN$COUNTY %in% c('臺北市', '新北市', '桃園市', '基隆市', '宜蘭縣'))


#use locator to get the ul lr from Popn.TWN
ul <- c(25.324075, 120.935067)
lr <- c(24.236039, 122.035227)

# download the map tile
MyMap <- openmap(ul,lr,9, "esri-topo")
# now plot the layer and the backdrop
par(mar = c(0,0,0,0))
plot(MyMap, removeMargin=F)
plot(spTransform(Popn.TWN, osm()), add = T)#plot taiwan
plot(spTransform(old.TOWN, osm()), add = T,col = rgb(red = 1, green = 0, blue = 0, alpha = 0.6))#plot old.TOWN


#plot

#Q2-3

#density level
Popn.data <- Popn.data %>%
  mutate(D.STATUS = case_when(
    DENSITY > 10000 ~ '高度密集',
    DENSITY < 2000 ~ '低度密集'
  ))
#box plot
ggplot(data = subset(Popn.data, Popn.data$D.STATUS %in% c('高度密集', '低度密集'))) +
  geom_boxplot(aes(x = D.STATUS, y = E.Prop))