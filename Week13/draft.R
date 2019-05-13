setwd("D:/GitHub/107-Spatial-Analysis/Week13")
library(GISTools);library(rgdal);library(sp);library(aspace);library(spatstat);library(raster)

#2
QA <- c(2, 1, 5, 3, 4, 4, 5, 6, 1, 3, 4, 3, 2, 2, 2, 1)
QA.mean <- mean(QA); QA.mean
QA.var <- var(QA); QA.var
QA.VMR <- QA.var/QA.mean;QA.VMR

s.e. <- sqrt(2/15)
t_value = (QA.VMR - 1)/s.e.;t_value
p_value = pt(t_value, df = 15)*2
p_value

#3
A_1 <- c(3,4,4,3,5,1,3,3,3,3)
A_2 <- c(3,4,4,3,5,1,6,6,6,6)

K_1 <- 3.2 / 10; K_1
L_1 <- sqrt(K_1/pi) - 0.3; L_1
K_2 <- 4.4 / 10; K_2
L_2 <- sqrt(K_2/pi) - 0.3; L_2

#A
##Read In Data
JR = readOGR(dsn = "./data", layer = "JR", encoding="utf8") #UTM54N

toshin = readOGR(dsn = "./data", layer = "toshin", encoding="utf8") #WGS84
toshin = spTransform(toshin, JR@proj4string) #轉成UTM54N

metro.data = read.csv("./data/metro.csv")
#讀取到的是經緯度，先套用WGS84
metro = SpatialPointsDataFrame(cbind(metro.data$LON, metro.data$LAT), metro.data,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"))
metro = spTransform(metro, JR@proj4string) #轉成UTM54N
metro.toshin = gIntersection(toshin, metro) #抓出都心的地下鐵

# 標準橢圓
plot(toshin)
K.sde <- calc_sde(points = metro.toshin@coords[,1:2])
plot_sde(plotnew = FALSE, plotcentre = T, centre.col = "#483D8B", 
         sde.col = "#483D8B", sde.lwd = 2, titletxt = "", plotpoints = T)

#B
toshin.union = gUnaryUnion(toshin) #melt
metro.ppp <- ppp(metro.toshin@coords[,1], metro.toshin@coords[,2], as.owin(toshin.union))
plot(metro.ppp)

nnd <- nndist(metro.ppp, k=1)
d1 <- mean(nnd) # mean of the nearest

# Random point 
random <- vector()
for (i in 1:99) {
  nn1 <- rpoint(length(metro.toshin), win = as.owin(toshin.union))
  nnd1 <- nndist(nn1, k=1)
  random[i] <- mean(nnd1)
}

# Frequency plot
hist(random, main = "Monte Carlo Significance Test", xlim = c(350, 650))
abline(v = d1, col = "red")

#C
color <- c("red", "orange", "yellow", "green")
plot(c(0, 2000), c(0,1), type = "n", xaxs = "i", yaxs = "i", xlab = "距離", ylab = "G")
for(i in 1:4) {
  lines(ecdf(nndist(metro.ppp, k= i)), col = color[i], cex = 0, lwd = 3, verticals = T)
}

#D
# KDE Calculation
KDE1 = kde.points(JR, 2000, 100, toshin.union)
KDE2 = kde.points(metro.toshin, 2000, 100, toshin.union)

# Dual KDE Calculation
KDE1.R = raster(KDE1)
KDE2.R = raster(KDE2)
KDE.DIFF = KDE1.R - KDE2.R #raster可直接相減

# KDE map
plot(KDE.DIFF) #KDE圖
masker = poly.outer(KDE.DIFF, toshin.union) #建立遮罩
add.masking(masker, col="white") #覆蓋遮罩
plot(toshin.union, add=T) #加邊框

plot(toshin.union)
points(JR)

plot(toshin.union)
points(metro.toshin)