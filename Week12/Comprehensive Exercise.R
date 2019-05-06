library(rgdal)
library(GISTools)
library(ggtern)
library(splancs)
Fastfood <- readOGR(dsn = "./data", layer = "Tpe_Fastfood", encoding="utf8")
Popn <- readOGR(dsn = "./data", layer = "Popn_TWN2", encoding="utf8")

#Filter Out TPE, MIC, KFC
TPE <- subset(Popn, COUNTY == "臺北市")
#MIC <- subset(Fastfood, STORE == "MIC")
#KFC <- subset(Fastfood, STORE == "KFC")
TPE_lim=c(TPE@bbox[1,], TPE@bbox[2,]) #Boundary

#Cencus Density Calculation
TPE$CENCUS <- TPE$A0A14_CNT + TPE$A15A64_CNT + TPE$A65UP_CNT #Census Total
TPE$AREA <- poly.areas(TPE) #District Area (person/m2)
TPE$DENSITY <- TPE$CENCUS / TPE$AREA

#Combine the Density with Fastfood Restaurant
Fastfood.attr <- Fastfood@data
Fastfood.attr$TYPE_99 <- as.numeric(as.character(Fastfood.attr$TYPE_99))
Fastfood.attr$DENSITY <- 0 #Assign value in advance
TPE.attr <- TPE@data

for (i in 1:nrow(Fastfood.attr)) {
  for (j in 1:nrow(TPE.attr)) {
    if (Fastfood.attr[i,]$TOWN == TPE.attr[j,]$TOWN) {
      Fastfood.attr[i,]$DENSITY = TPE.attr[j,]$DENSITY
    }
  }
}

MIC.attr <- subset(Fastfood.attr, STORE == "MIC")
KFC.attr <- subset(Fastfood.attr, STORE == "KFC")

#Weighted KDE Calculation
KDE.MIC.W <- kde2d.weighted(MIC.attr$X_COOR, MIC.attr$Y_COOR, 2000, 100, 
                            lims = TPE_lim, w = MIC.attr$TYPE_99 * MIC.attr$DENSITY)#x, y, search radius, resolution
KDE.KFC.W <- kde2d.weighted(KFC.attr$X_COOR, KFC.attr$Y_COOR, 2000, 100, 
                            lims = TPE_lim, w = KFC.attr$TYPE_99 * KFC.attr$DENSITY)

KDE.DIFF <- KDE.MIC.W
KDE.DIFF$z <- KDE.MIC.W$z - KDE.KFC.W$z

#Plot KDE Map
image(KDE.DIFF, asp = 1) #Remain x/y ratio
masker <- poly.outer(as.points(TPE@bbox[1,], TPE@bbox[2,]), TPE) 
add.masking(masker, col = "white")
plot(TPE, add = T)
