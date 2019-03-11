
#########################################################
#                                                       #
# Spatial Analysis (Geog 2017; NTU Geography)           #
#                                                       #
# Lecture 3: Using R as a GIS                           #
#                                                       #
# Instructor: Dr. Tzai-Hung Wen                         #
#                                                       #
# Source: Brunsdon and Comber(2015), Chapter 5          #
#                                                       #
# Date: 2019-03-11                                      #
#                                                       #
#########################################################


###########################
## 1. spatial intersection: 
##    Using gIntersection()
###########################

library(GISTools)
data(tornados)

# set plot parameters and initial plot for map extent
par(mar=c(0,0,0,0))
plot(us_states)
head(data.frame(us_states))

plot(torn, add = T, pch = 1, col = "#FB6A4A4C", cex = 0.4) 
plot(us_states, add = T)

head(data.frame(torn))
USstate_attr<- data.frame(us_states)

#AoI
index <- us_states$STATE_NAME == "Texas" | us_states$STATE_NAME == "New Mexico" | 
  us_states$STATE_NAME == "Oklahoma" | us_states$STATE_NAME == "Arkansas"

AoI <- us_states[index,]
head(data.frame(AoI))
plot(AoI)
plot(torn, add = T, pch = 1, col = "#FB6A4A4C")

# Intersection
?gIntersection()
AoI.torn <- gIntersection(AoI, torn, byid = TRUE) 
par(mar=c(0,0,0,0))
plot(AoI)
plot(AoI.torn, add = T, pch = 1, col = "#FB6A4A4C")

head(data.frame(AoI.torn))
head(rownames(data.frame(AoI.torn)))
tail(rownames(data.frame(AoI.torn)))

rownames(data.frame(us_states[index,]))
us_states$STATE_NAME[index]

#attach state names
tmp <- rownames(data.frame(AoI.torn))
n<-nrow(data.frame(AoI.torn))
new_stateid<-c(1:n); new_tornid<-c(1:n)
new_statename<-c(1:n); new_damage<-c(1:n)

for (i in 1:n) {
  new_stateid[i]<-substring(tmp[i], 1,2) 
  new_tornid[i]<-substring(tmp[i], 4,7) 
  new_statename[i]<-as.character(us_states$STATE_NAME[as.numeric(new_stateid[i])])
  new_damage[i]<-as.character(torn$DAMAGE[as.numeric(new_tornid[i])])
}

us_states$STATE_NAME[37]

torn$DAMAGE[1]

dfnew=cbind(new_tornid, new_stateid, new_statename,new_damage)
names(dfnew) <- c("torn_id","state_id","state_name", "torn_damage")
dfnew=data.frame(dfnew)

AoI.torn_new <- SpatialPointsDataFrame(AoI.torn, data = dfnew)
head(data.frame(AoI.torn_new))

attach(data.frame(AoI.torn_new))
count<-table(new_damage, new_statename)
count

detach(data.frame(AoI.torn_new))

#######################
## 2. Buffering: 
##    Using gBuffer()
#######################

# Example 2.1
# select an Area of Interest and apply a buffer
AoI <- us_states2[us_states2$STATE_NAME == "Texas",] 
AoI.buf <- gBuffer(AoI, width = 25000)
plot(AoI.buf)
plot(AoI, add = T, border = "blue")

# Example 2.2
data(georgia)
# apply a buffer to each object
buf.t <- gBuffer(georgia2, width = 5000, byid = T, id = georgia2$Name)

plot(buf.t)
plot(georgia2, add = T, border = "blue")

plot(buf.t[1,])
plot(georgia2[1,], add = T, col = "blue")

##################################
## 3. MERGING SPATIAL FEATURES: 
##    gUnaryUnion()
##################################

AoI.merge <- gUnaryUnion(us_states)
# now plot
par(mar=c(0,0,0,0))
plot(us_states, border = "darkgreen", lty = 3) 
plot(AoI.merge, add = T, lwd = 1.5)

########################################################
## 4. Point-in-Polygon, Data Join and Area Calculations: 
##    Using poly.counts(); left_join(); poly.areas(); 
########################################################

torn.count <- poly.counts(torn, us_states) 
head(torn.count)
stateid<-names(torn.count)

n<-49
new_stateid<-c(1:n); new_statename<-c(1:n)

for (i in 1:n) {
  new_stateid[i]<-stateid[i]
  new_statename[i]<-as.character(us_states$STATE_NAME[as.numeric(stateid[i])])
}

# create new table
dfnew=data.frame(new_stateid, STATE_NAME=new_statename, torn.count)

# create us_state attribute
library(dplyr)
us_states@data<- left_join(us_states@data, dfnew)
new_us.attr<-us_states@data

for (i in 1:n) {
  if( is.na(us_states$torn.count[i]) ) {us_states$torn.count[i]=0}
}

proj4string(us_states2)
us_states$AREA.KM2<-poly.areas(us_states2) / (1000 * 1000)

attach(us_states@data)
us_states$torn.density<-torn.count*1000/AREA.KM2
vacant.shades = auto.shading(us_states$torn.density,n=6)
choropleth(us_states,us_states$torn.density) 
choro.legend(-76.23261,34.20205,vacant.shades)

locator()
detach(us_states@data)

###############################################
## 5. Distance Analysis: 
##    Using gDistance() and gWithinDistance()
###############################################

data(newhaven)
proj4string(places) <- CRS(proj4string(blocks)) 
plot(blocks)
points(places, col="red", pch=16)

centroids <- gCentroid(blocks, byid = T, id = rownames(blocks))
points(centroids, col="blue", pch=16, cex=0.6)

proj4string(centroids)<- CRS(proj4string(blocks)) 
distances <- ft2miles(gDistance(places, centroids, byid = T))

####### using apply() function #
?apply

nearest <- apply(distances,1, mean)
nearest[1]
nearest <- unname(nearest)

nb <- which.min(nearest)
nearest[nb]
blocks@data[nb,]

#######

distances_2 <- gWithinDistance(places, blocks, byid = T, dist = miles2ft(1.2))

Hosp1 <- distances_2[,1]
plot(blocks)
plot(blocks[Hosp1,], col="yellow", add=TRUE)
points(places[1,], col="red", pch=16, cex=1.2)

length(blocks)
length(blocks[Hosp1,])
sum(blocks$POP1990[Hosp1])
sum(blocks$POP1990[Hosp1] * blocks$P_WHITE[Hosp1]/100 )

#######

# Example 1
min.dist <- apply(distances,1, min)
access <- min.dist < 1
plot(blocks)
plot(blocks[access,], col="yellow", add=TRUE)
points(places, col="red", pch=16, cex=1.2)

# Example 2
# extract the ethnicity data from the blocks variable
ethnicity <- as.matrix(data.frame(blocks[,14:18])/100) 
ethnicity <- apply(ethnicity, 2, function(x) (x * blocks$POP1990))
ethnicity <- matrix(as.integer(ethnicity), ncol = 5) 

colnames(ethnicity) <- c("White", "Black", "Native", "Asian", "Other")

access.eth<-xtabs(ethnicity~access) 

#Stacked Bar Plot
barplot(access.eth, names.arg=colnames(ethnicity), legend = rownames(access.eth), 
        main="Access to Hospitals with 1 miles") 