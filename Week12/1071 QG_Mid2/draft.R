library(GISTools);library(rgdal);library(sp);library(aspace);library(spatstat)
Crime=readOGR(dsn = "Data", layer = "Crime", encoding="utf8")
TP101=readOGR(dsn = "Data", layer = "TP101", encoding="utf8")
Police=readOGR(dsn = "Data", layer = "PoliceStation", encoding="utf8")
TP=readOGR(dsn = "Data", layer = "Taipei", encoding="utf8")

#1
plot(TP)
points(Crime,cex=0.2,pch=20,col='red')
points(Police,cex=0.4,pch=20,col='blue')
Crime.pt=cbind(Crime@coords[,1],Crime@coords[,2])
Police.pt=cbind(Police@coords[,1],Police@coords[,2])
calc_sde(points=Crime.pt)
plot_sde(plotnew=F, plotcentre=T, titletxt="",cex=2,plotpoints=F,sde.col="red",centre.col="red")
calc_sde(points=Police.pt)
plot_sde(plotnew=F, plotcentre=T, titletxt="",cex=2,plotpoints=F,sde.col="blue",centre.col="blue")
text(315000,2787500,"● 犯罪地點",cex=0.5,col="red")
text(315000,2784500,"● 警力資源",cex=0.5,col="blue")

#2a
Buf101=gBuffer(TP101,width = 1000)
grd=GridTopology(c(TP101@coords[1,1]-950,TP101@coords[1,2]-950),c(100,100),c(20,20))
grd=as.SpatialPolygons.GridTopology(grd,proj4string =TP@proj4string)

#Distance法
pix=c()
for(i in 1:400){if(gDistance(grd[i,],Buf101)==0) pix=c(pix,i)}
grd=grd[pix] #grd[pix,]

num=poly.counts(Crime,grd)
S.m=mean(num);S.v=var(num)
S.vmr=S.v/S.m
S.se=sqrt(2/(length(grd)-1))
S.t=(S.vmr-1)/S.se
pt(S.t,length(grd)-1)

#2b
CR101=gIntersection(Buf101,Crime)
par(mar=c(4,4,2,2),cex=1)
CR101.ppp=ppp(CR101@coords[,1],CR101@coords[,2],as.owin(Buf101))
ND=nndist(CR101.ppp,k=3)
plot(ecdf(ND),verticals = T,lwd=3,cex=0,col="red",main="G function",xlab="distance",ylab="G(d)",xlim=c(0,600))
for(i in 1:100) {lines(ecdf(nndist(rpoint(length(CR101),win=as.owin(Buf101)),k=3)),col="grey",cex=0,verticals = T)}
lines(ecdf(ND),verticals = T,lwd=3,cex=0,col="red")

#3
par(mar=c(4,4,2,2),cex=1)
Right2=function(s) substring(s,nchar(s)-1)
Police$TYPE=Right2(as.vector(Police$單位))
Div=subset(Police,Police$TYPE=="分局")
Sta=subset(Police,Police$TYPE=="出所")
Crime.p=ppp(Crime@coords[,1],Crime@coords[,2],as.owin(TP))
Div.p=ppp(Div@coords[,1],Div@coords[,2],as.owin(TP))
Sta.p=ppp(Sta@coords[,1],Sta@coords[,2],as.owin(TP))
CD=nncross(Crime.p,Div.p) #擺法注意
F_CD=ecdf(CD[,1]) #CD$dist
CS=nncross(Crime.p,Sta.p)
F_CS=ecdf(CS[,1]) #CS$dist
plot(F_CD,col="red",main="F function",xlab="distance",ylab="F(d)")
plot(F_CS,col="blue",add=T)
text(0,0.9,"派出所",col="blue")
text(4000,0.6,"分局",col="red")