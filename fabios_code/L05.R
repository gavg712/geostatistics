#############
#  Kriging  #
# Universal #
#############

library(rgdal)
library(maptools)
library(gstat)
library(sp)

setwd("C:\\Users\\FABIO\\Desktop\\Lesson 5")

data<-read.table("data.txt",sep="",header=T) #read txt file
#to read the data can be also used the function in Rcmdr

#transform the data frame into a spatial data frame
coordinates(data)=~Lat+Lon

##Set the coordinate system
proj4string(data)=CRS("+init=epsg:2078")

##the epsg numbers can be found here: http://spatialreference.org/ref/

#import area border from ESRI shape file
border<-readOGR("border.shp","border")
proj4string(border)=CRS("+init=epsg:2078")

#import a raster from a ArcInfo ASCII format
org_mat<-read.asciigrid("org_matter.asc")
proj4string(org_mat)=CRS("+init=epsg:2078")


#Let's first create a prediction grid for the interpolation, starting from the shape file
vals <- border@bbox
deltaLong <- as.integer((vals[1,2] - vals[1,1]) + 1.5)
deltaLat <- as.integer((vals[2,2] - vals[2,1]) + 1.5)
gridRes <-5   #change this value to change the grid size (in metres)
gridSizeX <- deltaLong / gridRes
gridSizeY <- deltaLat / gridRes
grd <- GridTopology(vals[,1],c(gridRes,gridRes),c(gridSizeX,gridSizeY))
pts <- SpatialPoints(coordinates(grd))
pts1 <- SpatialPointsDataFrame(as.data.frame(pts), data=as.data.frame(rep(1,nrow(as.data.frame(pts)))))
Overlay=overlay(pts1,border)
pts1$border=Overlay
nona<-na.exclude(as.data.frame(pts1))
coordinates(nona)=~x+y
gridded(nona) <- TRUE
proj4string(nona)=CRS("+init=epsg:2078")  #remeber to set the coordinate system also for the prediction grid
writeAsciiGrid(nona,"prediction_grid.asc")

#For the Universal kriging we need to obtain the value of the covariate for each observation
#for doing that we can use the function overlay
over=overlay(org_mat,data)
data$org_mat=over$org_matter.asc
str(as.data.frame(data))

#also the prediction grid need to be overlayed with the covariate
over=overlay(org_mat,nona)
nona$org_mat=over$org_matter.asc

#Fitting the variogram
#first, plot the residual variogram
plot(variogram(Oxigen~org_mat,data))

#the, create the variogram model
mod<-vgm(psill=var(data$Oxigen),model="Sph",range=sqrt(areaSpatialGrid(org_mat))/4,nugget=0) #From Spatial-analyst.net

#second, fit the variogram
#let's start trying to fit it the REML model
fit_reml<-fit.variogram.reml(Oxigen~org_mat,data,model=mod)

plot(variogram(Oxigen~org_mat,data),fit_reml,main="REML Model")


#now, we can try to fit the variogram with other algorithm
#for instance with the Ordinary Least Sqaure
fit_ols<-fit.variogram(variogram(Oxigen~org_mat,data),model=mod,fit.method=6)

plot(variogram(Oxigen~org_mat,data),fit_ols,main="OLS Model")


#Kriging 1-out Cross Validation
cross1<-krige.cv(Oxigen~org_mat,data,model=fit_reml)

#Goodness of Fit of the Cross Validation
RSQR<-as.numeric(cor.test(data$Oxigen,cross1$var1.pred)$estimate)^2  #Pearson's R Squared
RMSD<-sqrt(sum((cross1$residual)^2)/length(data$Oxigen))             #Root Mean Square Deviation


#Let's try a cross validation using the OLS model
cross1<-krige.cv(Oxigen~org_mat,data,model=fit_ols)

#Goodness of Fit of the Cross Validation
RSQR<-as.numeric(cor.test(data$Oxigen,cross1$var1.pred)$estimate)^2  #Pearson's R Squared
RMSD<-sqrt(sum((cross1$residual)^2)/length(data$Oxigen))             #Root Mean Square Deviation

#Plot observed versus residuals
plot(data$Oxigen,cross1$var1.pred,asp=1)
abline(0,1,col="red",cex=0.5)


#Interpolation and Map creation
#now that we finished the cross-validation part we can proceed to the creation of the map
map<-krige(Oxigen~org_mat,data,model=fit_ols,newdata=nona)

spplot(map,"var1.pred",col.regions=terrain.colors(50),main="Prediction Map")

spplot(map,"var1.var",col.regions=heat.colors(50),main="Error Map")

#save the map in jpeg
jpeg("Prediction_Map.jpg",800,600)
spplot(map,"var1.pred",col.regions=terrain.colors(50),main="Prediction Map")
dev.off()
