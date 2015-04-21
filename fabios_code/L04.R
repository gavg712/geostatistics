############
# Ordinary #
# Kriging  #
############

library(rgdal)
library(maptools)
library(gstat)
library(sp)

setwd("C:\\Users\\FABIO\\Desktop\\lesson4")

data<-read.table("data.txt",sep="",header=T) #read txt file
#to read the data can be also used the function in Rcmdr

str(data)
#3 columns, the names of the coordinates columns are Lat and Lon

coordinates(data)=~Lat+Lon

str(data) #the data object has been transformed from data.frame to SpatialPointsDataFrame

##Set the coordinate system
proj4string(data)=CRS("+init=epsg:2078")

##the epsg numbers can be found here: http://spatialreference.org/ref/

#import area border from ESRI shape file
border<-readOGR("border.shp","border")
proj4string(border)=CRS("+init=epsg:2078")
str(border)

#import a raster from a ArcInfo ASCII format
org_mat<-read.asciigrid("org_matter.asc")
proj4string(org_mat)=CRS("+init=epsg:2078")


#Let's first create a prediction grid for the interpolation, starting from the shape file
vals <- border@bbox
deltaLong <- as.integer((vals[1,2] - vals[1,1]) + 1.5)
deltaLat <- as.integer((vals[2,2] - vals[2,1]) + 1.5)
gridRes <-5  #change this value to change the grid size (in metres)
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

##See how our grid looks like
plot(nona)


#Fitting the variogram
#first plot the variogram
plot(variogram(Oxigen~1,data))

#then, create a variogram model
mod<-vgm(psill=var(data$Oxigen),model="Sph",range=sqrt(areaSpatialGrid(org_mat))/4,nugget=0) #From Spatial-analyst.net

#second, fit the variogram
#let's start trying to fit it the REML model
fit_reml<-fit.variogram.reml(Oxigen~1,data,model=mod)

plot(variogram(Oxigen~1,data),fit_reml,main="REML Model")


#now, we can try to fit the variogram with other algorithm
#for instance with the Ordinary Least Sqaure
fit_ols<-fit.variogram(variogram(Oxigen~1,data),model=mod,fit.method=6)

plot(variogram(Oxigen~1,data),fit_ols,main="OLS Model")


#Kriging 1-out Cross Validation
cross1<-krige.cv(Oxigen~1,data,model=fit_reml)

#Goodness of Fit of the Cross Validation
RSQR<-as.numeric(cor.test(data$Oxigen,cross1$var1.pred)$estimate)^2  #Pearson's R Squared
RMSD<-sqrt(sum((cross1$residual)^2)/length(data$Oxigen))             #Root Mean Square Deviation


#Let's try a cross validation using the OLS model
cross1<-krige.cv(Oxigen~1,data,model=fit_ols)

#Goodness of Fit of the Cross Validation
RSQR<-as.numeric(cor.test(data$Oxigen,cross1$var1.pred)$estimate)^2  #Pearson's R Squared
RMSD<-sqrt(sum((cross1$residual)^2)/length(data$Oxigen))             #Root Mean Square Deviation

#Plot observed versus residuals
plot(data$Oxigen,cross1$var1.pred,asp=1,xlab="Observed",ylab="Predicted")
abline(0,1,col="red",cex=0.5)


#Independent validation
#in order to try an independent validation we first need to subset the data into
#a training and a test subset.
#Then, we use the training dataset to predict the value in the test dataset.

i<-sample(nrow(data),round(nrow(data)*10/100)) #exclude 10% of the data

training<-data[!data$ID%in%i,]
test<-data[data$ID%in%i,]

#krige the trainig subset
krig<-krige(Oxigen~1,training,model=fit_ols,newdata=test)

#Goodness of Fit of the Cross Validation
RSQR<-as.numeric(cor.test(test$Oxigen,krig$var1.pred)$estimate)^2  #Pearson's R Squared
RMSD<-sqrt(sum((test$Oxigen-krig$var1.pred)^2)/length(test$Oxigen))             #Root Mean Square Deviation

#Plot observed versus residuals
plot(test$Oxigen,krig$var1.pred,asp=1)
abline(0,1,col="red",cex=0.5)

#The results are apparently better, just because the number of observation is smaller
#in this case, because the area is a field the best cross-validation is the one embedded into gstat
#but for larger areas, catchment or landscape scale, this validation procedure is less time and RAM consuming


#Interpolation and Map creation
#now that we finished the cross-validation part we can proceed to the creation of the map
map<-krige(Oxigen~1,data,model=fit_ols,newdata=nona)

spplot(map,"var1.pred",col.regions=terrain.colors(50))
