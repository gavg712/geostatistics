#######################
#  Kriging Universal  #
#######################

library(rgdal)
library(maptools)
library(gstat)
library(sp)

setwd("/home/gabo/Escritorio/geoestadística_R")  #recuerda se puede usar u doble \\ o u simple /

data<-read.table("L05_data.txt",sep="",header=T) #lee e importa en fichero txt
#para leer los datos tambien se puede usar la funcion en Rcmdr

#transforma el dataframe en un data frame espacial
coordinates(data)=~Lat+Lon

##asignar el sistema de coordenadas
proj4string(data)=CRS("+init=epsg:2078")

##Los numeros epsg se pueden encontrar en: http://spatialreference.org/ref/

#importar el borde del area desde un archio SHP
border<-readOGR("border.shp","border")
proj4string(border)=CRS("+init=epsg:2078")

#importar un raster desde un archivo en formato ArcInfo ASCII
org_mat<-read.asciigrid("org_matter.asc")
proj4string(org_mat)=CRS("+init=epsg:2078")


#Primero creamos la grid de prediccion para la interpolacion, 
#iniciando desde el fichero SHP
vals <- border@bbox
deltaLong <- as.integer((vals[1,2] - vals[1,1]) + 1.5)
deltaLat <- as.integer((vals[2,2] - vals[2,1]) + 1.5)
gridRes <-5   #Este valor es el tamano de celda (en metros debido al sistema de referencia)
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
proj4string(nona)=CRS("+init=epsg:2078")  #recuerda tambien fijar el sistema de referencia para la grid
writeAsciiGrid(nona,"prediction_grid.asc")

#Para el kriging universal necesitamos obtener el valor de covarianza para cada observacion
#Para que podamos usar la funcion overlay

over=overlay(org_mat,data)
data$org_mat=over$org_matter.asc
str(as.data.frame(data))

#Tambien la grid de prediccion necesida ser sobrepuesta con la covarianza
over=overlay(org_mat,nona)
nona$org_mat=over$org_matter.asc

#Ajuste del variograma
#Primero, graficamos el variograma de residuales
plot(variogram(Oxigen~org_mat,data))

#luego, Creamos el modelo de variograma
mod<-vgm(psill=var(data$Oxigen),model="Sph",range=sqrt(areaSpatialGrid(org_mat))/4,nugget=0) #tomado de Spatial-analyst.net

#Segundo, Ajuste del variograma
#empezamos probando el ajuste del modelo REML
fit_reml<-fit.variogram.reml(Oxigen~org_mat,data,model=mod)

plot(variogram(Oxigen~org_mat,data),fit_reml,main="REML Model")


#ahora, probemos ajustar el variograma con otro algoritmo
#para ello con el Ordinary Least Sqaure
fit_ols<-fit.variogram(variogram(Oxigen~org_mat,data),model=mod,fit.method=6)

plot(variogram(Oxigen~org_mat,data),fit_ols,main="OLS Model")


#salida 1 validacion cruzada de Kriging
cross1<-krige.cv(Oxigen~org_mat,data,model=fit_reml)

#bondad de ajuste de la valizacion cruzada
RSQR<-as.numeric(cor.test(data$Oxigen,cross1$var1.pred)$estimate)^2  #R Cuadrado
RMSD<-sqrt(sum((cross1$residual)^2)/length(data$Oxigen))             #Root Mean Square Deviation


#Ahora, probemos una valizacion cruzada usando el modelo OLS
cross1<-krige.cv(Oxigen~org_mat,data,model=fit_ols)

#bondad de ajuste de la valizacion cruzada
RSQR<-as.numeric(cor.test(data$Oxigen,cross1$var1.pred)$estimate)^2  #Pearson's R Squared
RMSD<-sqrt(sum((cross1$residual)^2)/length(data$Oxigen))             #Root Mean Square Deviation

#grafico de los observados versus los residuales
plot(data$Oxigen,cross1$var1.pred,asp=1)
abline(0,1,col="red",cex=0.5)


#Interpolacion and creacion del mapa
#ahora finalizamos la valizacion cruzada podemos proceder con la creacion del mapa
map<-krige(Oxigen~org_mat,data,model=fit_ols,newdata=nona)

spplot(map,"var1.pred",col.regions=terrain.colors(50),main="Prediction Map")

spplot(map,"var1.var",col.regions=heat.colors(50),main="Error Map")

#guardar el mapa en jpeg
jpeg("Prediction_Map.jpg",800,600)
spplot(map,"var1.pred",col.regions=terrain.colors(50),main="Prediction Map")
dev.off()
