############
# Ordinary #
# Kriging  #
############

library(rgdal)
library(maptools)
library(gstat)
library(sp)

setwd("/home/gabo/Escritorio/geoestadística_R")  #recuerda se puede usar u doble \\ o u simple /

data<-read.table("L04_data.txt",sep="",header=T) #leer fichero txt
#tambien se puede usar la funcion en Rcmdr

str(data)
#3 columnas, los nombres de las columnas de coordenadas son Lat y Lon

coordinates(data)=~Lat+Lon

str(data) #Los objetos son transformados de data.frame a spatialPointsDataFrame

##asignar sistema de coordenadas
proj4string(data)=CRS("+init=epsg:2078")

##los numeros epsg se pueden encontrar aqui: http://spatialreference.org/ref/

#importar el poligno de borde desde un ESRI shape file
border<-readOGR("border.shp","border")
proj4string(border)=CRS("+init=epsg:2078")
str(border)

#importar un raster en formato ArcInfo ASCII
org_mat<-read.asciigrid("org_matter.asc")
proj4string(org_mat)=CRS("+init=epsg:2078")


#primero creamos una grid de prediccion para interpolacion a partir del borde
vals <- border@bbox
vals
deltaLong <- as.integer((vals[1,2] - vals[1,1]) + 1.5)
deltaLat <- as.integer((vals[2,2] - vals[2,1]) + 1.5)
gridRes <-5  #Este valor es el tamano de celda en metros
gridSizeX <- deltaLong / gridRes
gridSizeY <- deltaLat / gridRes
grd <- GridTopology(vals[,1],c(gridRes,gridRes),c(gridSizeX,gridSizeY))
pts <- SpatialPoints(coordinates(grd))
pts1 <- SpatialPointsDataFrame(as.data.frame(pts), data=as.data.frame(rep(1,nrow(as.data.frame(pts)))))
proj4string(pts1)=CRS("+init=epsg:2078")
Overlay<-over(pts1,border)
pts1$border=Overlay
nona<-na.exclude(as.data.frame(pts1))
coordinates(nona)=~x+y
gridded(nona) <- TRUE
proj4string(nona)=CRS("+init=epsg:2078")  #importante asignar el sistema de coordenadas para la grid de prediccion
writeAsciiGrid(nona,"prediction_grid.asc")

##Mira como se muestra la grid
plot(nona)


#Ajuste de modelo de variograma
#Primero graficar el variograma
plot(variogram(Oxigen~1,data))

#luego, crear el modelo de variograma
mod<-vgm(psill=var(data$Oxigen),model="Sph",range=sqrt(areaSpatialGrid(org_mat))/4,nugget=0) #desde Spatial-analyst.net

#Segundo, Ajuste del variograma
#Empezamos probando el ajuste con el modelo REML
fit_reml<-fit.variogram.reml(Oxigen~1,data,model=mod)

plot(variogram(Oxigen~1,data),fit_reml,main="REML Model")


#ahora, probemos el ajuste con algun otro algoritmo
#para ello con el modelo Ordinary Least Sqaure (OLS)
fit_ols<-fit.variogram(variogram(Oxigen~1,data),model=mod,fit.method=6)

plot(variogram(Oxigen~1,data),fit_ols,main="OLS Model")


#salida 1 validacion cruzada de Kriging
cross1<-krige.cv(Oxigen~1,data,model=fit_reml)

#prueba de bondad de ajuste de la validacion cruzada
RSQR<-as.numeric(cor.test(data$Oxigen,cross1$var1.pred)$estimate)^2  #R cuadrado
RMSD<-sqrt(sum((cross1$residual)^2)/length(data$Oxigen))             #Root Mean Square Deviation


#Probemos la validacion cruzada del modelo OLS
cross1<-krige.cv(Oxigen~1,data,model=fit_ols)

#prueba de bondad de ajuste de la validacion cruzada
RSQR<-as.numeric(cor.test(data$Oxigen,cross1$var1.pred)$estimate)^2  #Pearson's R Squared
RMSD<-sqrt(sum((cross1$residual)^2)/length(data$Oxigen))             #Root Mean Square Deviation

#Grafico de observados versus residuales
plot(data$Oxigen,cross1$var1.pred,asp=1,xlab="Observed",ylab="Predicted")
abline(0,1,col="red",cex=0.5)


#Validacion independiente
#para probar una validacion independiente, primero necesitamos separar los datos en
#un dataset de entrenamiento y un dataset de validacion.
#Luego, con el dataset de entrenamiento intenramos predecir los valores del dataset de validacion 

i<-sample(nrow(data),round(nrow(data)*10/100)) #excluye el 10% de los datos

training<-data[!data$ID%in%i,]
test<-data[data$ID%in%i,]

#interpolar con krige en dataset de entrenamiento
krig<-krige(Oxigen~1,training,model=fit_ols,newdata=test)

#prueba de bondad de ajuste de la validacion cruzada
RSQR<-as.numeric(cor.test(test$Oxigen,krig$var1.pred)$estimate)^2  #Pearson's R Squared
RMSD<-sqrt(sum((test$Oxigen-krig$var1.pred)^2)/length(test$Oxigen))             #Root Mean Square Deviation

#Grafico de observedos versus residuales
plot(test$Oxigen,krig$var1.pred,asp=1)
abline(0,1,col="red",cex=0.5)

#Los resultados son aparentemente mejores, esto se debe a que el numero de observaciones es mas pequeno
#en este caso, porque gstat es mas especializado para este tipo de validacion cruzada en estas areas
#pero para grandes areas, a escala de cuenca o paisaje, esta validacion toma mayor tiempo y uso de memoria RAM


#Interpolacion and Creacion del mapa
#Ahora que hemos terminado la valizacion cruzada podermos generar el mapa
map<-krige(Oxigen~1,data,model=fit_ols,newdata=nona)

spplot(map,"var1.pred",col.regions=terrain.colors(50))
