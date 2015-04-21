##############
# Co-Kriging #
##############

library(rgdal)
library(maptools)
library(gstat)
library(sp)

setwd("/home/gabo/Escritorio/geoestadística_R")  #recuerda se puede usar u doble \\ o u simple /

data<-read.table("L06_data.txt",sep="",header=T) #Lee el fichero txt
#para leer los datos tambien se puede usar la funcion en Rcmdr

#transforma el dataframe en un data frame espacial
coordinates(data)=~x+y

##asignar el sistema de coordenadas
proj4string(data)=CRS("+init=epsg:28992")

##Los numeros epsg se pueden encontrar en: http://spatialreference.org/ref/

#importar el borde del area desde un archio SHP
border<-readOGR("meuse_area.shp","meuse_area")
proj4string(border)=CRS("+init=epsg:28992")

#importar un raster desde un archivo en formato ArcInfo ASCII
zinc<-read.asciigrid("zinc.asc")
proj4string(zinc)=CRS("+init=epsg:28992")

#Graficar sus datos
image(zinc)
plot(data,add=T)
plot(border,add=T)

#o usar la funccion spplot
spplot(zinc, scales=list(draw=T), sp.layout=list("sp.points", data, pch="+"))


#Primero creamos la grid de prediccion para la interpolacion, 
#iniciando desde el fichero SHP
vals <-border@bbox
deltaLong <- as.integer((vals[1,2] - vals[1,1]) + 1.5)
deltaLat <- as.integer((vals[2,2] - vals[2,1]) + 1.5)
gridRes <-50   #Este valor es el tamano de celda (en metros debido al sistema de referencia)
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
proj4string(nona)=CRS("+init=epsg:28992")  #recuerda tambien fijar el sistema de referencia para la grid
writeAsciiGrid(nona,"prediction_grid.asc")


#Para el Co-kriging necesitamos obtener el valor de covarianza para cada observacion
#para hacer eso se puede usar la funcion overlay
over=overlay(zinc,data)
data$zinc=over$zinc.asc
str(as.data.frame(data))

#La grid de prediccion tambien necesita ser sobrepuesta con la covarianza
over=overlay(zinc,nona)
nona$zinc=over$zinc.asc


#Para el Co-kriging, la primera cosa a hacer es crear un objeto con la funcion
# gstat() que contenga la variable y la covarianza
g<-gstat(id="lead",formula=lead~1,data=data)
g<-gstat(g,id="zinc",formula=zinc~1,data=data)


#Ajuste del variograma
#Primero, graficar el variograma de residuales
vario<-variogram(g)
plot(vario)

#Ahora podemos ajustar el modelo lineal de co-regionalizacion (LMC)
g<-gstat(g,id=c("lead","zinc"),model=vgm(psill=cov(data$lead,data$zinc),model="Sph",range=sqrt(areaSpatialGrid(zinc))/4,nugget=0))
g<-fit.lmc(vario,g,model=vgm(psill=cov(data$lead,data$zinc),model="Sph",range=sqrt(areaSpatialGrid(zinc))/4,nugget=0))

plot(vario,g$model)

k<-predict.gstat(g,nona)

#Validacion
#Crear los dataset de entrenamiento y comprobacion
#volver a predecir el LMC
#optimizar la prediccion

i<-sample(nrow(data),round(nrow(data)*10/100)) #excluye 10% de los datos
training<-data[!data$ID%in%i,]
test<-data[data$ID%in%i,]

gv<-gstat(id="lead",formula=lead~1,data=training)
gv<-gstat(gv,id="zinc",formula=zinc~1,data=training)

gv<-gstat(gv,id=c("lead","zinc"),model=vgm(psill=cov(training$lead,training$zinc),model="Sph",range=sqrt(areaSpatialGrid(zinc))/4,nugget=0))
gv<-fit.lmc(variogram(gv),gv,model=vgm(psill=cov(training$lead,training$zinc),model="Sph",range=sqrt(areaSpatialGrid(zinc))/4,nugget=0))

plot(variogram(gv),gv$model)

krige_cross<-predict.gstat(gv,test)
str(krige_cross)

#Goodness of fit indexes
RSQR<-as.numeric(cor.test(test$lead,krige_cross$lead.pred)$estimate)^2  #R cuadrado
RMSD<-sqrt(sum((test$lead-krige_cross$lead.pred)^2)/length(test$lead))  #Root Mean Square Deviation

#Guardar variogramas y mapa 
jpeg("Variogram.jpg",1200,1200,res=300)
plot(vario,g$model)
dev.off()

jpeg("Prediction_Map.jpg",1200,1200,res=300)
spplot(k,"lead.pred",col.regions=terrain.colors(50),main="Prediction Map",scales=list(draw=T))
dev.off()

jpeg("Error_Map.jpg",1200,1200,res=300)
spplot(k,"lead.var",col.regions=heat.colors(50),main="Error Map",scales=list(draw=T))
dev.off()


#Referencias:
#- Applied Spatial Data Analysis with R. Bivand,Pebesma,Gómez-Rubio (2008)
#- cran.r-project.org/web/packages/gstat/vignettes/gstat.pdf
#- #- http://www.css.cornell.edu/faculty/dgr2/teach/R/R_ck.pdf
#- http://spatialreference.org/ref/
