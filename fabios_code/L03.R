##############
#   Objetos  #
# Espaciales #
##############

#Lo primero de hacer es importar los aquetes necesarios
#con las siguientes líneas de código
library(rgdal)
library(maptools)
library(sp)

#Con el siguiente comando se puede seleccionar el directorio de trabajo
setwd("/home/gabo/Escritorio/geoestadística_R")  #recuerda se puede usar u doble \\ o u simple /


data<-read.table("L03_data.txt",sep="",header=T) #leer un archivo txt

str(data)
#3 columnas, los nombres de las columnas de coordenadas son Lat y Lon

#linea de código para transformar el data frame en un data frame espacial
coordinates(data)=~Lat+Lon

str(data) #El objeto de datos ha sido transformado desde el data.frame a frame de puntos espaciales

##Configurar el sistema de coordenadas
proj4string(data)=CRS("+init=epsg:2078") #32717 es SP para UTM17S

##Los numeros epsg pueden consultarse aqui: http://spatialreference.org/ref/

#importar el área de borde desde archivo shp
border<-readOGR("border.shp","border")
proj4string(border)=CRS("+init=epsg:2078")
str(border)

#importar un raster a partir de formato ArcInfo ASCII
org_mat<-read.asciigrid("org_matter.asc")
proj4string(org_mat)=CRS("+init=epsg:2078")

#Graficos
#La funcion básica para graficar en el paquete sp es splot

spplot(org_mat, scales=list(draw=T), sp.layout=list("sp.points", data, pch="+")) #sobreponer raster y puntos 

spplot(org_mat, scales=list(draw=T), sp.layout=list("sp.points", data, pch="+"),col.regions=terrain.colors(50)) #cambiar la escala de color

spplot(org_mat, scales=list(draw=T), sp.layout=list("sp.points", data, pch="+"),col.regions=terrain.colors(50),main="Map") #poner un titulo


#Creando un plot paso a paso
#tambien la función plot trabaja bien dentro del paquete sp
image(org_mat)
plot(data,add=T)
plot(border,add=T)

#y aqui se puede usar todos los argumentos de la funcion plot
image(org_mat,col=heat.colors(10))
plot(data,add=T,pch=16,cex=0.5,col="blue",main="Map")
plot(border,add=T)
box("plot")
legend(locator(1),legend=c(1:10),fill=heat.colors(10),bty="n",title="Legend") #muy grande

#cambiar el tamanio de la leyenda
image(org_mat,col=heat.colors(10))
plot(data,add=T,pch=16,cex=0.5,col="blue",main="Map")
plot(border,add=T)
box("plot")
legend(locator(1),legend=c(1:10),fill=heat.colors(10),bty="n",title="Legend",cex=0.5) #aún dentro del plot

##Hagamos esto horizontal
image(org_mat,col=heat.colors(10))
plot(data,add=T,pch=16,cex=0.5,col="blue",main="Map")
plot(border,add=T)
box("plot")
legend(locator(1),legend=c(1:10),fill=heat.colors(10),bty="n",title="Legend",cex=0.7,horiz=T) 

##agregar una barra de escala y la flecha norte
image(org_mat,col=heat.colors(10))
plot(data,add=T,pch=16,cex=0.5,col="blue",main="Map")
plot(border,add=T)
box("plot")
legend(locator(1),legend=c(1:10),fill=heat.colors(10),bty="n",title="Legend",cex=0.7,horiz=T) 
SpatialPolygonsRescale(layout.scale.bar(),offset=locator(1),scale=100,fill=c("white","black"),plot.grid=F)
text(locator(1),"0")
text(locator(1),"100 m")
SpatialPolygonsRescale(layout.north.arrow(),offset=locator(1),scale=35,fill=c("black"),plot.grid=F)

#Para más informacion y detalles:
# - Applied Spatial Data Analysis with R, R.S. Bivand; E.J. Pebesma; V. Gomez-Rubio (2008)
# - Spatial-Analyst.net, http://spatial-analyst.net/wiki/index.php?title=Main_Page
