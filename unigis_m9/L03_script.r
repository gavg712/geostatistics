###ANALISIS DE PATRONES

##OPCIONAL: Fijar el directorio de trabajo de forma interactiva
#install.packages("tcltk")
#library(tcltk)
#setwd(tclvalue(tkchooseDirectory()))

##OPCIONAL: Fijar la configuración regional de R
#Sys.setlocale(category = "LC_ALL", locale = "C")

##Lectura del archivo shape
#install.packages("maptools")
require(maptools)
variablesshp <- readShapePoints("Sectores_censales_variables_centroids.shp")

#################
###Moran ́s I
##Instalación y activación del paquete necesario
#install.packages("ape") #solo en caso de ser necesario
library(ape)

##Generación de la matriz de pesos en función de la distancia inversa
dist <- as.matrix(dist(cbind(variablesshp$x, variablesshp$y)))
dist.inv <- 1/dist
diag(dist.inv)<-0

##Cálculo del Moran ́s I de la variable “P_sin_alca” (porcentaje de personas sin alcantarillado)
Moran.I(variablesshp$P_sin_alca,dist.inv)

#################
###Geary ́s C Statistic
##Instalación y activación del paquete necesario
#install.packages("spdep") #solo en caso de ser necesario
#install.packages('RANN') #solo en caso de ser necesario
library(spdep)
require(RANN)

##Generación de una matriz de coordenadas. Nota: Se utiliza el mismo
##archivo usado en el Moran ́s I
xy <- cbind(variablesshp$x, variablesshp$y)
#xy

##Generación de lista de valores vecinos
nb <- knn2nb(knearneigh(xy))
#nb

##Generación de matriz de pesos espaciales
listw <- nb2listw(nb,glist=NULL, style="W", zero.policy=NULL)

##Cálculo del Geary ́s C Statistic para la variable “P_sin_alca” (porcentaje de 
##personas sin alcantarillado)
geary.test(variablesshp$P_sin_alca,listw,randomisation=TRUE,
zero.policy=NULL,alternative="greater", spChk=NULL, adjust.n=TRUE)

#################
###Getis-Ord General G
##Nota: Seguir los mismos pasos que se siguió para calcular el índice Geary ́s C 
##Statistic hasta el cálculo de los pesos espaciales (listw)

##Cálculo Getis-Ord General G de la variable “P_sin_alca” (porcentaje de
##personas sin alcantarillado)
globalG.test(variablesshp$P_sin_alca, listw, zero.policy=NULL,alternative ="greater", 
spChk=NULL, adjust.n=TRUE, B1correct=TRUE)


#################
###Ripley ́s K statistic
##install.packages("maptools")
require(maptools)

##Lectura del archivo shape de puntos: Sectores_censales_variables_centroids.
##Nota: Copiar antes todos los archivos del shapefile Sectores_censales_variables_centroids
##en el disco D o bien copiar en otra direccion, pero tomar en cuenta que debe
##cambiar la direccion de abajo por la direccion donde copie el archivo en su computadora

variablesshppuntos <- readShapePoints("Sectores_censales_variables_centroids.shp")

##Conversión de variablesshppuntos en un patrón de puntos. Nota: Para esto (y para 
##visualizar el Ripley's K statistic) se necesita la librería “spatstat”. Si hay 
##problemas corriendo esta libreria en RStudio, instale la consola de R.3.1.2 
##(se la puede bajar de internet) y después realice el proceso indicado 
##del Ripley ́s K statistic.

##Para instalar el paquete en Ubuntu es necesario actualizar la version de R. Para
##eso se necesita agregar el repositorio de R y actualizar los paquetes. Ver como 
## hacerlo en> http://cran.rstudio.com/
#install.packages("spatstat")
library(spatstat)

ppp <- as(variablesshppuntos,"ppp")

##Visualización del Ripley ́s K statistic 
K<-Kest(ppp,correction="Ripley")
plot(K)

##Visualización de la función L(h)
plot(K, sqrt(iso/pi) ~ r)

#################################
#########CREADO POR PABLO CABRERA BARONA
####MODIFICADO POR GABRIEL GAONA
