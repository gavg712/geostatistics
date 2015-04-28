###ESTADISTICA DESCRIPTIVA

##OPCIONAL: Fijar la configuración regional de R a un lenguaje internacional
#Sys.setlocale(category = "LC_ALL", locale = "C")

##Instalación y activación del paquete necesario
#install.packages("aspace") #solo si es necesario
require(aspace)

##Lectura del set de datos con las observaciones x y en dos columnas.
##Nota: Copiar antes el archivo points.dat en el disco D o bien copiar en
##otra dirección pero tomar en cuenta que debe cambiar la dirección de
##abajo por la dirección donde copie el archivo en su computadora

observations <- read.table("points.dat", header = T)

#################
##Cálculo de la media espacial
mean_centre(id=1, weighted=FALSE,weights=NULL, points=observations)

#################
##Cálculo de la distancia estándar
calc_sdd(id=1,centre.xy=NULL,calccentre=TRUE,weighted=FALSE,weights=NULL, 
        points=observations, verbose=FALSE)

##Gráfico de la distancia estándar
plot_sdd(plotnew=TRUE,plothv=FALSE,plotweightedpts=FALSE,weightedpts.col='black', 
        weightedpts.pch=19, plotpoints=TRUE,points.col='black',
        points.pch=1, plotcentre=TRUE, centre.col='blue',centre.pch=19,
        titletxt="Distancia Estándar", xaxis="Oeste (m)",yaxis="Norte (m)",
        sdd.col='black', sdd.lwd=2)

#################
##Cálculo de la elipse de desviación estándar
calc_sde(id=1,centre.xy=NULL,calccentre=TRUE,weighted=FALSE,weights=NULL, 
        points=observations, verbose=FALSE)

##Gráfico de la elipse de desviación estándar
plot_sde(plotnew=TRUE,plotSDEaxes=FALSE,plotweightedpts=FALSE,weightedpts.col='black', 
        weightedpts.pch=19, plotpoints=TRUE,points.col='black',
        points.pch=1, plotcentre=TRUE, centre.col='blue',centre.pch=19,
        titletxt="Elipse de desviación estándar ", xaxis="Oeste(m)",yaxis="Norte (m)", 
        sde.col='black', sde.lwd=2)


##################################
###ESDA
##Instalación y activación de los paquetes necesarios
#install.packages("maptools") #solo si es necesario
require(maptools)
#install.packages("deldir") #solo si es necesario
require(deldir)
require(sp)

##Lectura del set de datos a analizar: Sectores_censales_variables.
##Nota: Copiar antes todos los archivos del shapefile
##Sectores_censales_variables_centroids en el disco D o bien copiar en otra
##dirección pero tomar en cuenta que debe cambiar la dirección de abajo
##por la dirección donde copie el archivo en su computadora
variablesshp <- readShapePoints("Sectores_censales_variables_centroids.shp")

##Ploteo del archivo shape
plot(variablesshp)

#################
##Lectura de la variable porcentaje de población indígena en cada sector
##censal y conversión de esta variable a un vector numérico. Asignación
##del nombre “Indígena” a esta variable
Indigena <- as.numeric(variablesshp$P_Ind.gena)

#################
###Generación del histograma para la variable Indígena
hist(Indigena)

#################
##Generación del Q-Q plot para la variable Indígena
##Generación de una distribución normal con 428 observaciones, ya que
##existen 428 registros censales en el archivo shape

rnorm(428)
curvanormal <- rnorm(428)

##Comprobación de la normalidad de esta curva usando el histograma
hist(curvanormal)

##Generación del Q-Q plot
qqplot(Indigena, curvanormal)
qqline(curvanormal)

#################
##Generación del Box plot para la variable Indígena
boxplot(Indigena)

#################
##Generación del diagrama de Voronoi para la variable Indígena

##Extracción de las coordenadas x,y del shapefile
x <- variablesshp$x <- as.numeric(variablesshp$x)
y <- variablesshp$y <- as.numeric(variablesshp$y)

##Cálculo del diagrama de Voronoi
voronoi <- deldir(x, y, dpl=NULL, rw=NULL, eps=1e-09, sort=TRUE, plotit=FALSE,
        digits=6, z=NULL, zdum=NULL, suppressMsge=FALSE)
        
## gráfico del voronoi
#png("varonoi.png") #para exportar el gráfico en formato PNG
plot(x,y, pch=20, col=grey(.7))
plot(voronoi.areas, fillcol=wcols, close=TRUE, wlines="tess", lty="solid", add=TRUE, pch=".", col=grey(.5))
text(x,y-40,labels=round(variablesshp$P_Ind.gena,2), cex=0.65)
#dev.off() #desactiva la ventana virtual de plot

###VORONOI COLOREADO
##Lectura del archivo shape
#install.packages("maptools")
#Sys.setlocale(category = "LC_ALL", locale = "C") #Solo en caso de ser necesario

##Voronoi con colores
require(maptools)
require(deldir)
require(grDevices)

##Leer el fichero SHP
variablesshp <- readShapePoints("Sectores_censales_variables_centroids.shp")

##Extraer coordenadas y valores
x <- variablesshp$x <- as.numeric(variablesshp$x)
y <-variablesshp$y <- as.numeric(variablesshp$y)
Indigena <- as.numeric(variablesshp$P_Ind.gena)

##Generar los poligonos del objeto voronoi
voronoi <- tile.list(deldir(x, y, dpl=NULL, rw=NULL, eps=1e-09, sort=TRUE, plotit=FALSE,
                digits=6, z=NULL, zdum=NULL, suppressMsge=FALSE))

##Grafico del voronoi
#png("voronoicoloreado.png", height=600, width=600)
colores <- colorRampPalette(c("red", "yellow","green","blue"))(length(Indigena))
plot(x,y, pch=".")
plot(voronoi,fillcol=colores,close=TRUE, pch=".")
text(x,y,labels=round(Indigena,2), cex=0.5)
#dev.off()

#OTRA FORMA DE GRAFICAR EL VORONOI
library(ggplot2)
library(reshape)

## creamos el data frame que será el espacio de trabajo
coord<-data.frame(x,y, Indigena) 
##como líneas
out.plot <- ggplot(data=coord, aes(x=x,y=y)) +
    ##Trazar las líneas de las tesselas
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),size = 0.5, 
                data = voronoi$dirsgs,linetype = 1, color= "#80a6c9") + 
    ##Dibujar los puntos
    geom_point(fill=rgb(70,130,180,255,maxColorValue=255), pch=20, size = 0.5,
    color="#4b5794") + 
    ##Dibujar las etiquetas
    geom_text(aes(label=round(Indigena,2)), hjust=0, vjust=0, 
    size=2, color="#313038")
out.plot

###VORONOI COMO POLIGONOS
##si alguno de estos paquetes no está instalado se debe hacerlo
require(ggplot2)
require(deldir)
require(scales)
require(reshape2)
require(plyr)

#Sys.setlocale(category = "LC_ALL", locale = "C") #Solo en caso de ser necesario

##Leer el fichero SHP
variablesshp <- readShapePoints("Sectores_censales_variables_centroids.shp")
labels <- as.numeric(variablesshp$P_Ind.gena)

##Extracción de las coordenadas x,y del shapefile
x <- variablesshp$x <- as.numeric(variablesshp$x)
y <- variablesshp$y <- as.numeric(variablesshp$y)

##Cálculo del diagrama de Voronoi 
voronoi <- deldir(x, y, dpl=NULL, rw=NULL, eps=1e-09, sort=TRUE, plotit=FALSE,
        digits=6, z=NULL, zdum=NULL, suppressMsge=FALSE)

##Convertir las lineas en poligonos
voronoipol <- unclass(tile.list(voronoi))
voronoipol <- lapply(tilelist, function(l){
                     data.frame(x = l$x, y = l$y)
                     })

##Construir un data frame con los datos que iran en el grafico
voronoidf <- melt(voronoipol, id.vars = c('x','y'))[,c(3,1:2)]
names(voronoidf) <- c('pol','x','y')

ggplot(data=coord, aes(x=x,y=y)) +
    ##Trazar los polígonos de las tesselas
    geom_polygon(aes(x = x, y = y, fill = factor(pol)), data = voronoidf, 
    colour = 'grey', alpha = 0.7) +
    ##Dibujar los puntos
    geom_point(fill=rgb(70,130,180,255,maxColorValue=255), pch=20, size = 0.5,
    color="#4b5794") + 
    ##Dibujar las etiquetas
    geom_text(aes(label=round(Indigena,2)), hjust=0, vjust=0, 
    size=2, color="#313038") + theme_bw() + scale_fill_discrete(guide = FALSE) +    
    scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0))
    
#################################
#########CREADO POR PABLO CABRERA BARONA
####MODIFICADO POR GABRIEL GAONA
