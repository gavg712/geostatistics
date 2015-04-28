###INTERPOLACION

##OPCIONAL: Fijar el directorio de trabajo de forma interactiva
#install.packages("tcltk")
#library(tcltk)
#setwd(tclvalue(tkchooseDirectory()))

##OPCIONAL: Fijar la configuración regional de R
#Sys.setlocale(category = "LC_ALL", locale = "C")

##Interpolación IDW
##Lectura del set de datos a analizar: Elevacion. Nota: Copiar antes este archivo 
##en el disco D o bien copiar en otra dirección pero tomar en cuenta que debe 
##cambiar la dirección de abajo por la dirección donde copie el archivo en su 
##computadora. El archivo Elevacion.txt posee información de puntos que representan 
##Elevacion sobre el nivel del mar, en metros. Incluye la información de las 
##coordenadas de estos punto.
Elev <- read.table("Elevacion.txt", header = TRUE)
Elev$CoordX
Elev$CoordY
Elev$Elevac

##Creación de un archivo espacial con coordenadas
library (sp)
Elevacion <- Elev
Elevacion$x <- Elevacion$CoordX
Elevacion$y <- Elevacion$CoordY
coordinates(Elevacion) = ~x + y
plot(Elevacion)

##Creación de un grid para la interpolación
rango.x <- as.numeric(c(min(Elevacion$x), max(Elevacion$x)))
rango.y <- as.numeric(c(min(Elevacion$y), max(Elevacion$y)))
grd <- expand.grid(x = seq(from = rango.x[1], 
                            to = rango.x[2], 
                            by = 2000), 
                   y = seq(from = rango.y[1], 
                            to = rango.y[2], 
                            by = 2000))
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE
plot(grd, cex= 1.5, col = "grey")

##Ubicación de puntos de Elevacion en el grid
points(Elevacion, pch = 1, col = "red", cex = 1)


#################
###IDW
#install.packages("gstat") ##solo si es necesario
library(gstat)
IDW <- idw(formula = Elevac ~ 1, locations = Elevacion, newdata = grd)
IDW
IDW.resultado <- as.data.frame(IDW)
names(IDW.resultado)[1:3] <- c("X", "Y", "Elevacion.estimada")
IDW.resultado
#install.packages("ggplot2") ##solo si es necesario
library(ggplot2)
ggplot() + geom_tile(data = IDW.resultado, 
                    aes(x = X, y = Y, fill = Elevacion.estimada)) + 
        geom_point(data = Elev, aes(x = CoordX , y = CoordY), 
                    shape = 18, colour = "red")


##################################
##Semivariograma
#install.packages("geoR") ##solo si es necesario
library(geoR)

##Leer dato
Elev <- read.table("Elevacion.txt", header = TRUE)
head(Elev, 10)

##Explorar un resumen de las distancias entre los puntos de muestreo
distancias <- dist(Elev[,1:2])
summary(distancias)

##Cálculo de distancia h o tamaño de lag si se trabaja con 10 lags. Se
##divide la mitad de la distancia máxima para el número de lags
lag <- round((max(distancias)/2)/10,0)


#################
#Binning
breaks = seq(0, lag, l = 11)


#################
#Generación del semivariograma
semivariograma <- variog(coords = Elev[,1:2], data = Elev[,3], option=c("bin"), 
                        breaks = breaks)

plot(semivariograma,type = "b", main = "Variogram: Elevac")
reporte <- cbind(c(1:length(semivariograma$n)), semivariograma$v, semivariograma$n)
colnames(reporte) <- c("lag", "semivarianza", "Número de pares en bins")
reporte


##################
#Semivariograma sin tomar en cuenta una distancia h predefinida
semivariograma2 <- variog(coords=Elev[,1:2], data=Elev[,3], breaks="default")
plot(semivariograma2,type = "b", main = "Variogram: Elevac")
reporte2 <- cbind(c(1:length(semivariograma2$n)), semivariograma2$v, semivariograma2$n)
colnames(reporte2) <- c("lag", "semivarianza", "Número de pares en bins")
reporte2

#################
#Mismo semivariograma anterior pero a manera de nube de puntos, sin ningún break
semivariograma2.nube.puntos <- variog(coords=Elev[,1:2], data=Elev[,3], 
                                    option = "cloud")
plot(semivariograma2.nube.puntos, main = "classical estimator")

#################

#Sobreposición de una línea de modelo de variograma sobre el modelo empírico. Se 
#utiliza el semivariograma2 como ejemplo

plot(semivariograma2,type = "b", main = "Variogram: Elevac")
lines.variomodel(cov.model = "gaussian", cov.pars = c(2000000, 30000),
nugget = 0, max.dist = 100000, lwd = 3)

##################################

#Kriging Ordinario
Kriging <- krige(formula = Elevac ~ 1, locations = Elevacion, newdata =grd)
spplot(Kriging["var1.pred"], main = "Kriging Ordinario")

#Otra forma de graficar Kriging
spplot(Kriging,col.regions=rev(terrain.colors(100)), 
        names.attr=c("Predictions","Variance"), main="Kriging Ordinario",pch=2,
        cex=2)
        
#################

#Kriging Simple
Kriging.Simple <- krige(formula = Elevac ~ 1, locations = Elevacion, newdata = grd, beta = 10)
spplot(Kriging.Simple["var1.pred"], main = "Kriging Simple")

#################
#Kriging Universal
Kriging.Universal <- krige(formula = Elevac ~ 1, locations = Elevacion,
                            newdata = grd, block = c(1000,1000))
spplot(Kriging.Universal["var1.pred"], main = "Kriging Universal")

#################################
#########CREADO POR PABLO CABRERA BARONA
####MODIFICADO POR GABRIEL GAONA
