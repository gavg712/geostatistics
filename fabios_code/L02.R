 #Librerias necesarias para realizar todo el ejercicio. Si no están disponibles hay que instalarlas
#empleando el commando >>> install.package("nombre_de_librería_faltante", dependecies=TRUE)
library(gdata)

#importar los datos desde el archivo de texto a R
data <-read.table("L02_data.txt", sep=",",header=TRUE )

print(data) #Para visualizar los datos importados
str(data) #para ver un ejemplo del archivo cargado
summary(data) #muestra estadisticas de los datos cargados

# GRAFICOS CON R
##El primero es mediante la funcion scatterplot de la libreria CAR
library(car)
scatterplot(var1~times, reg.line=lm, smooth=TRUE, spread=TRUE, boxplots='xy', span=0.5, xlab="Times", ylab="Variable 1", data=data)

##Graficos simples
plot(data$times,data$var1)

##Modificando graficos simples
plot(data$times,data$var1,xlab="Times",ylab="Variable 1")       #Agregar rotulos de ejes
plot(data$times,data$var1,xlab="Times",ylab="Variable 1",main="Title")	#gregar un titulo al grafico
plot(data$times,data$var1,xlab="Times",ylab="Variable 1",main="Title",pch=16)   #cambiar el simbolo usado
                                                                                #http://voteview.com/symbols_pch.htm
                                                                                #En esa dirección se puede encontrar 
                                                                                #todos los posibles simbolos que pueden
                                                                                #ser usados con la opcion pch
plot(data$times,data$var1,xlab="Times",ylab="Variable 1",main="Title",pch=16,cex=3)	#cambia el tamanio del simbolo
plot(data$times,data$var1,xlab="Times",ylab="Variable 1",main="Title",pch=16,cex=3,col="red")	#cambia el color del simbolo
points(data$times,data$var2,pch=15,cex=3,col="blue")	#Agrega una segunda serie de puntos al grafico actual
plot(data$times,data$var1,xlab="Times",ylab="Variable 1",main="Title",col="red",type="l") #cambiar a lineas en vez de puntos
lines(data$times,data$var2,col="blue")


#multiples graficos
par(mfrow=c(2,1))    #2 filas y 1 columna
plot(data$times,data$var1,xlab="Times",ylab="Variable 1",main="Title",pch=16,cex=3,col="red")
points(data$times,data$var2,pch=15,cex=3,col="blue")

plot(data$times,data$var1,xlab="Times",ylab="Variable 1",main="Title",col="red",type="l")
lines(data$times,data$var2,col="blue")

#Agregar una leyenda
plot(data$times,data$var1,xlab="Times",ylab="Variable 1",main="Title",pch=16,cex=1,col="red")
points(data$times,data$var2,pch=15,cex=2,col="blue")

legVals<-c("variable 1", "variable 2")
legend(1,41,legend=legVals,pch=c(16,15),pt.cex=2,col=c("red","blue"))

#Funcion <<locator>> para ubicar objetos en cualquier parte del area de grafico
plot(data$times,data$var1,xlab="Times",ylab="Variable 1",main="Title",pch=16,cex=1,col="red")
points(data$times,data$var2,pch=15,cex=2,col="blue")

legVals<-c("variable 1", "variable 2")
legend(locator(1),legend=legVals,pch=c(16,15),pt.cex=2,col=c("red","blue"))

text(locator(1),"control", col="red")
