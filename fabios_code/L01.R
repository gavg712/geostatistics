# R puede ser usado como calculadora 

3*5

45^2

3/2

sum(3,5,7,2)

#pero R no es solo eso, es un muy potente lenguaje estadistico
# y esta orientado a objetos, tal como muchos otros lenguajes de programacion.
#Esto significa que trabaja con amplias series de objetos y en esta leccion mostrare
#los objetos básicos en R, vectores, matrices y conjuntos de datos.

#Empecemos con los vectores

vector<-c(1,34,56,76,58)

print(vector) #Un vector es como una matriz con una fila

vector[3]+vector[2] #Con el corchete [] puedo seleccionar un elemento de un vector
#en este caso quiero sumar el tercer (56) y el segundo número (34) en el vector

sum(vector[3],vector[2]) #Tambien se puede emplear la funcion sum()

mean(vector)  #esta es la función para calcular el promedio en mi vector


# Matrices
c1<-c(1:10)  #la cadena 1:10 es un rango y significa que R usará los numeros enteros del 1 al 10
c2<-c(10:1)  #esto es el inverso del rango usado anteriormente, en este caso irá desde 10 hasta 1
c3<-seq(from=10,to=100,by=10)  #esta es una secuencia en la cual se puede especificar el inicio, el final y el intervalo


mat<-matrix(c(c1,c2,c3),ncol=3,byrow=F)  #Esta es la funcion usada para crear una matriz

help(matrix)  #esto llama la pagina de ayuda con todas las opciones de la funcion matrix

mat[4,2]+mat[4,1]

mat[4,]

mat[,1]

mat[,1]+3

# Conjuntos de datos (data.frames)

frame<-data.frame(c1,c2,c3)

frame[4,1]

frame[,1] #Igual para seleccionar de una matriz

frame$c1  #importante cuando se importa una tabla

# Guardar congunto de datos

write.table(frame,"test.txt",row.names=F,col.names=F)


