###ANALISIS DE REGRESION
##Regresión lineal
##Se usa en este caso el archivo Sectores_censales_variables_centroids.shp. En la 
##regresión se considera la variable "porcentaje de casas que son media agua, choza 
##o covacha" (P_V_ma_c_c) como la variable dependiente o variable respuesta. Sus
##variables predictores o variables independientes son: "porcentaje de casas con 
##techo en estado regular o malo" (P_Tech_r_m), "porcentaje de casas en donde la 
##gente vive en condiciones de hacinamiento" (P_Hacina), "Porcenteje de casas sin 
##alcantarillado" (P_sin_alca), "Porcenteje de casas sin acceso a sistema electrico
##publico" (P_no_sis_e), y "porcentaje de casas ubicados en accesos viales no 
##pavimentados" (P_no_acc_p)

##OPCIONAL: Fijar el directorio de trabajo de forma interactiva
#install.packages("tcltk")
#library(tcltk)
#setwd(tclvalue(tkchooseDirectory()))

##OPCIONAL: Fijar la configuración regional de R
#Sys.setlocale(category = "LC_ALL", locale = "C")

##Lectura del archivo shape
#install.packages("maptools") # en caso de ser necesario instalar
require(maptools)
variablesshp <- readShapePoints("Sectores_censales_variables_centroids.shp")

##Primero se evalúa si existe o no multicolinealidad. Primero se calcula los 
##coeficientes de correlación entre todas las variables independientes. Para 
##calcular al mismo tiempo varios coeficientes de correlación se debe primero crear 
##un data frame de las variables en cuestión y luego correr la regresión

df <-data.frame(variablesshp$P_Tech_r_m,variablesshp$P_Hacina,
                variablesshp$P_sin_alca,variablesshp$P_no_sis_e,
                variablesshp$P_no_acc_p)

head(df)
cor(df)

##Se calculan los Variances Inflation Factors (VIF). En este caso se calcula un 
##VIF de las variables P_Tech_r_m y P_Hacina, usando su coeficiente de correlación 
##que es 0.7762121, que es el coeficiente más alto encontrado en todo el set de 
##variables independientes analizado. El resultado del VIF en este caso es de 4.46 
##que es menor a 5- Siendo este VIF menor a 5, y el mayor VIF que se puede esperar 
##del set de variables independientes, se concluye que se pueden usar todas estas 
##variables en el modelo de regresión

VIF <- 1/(1-max(abs(cor(df)-1)))
VIF

##Cálculo de regresión
regreline <- lm(variablesshp$P_V_ma_c_c ~ variablesshp$P_Tech_r_m + 
                variablesshp$P_Hacina + variablesshp$P_sin_alca + 
                variablesshp$P_no_sis_e + variablesshp$P_no_acc_p)

summary(regreline)

##Test de normalidad (para identificar si hay normalidad)
shapiro.test(resid(regreline))

##Identificación de homocedasticidad / heterocedasticidad
residuos <- rstandard(regreline)
valores_ajustados <- fitted(regreline)
plot(valores_ajustados,residuos)

##Otra forma de identificar si hay normalidad
qqnorm(residuos)
qqline(residuos)

#################
##Regresión lineal espacial o modelo espacial autoregresivo
##Generación de una matriz de coordenadas.
xy <- cbind(variablesshp$x, variablesshp$y)

##Generación de lista de valores vecinos
##install.packages("spdep") #Solo en caso de ser necesario
library(spdep)
nb <- knn2nb(knearneigh(xy))

##Generación de matriz de pesos espaciales
listw <- nb2listw(nb,glist=NULL, style="W", zero.policy=NULL)

##Modelo espacial autoregresivo (spatial autoregressive model, SAR)
sar <- errorsarlm(variablesshp$P_V_ma_c_c~variablesshp$P_Tech_r_m +
                variablesshp$P_Hacina + variablesshp$P_sin_alca +
                variablesshp$P_no_sis_e + variablesshp$P_no_acc_p, listw = listw)

summary(sar)

##Test de normalidad
resi <- residuals (sar)
shapiro.test(resi)

#################
###REGRESION LOGISTICA

## Se lee el archivo CalidadVida_Salud.shp. Este archivo posee tres variables: 
##"calidad de vida" (CaliVida), "estado de salud" (EstaSalud) y "distancia al 
##servicio de salud más cercano"(DistSalud). CaliVida es una variable binaria, en 
##donde 1 representa "buena calidad de vida", 0 lo contrario. EstaSalud es una 
##variable categórica con una escala de 1 a 5, donde 1 representa "muy mala salud", 
##2 "mala salud", 3 "regular salud", 4 "buena salud" y 5 "excelente salud". Se va 
##a considerar a DistSalud y EstaSalud como predictores o variables independientes 
##de CaliVida (variable dependiente)

variableslogi <- readShapePoints("CalidadVida_Salud.shp")

variableslogi$CaliVida
variableslogi$EstaSalud
variableslogi$DistSalud

##Modelo de regresión logística
variableslogi$EstaSalud <- factor(variableslogi$EstaSalud)
logit <- glm(CaliVida ~ EstaSalud + DistSalud, data = variableslogi, family = "binomial")
summary(logit)
predict(logit, variableslogi, type = "response")

##Ejemplo de cálculo de probabilidades específicas. Este ejemplo indica la 
##probabilidad de tener una "buena calidad de vida", si el "estado de salud" es 
##"regular" (valor 3) y si "distancia al servicio de salud más cercano" es 150 
##metros. Se pueden ir variando los valores de EstaSalud y DistSalud para encontrar 
##distintas probabilidades de "buena calidad de vida"
dato <- data.frame(EstaSalud = factor(3),DistSalud = 150)
predict (logit, dato, type = "response")

##Cálculo de los Odds ratios y de los intervalos de confianza para todo
##el modelo de regresión logística
exp(cbind(OR = coef(logit), confint(logit)))

#################
###REGRESION PONDERADA GRAFICAMENTE (GWR) 
##Se lee el archivo: FertPrivSalu.shp. Este archivo tiene tres variables, una 
##medida de "privación socio-económica" (Dep_AHP_N), una medida de "distancia al 
##servicio de salud más cercano" (Dist_healt) y otra de "infertilidad" (NoNacViv). 
##En este ejercicio de GWR se considera NoNacViv como la variable respuesta o 
##dependiente. Las otras dos son las covariables o variables independientes

variablesGWR <- readShapePoly("FertPrivSalu.shp")
variablesGWR$Dep_AHP_N
variablesGWR$Dist_healt
variablesGWR$NoNacViv

##Instalación y activación del paquete necesario
##install.packages("spgwr") #Solo en caso de ser necesario
library(spgwr)

##Validación cruzada para el ancho de banda
B <- gwr.sel(NoNacViv ~ Dep_AHP_N + Dist_healt, data = variablesGWR, 
            gweight = gwr.Gauss, verbose = TRUE)
B

##Modelo GWR
gwr <- gwr(NoNacViv ~ Dep_AHP_N + Dist_healt, data = variablesGWR,
            bandwidth = B, gweight = gwr.Gauss, hatmatrix = TRUE)
gwr

#################################
#########CREADO POR PABLO CABRERA BARONA
####MODIFICADO POR GABRIEL GAONA
