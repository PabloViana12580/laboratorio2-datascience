#Universidad del Valle de Guatemala 
#Data science - laboratorio 2 
#Ana laura - 17459
#Pablo Viana - 16091

#Librerias a utilizar 
library(corrplot)
library(debug_contr_error2)
library(car)
library(ipred)

# Datos de entrenamiento provistos por kaggle
datos <- read.csv("train.csv")

#Datos de prueba provistos por kaggle
datos_prueba_kaggle <- read.csv("test.csv")

#--- Resumen de los datos
summary(datos)
str(datos)

#elegimos una semilla para asegurar repetibilidad del proceso
set.seed(7)

#Ponemos el porcentaje de división
porciento <- 60/100

#Obtenemos valores aleatorio de la proporción
muestra_training <- sample(1:nrow(datos), porciento*nrow(datos))

#Pregunta número 1
#------------------

#Dividimos los datos
entrenamiento_60 <- datos[muestra_training,]
prueba_40 <- datos[-muestra_training,]


#Utilizamos las variables númericas del conjunto de datos entrenamiento_60
nums_entrenamiento<-dplyr::select_if(entrenamiento_60, is.numeric)

# Pregunta número 2
# ------------------

#Se realiza un modelo con las variables mas representativas obtenidas del análisis PCA
modelo_lineal_pca <- lm(SalePrice~YearBuilt+YearRemodAdd+TotalBsmtSF+X1stFlrSF+GrLivArea+GarageCars,data=entrenamiento_60)

#Se realiza una predicción con los datos de prueba
predic_pca <- predict(modelo_lineal_pca, newdata = prueba_40)
#Se agrega una nueva columna a la tabla
prueba_40$prediccion_pca <- predic_pca
#Se saca la diferencia entre el precio real y el predicho por el algoritmo
dif<-abs(prueba_40$SalePrice-prueba_40$prediccion_pca)
#Se adjunta la diferencia como una nueva tabla 
prueba_40$diferencia_pca <- dif

#--------------------- Se adjunta código utilizado en laboratorio 1 de la clase para el análisis PCA ------------------------
# Utilizamos la función complete.cases ya que existen demasiados valores NA en nums
# nums_completo <- nums_entrenamiento[complete.cases(nums_entrenamiento),]
# 
# #TEST ESFERICIDAD DE BARLETT
# nums.NoNa <- na.omit(nums) 
# cortest.bartlett(nums.NoNa)
# #$chisq
# #[1] 98752
# 
# #$p.value
# #[1] 0
# 
# #$df
# #[1] 703
# 
# KMO(nums.NoNa)
# bartlett.test(nums.NoNa)
# #Bartlett's K-squared = 531000, df = 37, p-value <2e-16
# #PCA
# values<- nums.NoNa[,c(7,8,13,14,17,27)]
# View(values)
# #values2<- datos[,c(5,7,8,13,14,17,20,24,27,28)]
# #values2<-datos %>% select(19,20,21,39,44, 47, 50, 55, 62,63)
# #View(values2)
# compPrin <- prcomp(values, scale=T)
# compPrin
# autoplot(compPrin)
# autoplot(compPrin, data = nums_completo,loadings = TRUE, loadings.colour = 'green',loadings.label = TRUE, loadings.label.size = 3)


#Hacemos modelo de regresión lineal con variables númericas 
modelo_lineal_numerico <- lm(SalePrice ~.,data=nums_entrenamiento)
#Se realiza la predicción
predic_numerico <- predict(modelo_lineal_numerico, newdata = prueba_40)
#Se agrega una nueva columna
prueba_40$prediccion_numerico <- predic_numerico
#Se saca la diferencia entre el valor real y el predicho
dif<-abs(prueba_40$SalePrice-prueba_40$prediccion_numerico)
#se agrega una nueva columna con la diferencia
prueba_40$diferencia_numerica <- dif

#Pregunta 3 
#-----------

#Una vez escogido el modelo lineal numerico debido a su mayor valor en Multiple R-squared
#Procedemos a realizar un analisis de correlación para encontrar una multilinealidad entre las variabels que puede estar sesguando el modelo 
nums.cor <- cor(nums_entrenamiento)
corrplot(nums.cor)

#Encontramos variables linealmente dependientes
ld.vars <- attributes(alias(modelo_lineal_numerico_2)$Complete)$dimnames[[1]]

#Las eliminamos de los datos
nums_new <- nums_entrenamiento
nums_new$TotalBsmtSF <- NULL
nums_new$GrLivArea <- NULL

#detectamos la multicolinealidad en el modelo
vif(modelo_lineal_numerico_2)

#Eliminamos todas las variables que hayan resultado con un valor > 5 de la funcion vif
nums_new$YearBuilt <- NULL
nums_new$BsmtFinSF1 <- NULL
nums_new$X1stFlrSF <- NULL
nums_new$BsmtUnfSF <- NULL
nums_new$X2ndFlrSF <- NULL

#Hacemos modelo de regresión lineal con variables númericas despues de eliminar los coeficientes colineales
modelo_lineal_numerico_2 <- lm(SalePrice ~.,data=nums_new)
#Se realiza la predicción
predic_sin_multilinealidad <- predict(modelo_lineal_numerico_2, newdata = prueba_40)
#Se agrega una nueva columna
prueba_40$prediccion_sin_multi <- predic_sin_multilinealidad
#Se saca la diferencia entre el valor real y el predicho
dif<-abs(prueba_40$SalePrice-prueba_40$prediccion_sin_multi)
#se agrega una nueva columna con la diferencia
prueba_40$diferencia_sin_multi <- dif

# Pregunta 4
# ----------

mean(prueba_40$diferencia_numerica, na.rm=TRUE)
#Promedio de error utilizando variables numericas 23183.19
mean(prueba_40$diferencia_pca)
#Promedio de error utilizando variables pca 26843.51
mean(prueba_40$diferencia_sin_multi, na.rm=TRUE)
#Promedio de error utilizando variables numericas y eliminando multicolinealidad 26142.52

# Pregunta 5
# ------------ 
#Se realiza la predicción
predic_prueba <- predict(modelo_lineal_numerico_2, newdata = datos_prueba_kaggle)
#Se agrega una nueva columna
datos_prueba_kaggle$prediccion <- predic_prueba

#Se analizan componentes estadisticos de la predicción
summary(datos_prueba_kaggle$prediccion)
