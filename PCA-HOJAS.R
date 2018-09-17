
# Cargamos datos
dataBank_carga<-read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data", sep=" ")

#vemos un resumen de la base de datos
summary(dataBank_carga)

# Seleccionamos solo las 500 primeras filas del juego de datos
dataBank <- dataBank_carga[1:500,]

# N?mero de columnas en la tabla
ncol(dataBank)
# N?mero de observaciones en la tabla
nrow(dataBank)

# Visualizaci?n de la tabla
View(dataBank)

#Separar los datos en: etiquetas y datos
#     En primer lugar seleccionamos las etiquetas, que es la columna 21
labels <- dataBank[,21]
labels
View(labels)
#     En segundo lugar seleccionamos solo las columnas num?ricas
data <- dataBank[,c(2,5,8,11,13,16,18)]
data
# Visualizamos los primeros registros de nuestro juego de datos
head(data)

#Calcular el an?lisis de componentes principales
pca <- princomp(data)
pca

# Visualizamos el % de cantidad de informacion que vamos ganando cada vez que incorporamos una  variable mas.
cumsum(pca$sdev)/sum(pca$sdev)

# Visualizamos gr?ficamente el paso anterior
plot(cumsum(pca$sdev)/sum(pca$sdev))

# Observamos los dos componentes principales y los presentamos en un gr?fico
scores <- pca$scores
plot(scores[,1],scores[,2])

# Generamos un gr?fico mostrando las dos variables que contienen m?s informaci?n
# y coloreamos en rojo los puntos 1 y verde los puntos 2
plot(scores[labels==1,1],scores[labels==1,2],col="red")
points(scores[labels==2,1],scores[labels==2,2],col="green")

