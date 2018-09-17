#ALGORITMO PCA

#Datos
X <- c(2.5,0.5,2.2,1.9,3.1,2.3,2,1,1.5,1.1)
Y <- c(2.4,0.7,2.9,2.2,3,2.7,1.6,1.1,1.6,0.9)

#Inicializamos el algoritmo PCA

#Paso 1, calcular las medias
#Medias
mX <- mean(X)
mY <- mean(Y)
mX
mY

#Paso 2
#centramos los datos, es decir, restamos la media
Data <- cbind(X-mX,Y-mY)
Data

#Matriz de convarianza con los datos centrados
covData <- (1/10)*t(Data)%*%Data
covData

#Paso 3
#Calculamos los vectores y valores propios de la matriz de covarianza
pcas <- eigen(covData)
pcas$vectors
pcas




