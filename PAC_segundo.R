# Cargamos datos
dataCancer <-read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data", sep=",")

#more available data set.. in http://archive.ics.uci.edu/ml/

#vemos un resumen de los datos
summary(dataCancer) #la etiqueta V2 nos dice si el cancer es benigno 357 o maligno 212

#tomamos los datos de interes
labCancer <- dataCancer[,2]
labCancer
dataCancer <- dataCancer[,3:32]
dataCancer

#usamos el algoritmo codificado en R para hacer PCA que es princomp
pcaCancer <- princomp(dataCancer)
pcaCancer
#una cosa que podemos hacer es sumar los valores propios y ver como contribuye a la varianza total, lo dibujamos
cumsum(pcaCancer$sdev)/sum(pcaCancer$sdev)
#el grafico nos inidca que si cogemos 5 vectores propios(los que tienen los 5 valores propios mayores) conservamos mucha varianza
#si cogemos 30 casi tenemos toda la varianza pero con pocos vectores propios conservamos practicamente toda la informacion del conjunto de datos
#eso significa que estos datos estan muy correlacionados, se pueden comprimir mucho
plot(cumsum(pcaCancer$sdev)/sum(pcaCancer$sdev))

#vamos a quedarnos con nuestros datos proyectados, estos son los datos proyectados en el espacio PCA
scoresCancer <- pcaCancer$scores
scoresCancer

#y los dibujamos las 2 primeras componentes del PCs, las que conservan un 94% de la ¿valla?, lo podemos ver en el grafico anterior o cumsum(pcaCancer$sdev)/sum(pcaCancer$sdev)
plot(scoresCancer[,1], scoresCancer[,2])

#podemos usar el PCA para la visualizacion, dibujamos con colores para verlo mas claro cual es benigno o maligno
plot(scoresCancer[labCancer=="M",1], scoresCancer[labCancer=="M",2], col="red")
points(scoresCancer[labCancer=="B",1], scoresCancer[labCancer=="B",2], col="green")
#asi nos permite coger unos datos de dimension 30, pasarlos a 2 dimensiones y ver los resultados asociados.










