setwd("~/Escritorio")
library(e1071)
#Breast Cancer Wisconsin
#Aqui se intenta separar dos clases a pacientes a los cuales se les ha dignasticado cancer y se intenta
#averiguar si sufren o no sufren la enfermedad apartir de una serie de muestras
dataCancer <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data", sep=",")

#tiene 32 variables, de las cuales la primera es la id que no nos interesa y la segunda nos indica si tiene tumor maligno o benigno
#por lo que guardamos la segunda variable
labCancer <- dataCancer[,2]
labCancer
#y en dataCancer nos quedaremos con los datos "crudos"
dataCancer <- dataCancer[,3:32]
dataCancer

#creamos un data.frame y aprender un modelo svm con estos datos
train = data.frame(dataCancer, y=as.factor(labCancer))
train
svmfit = svm(y~., data=train, kernel="radial", cost=1, scale=FALSE)
svmfit

#solo nos queda predecir que pasarian con nuestros datos
out=predict(svmfit,train)
out
#en out tendriamos si el tumar es maligno o  benigno y lo que haremos es intentar comparar la salida que nos 
#ha dado en out con las etiquetas de los pacientes
100*sum(out==labCancer)/length(labCancer)
#tenemos un 100% de acierto, es debido a que le hemos dado todos los datos para entrenar y le hemos dado los mismos datos para testear
#es decir, el algoritmo ya sabia lo que nos vendria, no ha predecido el futuro, sino simplemente el presente
#lo que ya tenia.

##########

#Crossvalidation Breast Cancer Wisconsin

#para hacerlo mas real seguimos un protocolo de crossvalidation, aqui tnemos una version mas simplificada
#lo primero generaremos unos indices, es decir, numeros al azar del 1 al 569, es decir, una permutacion aleatoria de 569 valores
ind <- sample.int(569)
ind
#y a continuacion haremos un data.frame usando solo 500 muestras de entrenamiento
#es decir, de los 569 pacientes cogemos 500 para entrenar y reservaremoos 69 para testear
#son 69 pacientes que aun no hemos visto
#el objetivo sera entrenar un modelo con lo que hemos visto, 500 pacientes y simular el hecho que nos vengan
#69 pacientes nuevos que no sabemos como son para ver que les diriamos a ellos.
train = data.frame(dataCancer[ind[1:500],], y=as.factor(labCancer[1:500]))
train

svmfit = svm(y~., data=train, kernel="radial", cost=1, scale=FALSE)
svmfit

test=data.frame(dataCancer[ind[501:569],], y=as.factor(labCancer[501:569]))
test
#y predecimos con out
out = predict(svmfit,test)
out
#el porcentaje de acierto seria un 75%
100*sum(out==labCancer[501:569])/69
#es decir, acertariamos un 75% de los pacientes les diriamos que tienen tumor maligno o benigno correctamente
#simplemente habiendo aprendido 500 pacientes anteriores
#obviamente esto se puede mejorar, hay algoritmo mas robustos que podriamos hacer entrenamientos mucho mas serios, esto se trata de una primera aproximacion
#y de hecho si queremos ser un poco mas justos, lo que tendriamos que hacer es repetir el proceso al menos 10 o hasta 100 veces
#y quedarnos con la media de los resultados.
#podriamos sacar estadisticos para tener una cierta confidencia y sabriamos mas o menos
#que podriamos predecir apartir de nuestras muestras de entrenamiento
