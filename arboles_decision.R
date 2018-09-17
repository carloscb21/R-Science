install.packages("pbkrtest")
install.packages("rpart.plot")
install.packages("car")
install.packages("caret")
require(rpart);
require(rpart.plot);
require(car);
require(caret)

setwd("~/Escritorio")

#https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Original)
#tiene 699 elementos de dimension 10, solo tiene 2 clases
wdbc = read.table("breast-cancer-wisconsin.data.txt", sep=",", header = FALSE)

#Vamos a construir arboles de decision, iremos ejecutando el codigo para ver como podemos ejecturar
#todos los parametros necesarios para decidir cual es nuestro conjunto de entrenamiento y 
#cual es nuestro conjunto de test y luego construir el arbol.
#tambien construiremos un arbol que incorporara el resultado de aplicar del pca para ver como
#esto puede mejorar nuestro arbol de decision

dim(wdbc) #la V11 hace de variable objetivo que es la queremos predecir

#este conjunto tiene unos cuantos elementos que no estan codificados es decir que falta
#datos en alguna columna, lo que hacemos es recodificarlos y con R nos quedamos solamente con aquellos elementos que tenemos completos. 
wdbc$V7 <- as.numeric(recode(wdbc$V7, "'?'=NA"))
x <- wdbc[complete.cases(wdbc),]
x
dim(x)

#vamos a empezar partiendo este conjunto en dos, uno de entrenamiento y otro de test
#siguiendo la regla de los 2/3, train(2/3) y test(1/3), con lo cual

N=dim(x)[1]
N
all=seq(1,N)
all

#hacemos un samply de esta secuencia para quedarnos con 2/3 a la azar que formaran parte del conjunto de entramiento
#y lo mismo el 1/3 que formara el conjunto de test
#sample -> un vector que puede hacer hasta N valores, el vector es del tamaño N*2/3, el cero indica que no se pueden repetir los numeros
sample(N,N*2/3,0)
train = sort(sample(N,N*2/3,0))
train
#con el setdiff nos quedamos con los valores y dimension que nos falta para 1/3
test = setdiff(all,train)
test

#creamos los conjuntos de entrenamiento y test
xtrain = x[train,]
xtrain
xtest=x[test,]
xtest

#creamos el primer arbol, utilizando los parametros por defecto que utiliza el package rpart
#V1 no entra porque es el identificador y no tieen sentido mezclarlo
#en t1 creamos el arbol itentando predecir la V11 apartir del resto de variables
#prp dibujamos el arbol
attach(x)
t1 = rpart(as.factor(V11) ~ V2+V3+V4+V5+V6+V7+V8+V9+V10, data=xtrain, method="class")
t1
prp(t1,extra=1)

#veamos como de preciso es el arbol, para ello lo que hacemos es predecir como funciona el arbol
#para clasificar los elementos del conjunto de entrada y los del conjunto de test y
#calculamos la matriz de confusion para cada uno de los dos conjuntos
y1train=predict(t1,xtrain,type="class")
y1test=predict(t1,xtest, type = "class")
y1train
y1test

#la matrices de confusion
#en el caso del conjunto de entrenamiento, vemos que tenemos una precision del 95,6%
#mientras que para el conjuntos de test es de 95,61%
confusionMatrix(table(xtrain$V11, y1train))
confusionMatrix(table(xtest$V11, y1test))
#este arbol a usado una variable en cada corte

#vamos a usar el analisis PCA para intentar obtener una combinacion lineal de las variables de entrada
#de forma que tengamos la posibilidad de hacer cortes oblicuos(diagonales)
#vemos la primera componente que tiene un valor de 6.36 que es bastante elevado y explica
# por si sola el 0.660(66%) de la variacion, lo cual esta muy bien.
pca=princomp(xtrain[,2:10])
pca
summary(pca)
#aqui pedimos cuales son las cargas que se asigna a cada variable, veremos que el primer factor
#aunq sean todos negativos nos da mucho peso en V4 igual que en V3 y tambien en V9
#en cambio otras variables tienen pesos menores
pca$loadings[,1]

#esta combinacion lineal esperaremos que capture mejor las caracteristicas de los datos de este conjunto.
#entonces lo que hacemos es añadir una nueva variable llamada C1 al conjunto de entrenamiento y 
#tambien para el conjunto de test utilizando el mismo pca que hemos calculado para el conjunto de entrenamiento
xtrain$C1 = pca$scores[,1]
xtest$C1=predict(pca,xtest)[,1]

#entonces vamos a crear un arbol llamado t2, utilizando esta nueva variable
#este arbol utilizara la nueva variable C1 que solo tendra un corte
t2 = rpart(as.factor(V11) ~ V2+V3+V4+V5+V6+V7+V8+V9+V10+C1, data=xtrain, method="class")
t2
prp(t2,extra=1)
y2train=predict(t2,xtrain,type = "class")
y2test=predict(t2,xtest,type = "class")
y2train
y2test
#volvemos a calcular la matriz de confusion para ver la precision
confusionMatrix(table(xtrain$V11, y2train))
confusionMatrix(table(xtest$V11, y2test))

#el hecho de que el test sea mejor que el entramiento se explica
#de que al tratarse de un arbol muy pequeño, de hecho de un solo nodo,
#no se produce de ninguna manera overfitting, que es problema cuando hacemos crecer
#el arbol de entramiento demasiado.
#vamos a verlo esto forzando los parametros de la creacion del arbol para llegar a construir un arbol puro

#construimos el arbol puro
t3 = rpart(as.factor(V11) ~ V2+V3+V4+V5+V6+V7+V8+V9+V10+C1, data=xtrain, method="class",
           control=rpart.control(minsplit = 0,cp=0.0))#en esta ultima linea le estamos especificando al sistema que queremos llega a construir un arbol
#hasta que el control point sea cero, es decir, que no haya ningun error
prp(t3,extra=1)
y3train=predict(t3,xtrain,type = "class")
y3test=predict(t3,xtest,type = "class")
y3train
y3test
#volvemos a calcular la matriz de confusion para ver la precision
confusionMatrix(table(xtrain$V11, y3train))
confusionMatrix(table(xtest$V11, y3test))
#este ultimo arbol es capaz de clasificar el conjunto de entrada perfectamente alcanzado una precision de 100%
#en cambio,  el conjunto del mismo arbol aplicado al conjunto de test solo tiene una precision del 93%
#lo que tendriamos que hacer seguidamente es utilizar un algoritmo de poda o de pruding para recortar
#el arbol de forma que nos quedara el arbol minimo que maximice la precision en el conjunto de test