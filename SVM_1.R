#como calcula el SVM

#generamos los datos con una distribucion aleatoria, usamos una semilla seed
set.seed(1011)
#generamos dos nubes de puntos x, 200 puntos en dos dimensiones para 400 valores siguiendo una distribucion normal
x = matrix(rnorm(400),200,2)
#generamos las etiquetas 1 y -1 para cada clase
y = rep(c(-1,1), c(100,100))
#aqui añadimos artifialmente 4.5 a cada uno de los puntos de la primera clase 
#para que nos quede unos datos muy separable y observar mejor el ejemplo
x[y==1,] = x[y==1,] +4.5
#son claramente linealmente separable
plot(x,col=y+3,pch=19)

####
#el objetivo es aprender un model SVM para estos datos
#abrimos la liberia correspondiente
library(e1071)

#generamos un data.frame para los datos x e y
train=data.frame(x,y=as.factor(y))
#y mediante la funcion es svm que le pasamos nuestros datos y etiquetas pues entrenamos nuestro clasificador, nuestro modelo aprendido
svmfit=svm(y~., data=train,kernel="linear", cost=1,scale = FALSE)
#con print tenemos informacion del modelo
#con solo dos vectores de soporte ha sido suficiente para tener una frontera de decision
print(svmfit)
#lo visualizamos
plot(svmfit,train)

####

#veamos como funciona en testing
#generamos otra matriz de datos y sus etiquetas
x = matrix(rnorm(400),200,2)
y = rep(c(-1,1), c(100,100))
x[y==1,] = x[y==1,]+3
plot(x,col=y+3, pch=19)

#vamos a intentar probar que tal funciona el modelo que hemos aprendido antes sobre estas nuevas distribuciones
#generamos un data.frame de test sobre x e y
test=data.frame(x,y=as.factor(y))
#usamos la funcion predict, que recibe un modelo(el que hemos aprendido antes) y los datos de test
#y nos va a hacer un prediccion sobre este modelo de los datos de test
out=predict(svmfit,test)
#y vemos que siguiendo el modelo anterior vemos que se equivoca en una serie de puntos, esto seria el error de test
plot(svmfit,test)

###
#para evaluarlo solo tenemos que mirar la salida de la prediccion y compararlo con las etiquetas
#nos dara un numero que es el porcentaje de la salida de acierto, un 93½, que no esta mal.
sum(out==y)/200 #por los 200 puntos que teniamos.

