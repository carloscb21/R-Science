
# Lectura de los datos a utilizar
crime <- read.csv("/home/carlos/Escritorio/USArrests.csv", header = TRUE)

#Visualizar un resumen de los datos
summary(crime)

# Creamos la tabla de trabajo crime2 con las variables Assault y UrbanPop
crime2 <- cbind(crime[1],crime[,3:4])  

# Visualizaci?n
View(crime2)

crime2$X <- NULL

#Vihc <- hclust(dist(crime2), "ward.D")

hc <- hclust(dist(crime2), "ward.D")
hc 

# Visualizamos en pantalla un dendograma para elegir el n?mero de cl?steres
plot(hc, hang = -1, labels= crime2$X)
# Marcamos con un rect?ngulo los tres cl?steres principales
rect.hclust(hc, k=10)

# Estandarizamos las variables Assault y UrbanPop y lanzamos el algoritmo hclust()
crime3 <- cbind(crime[1],crime[,3:4])
crime3$Assault <- scale(crime3$Assault)
crime3$UrbanPop <- scale(crime3$UrbanPop)
crime3$X <- NULL
hc2 <- hclust(dist(crime3), "ward.D")
# Visualizamos en pantalla un dendograma para elegir el n?mero de cl?steres
plot(hc2, hang = -1, labels= crime3$X)
# Marcamos con un rect?ngulo los tres cl?steres principales
rect.hclust(hc2, k=10)

 # Calculamos los centroides mediante la funci?n clust.means y comparamos
## clust.means - function to find centroids of clusters
## based on example by Venables & Ripley, MASS 4thEd, Page 318 [1]
##
## x            = input data as data.frame or matrix
## res.clust    = object of class "hclust"
## groups       = number of groups to cut dendrogram into
##
## References:
##
## [1] Venables, W.N. and Ripley, B.D. (2002) Modern Applied
##     Statistics with S. 4th Edition. Springer.

clust.means <- function(x, res.clust, groups)
{
  if(!is.matrix(x))
    x <- as.matrix(x)
  print("el corte")
  print(cutree(res.clust, groups))
  print("repetimos el corte por el numero de columnas que hay")
  print(rep(cutree(res.clust, groups), ncol(x)))
  print("hacemos la lista con lo de antes y las columnas que trabajamos")
  print(list(rep(cutree(res.clust, groups), ncol(x)), col(x)))
  means <- tapply(x, list(rep(cutree(res.clust, groups), ncol(x)), col(x)), mean)
  print(means)
  print(dimnames(x))
  print(dimnames(x)[[2]])
  dimnames(means) <- list(NULL, dimnames(x)[[2]])
  return(as.data.frame(means))
}

# Obtenci?n de dos centroides del juego de datos sin estandarizar
clust.means(crime2, hc, 2)
# Obtenci?n de dos centroides del juego de datos estandarizado
clust.means(crime3, hc2, 2)

# Funci?n hclust() cuando se utiliza la distancia Manhattan dist() y el m?todo de Ward para fusionar los cl?steres. Calculamos los centroides mediante la funci?n clust.means.
hc3 <- hclust(dist(crime2, method = "manhattan"), "ward.D")
clust.means(crime2, hc3, 2)

# Centroides a partir de una jerarquizaci?n con distancia euclidiana y el m?todo de fusi?n de Ward
hc4 <- hclust(dist(crime2), "ward.D")
# Centroides a partir de una jerarquizaci?n con distancia euclidiana y el m?todo de fusi?n completo
hc5 <- hclust(dist(crime2), "complete")

# Generamos 2 centroides con los dos m?todos
clust.means(crime2, hc4, 2) 
clust.means(crime2, hc5, 2)

# Generamos 3 centroides con los dos m?todos
clust.means(crime2, hc4, 3)
clust.means(crime2, hc5, 3)

# Generamos 4 centroides con los dos m?todos
clust.means(crime2, hc4, 4)
clust.means(crime2, hc5, 4)

