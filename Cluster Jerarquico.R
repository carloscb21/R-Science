
# Lectura de los datos a utilizar
Cust_Master <- read.csv2("/home/carlos/Escritorio/Cust_Master.csv", header = FALSE)

#Visualizar un resumen de los datos
summary(Cust_Master)

# Creamos la tabla de trabajo Cust_Master2 con las variables V3 (SENSI) y V4 (SALES)
Cust_Master2 <- cbind(Cust_Master[1],Cust_Master[,3:4])  

# Visualizaci?n
View(Cust_Master2)

Cust_Master2$V1 <- NULL

hc <- hclust(dist(Cust_Master2), "ward.D2")

hc <- hclust(dist(Cust_Master2), "ward.D2")

# Visualizamos en pantalla un dendograma para elegir el n?mero de cl?steres
plot(hc, hang = -1, labels= Cust_Master2$V1)
# Marcamos con un rect?ngulo los tres cl?steres principales
rect.hclust(hc, k=10)

# Estandarizamos las variables Assault y UrbanPop y lanzamos el algoritmo hclust()
Cust_Master3 <- cbind(Cust_Master[1],Cust_Master[,3:4])
Cust_Master3$V3 <- scale(Cust_Master3$V3)
Cust_Master3$V3 <- scale(Cust_Master3$V4)
Cust_Master3$V1 <- NULL
hc2 <- hclust(dist(Cust_Master3), "ward.D2")

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
  means <- tapply(x, list(rep(cutree(res.clust, groups), ncol(x)), 
                          col(x)), mean)
  dimnames(means) <- list(NULL, dimnames(x)[[2]])
  return(as.data.frame(means))
}

# Obtenci?n de dos centroides del juego de datos sin estandarizar
clust.means(Cust_Master2, hc, 2)
# Obtenci?n de dos centroides del juego de datos estandarizado
clust.means(Cust_Master3, hc2, 2)

# Funci?n hclust() cuando se utiliza la distancia Manhattan dist() y el m?todo de Ward para fusionar los cl?steres. Calculamos los centroides mediante la funci?n clust.means.
hc3 <- hclust(dist(Cust_Master2, method = "manhattan"), "ward.D")
clust.means(Cust_Master2, hc3, 2)

# Centroides a partir de una jerarquizaci?n con distancia euclidiana y el m?todo de fusi?n de Ward
hc4 <- hclust(dist(Cust_Master2), "ward.D")
# Centroides a partir de una jerarquizaci?n con distancia euclidiana y el m?todo de fusi?n completo
hc5 <- hclust(dist(Cust_Master2), "complete")

# Generamos 2 centroides con los dos m?todos
clust.means(Cust_Master2, hc4, 2) 
clust.means(Cust_Master2, hc5, 2)

# Generamos 3 centroides con los dos m?todos
clust.means(Cust_Master2, hc4, 3)
clust.means(Cust_Master2, hc5, 3)

# Generamos 4 centroides con los dos m?todos
clust.means(Cust_Master2, hc4, 4)
clust.means(Cust_Master2, hc5, 4)
