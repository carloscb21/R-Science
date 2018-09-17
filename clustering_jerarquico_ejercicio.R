
# Lectura de los datos a utilizar
crime <- read.csv("/home/carlos/Escritorio/USArrests.csv", header = TRUE)
crime$X <- NULL
summary(crime)

murder <- crime$Murder
murder_estandarizado <- scale(crime$Murder)
summary(murder)
summary(murder_estandarizado)

hc <- hclust(dist(crime), "ward.D")
hc 

# Visualizamos en pantalla un dendograma para elegir el n?mero de cl?steres
plot(hc, hang = -1, labels= crime$X)
rect.hclust(hc, k=10)

crime2 <- crime
crime2$Assault <- scale(crime2$Assault)
crime2$UrbanPop <- scale(crime2$UrbanPop)
crime2$Rape <- scale(crime2$Rape)
crime2$Murder <- scale(crime2$Murder)
hc2 <- hclust(dist(crime2), "ward.D")

clust.means <- function(x, res.clust, groups)
{
  if(!is.matrix(x))
    x <- as.matrix(x)
  means <- tapply(x, list(rep(cutree(res.clust, groups), ncol(x)), col(x)), mean)
  dimnames(means) <- list(NULL, dimnames(x)[[2]])
  return(as.data.frame(means))
}

# Obtenci?n de dos centroides del juego de datos sin estandarizar
clust.means(crime, hc, 2)
# Obtenci?n de dos centroides del juego de datos estandarizado
clust.means(crime2, hc2, 2)


# Funci?n hclust() cuando se utiliza la distancia Manhattan dist() y el m?todo de Ward para fusionar los cl?steres. Calculamos los centroides mediante la funci?n clust.means.
hc3 <- hclust(dist(crime, method = "manhattan"), "ward.D")
clust.means(crime, hc3, 2)

# Centroides a partir de una jerarquizaci?n con distancia euclidiana y el m?todo de fusi?n de Ward
hc4 <- hclust(dist(crime), "ward.D")
# Centroides a partir de una jerarquizaci?n con distancia euclidiana y el m?todo de fusi?n completo
hc5 <- hclust(dist(crime), "complete")

clust.means(crime, hc4, 4)
clust.means(crime, hc5, 4)
