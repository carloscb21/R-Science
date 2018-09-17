setwd("~/Escritorio")
databank_carga <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data",sep=" ")
#View(databank_carga)
summary(databank_carga)

labels <- databank_carga[,21]
labels
length(labels)
#     En segundo lugar seleccionamos solo las columnas num?ricas
data <- databank_carga[,c(2,8,11,13,16,18)]
data

#Calcular el an?lisis de componentes principales
pca <- princomp(data)
pca

# Visualizamos el % de cantidad de informacion que vamos ganando cada vez que incorporamos una  variable mas.
cumsum(pca$sdev)/sum(pca$sdev)
scores <- pca$scores
scores

plot(scores[,1],scores[,2])

plot(scores[labels==1,5], scores[labels==1,6], col="red")
points(scores[labels==2,5], scores[labels==2,6], col = "green")
