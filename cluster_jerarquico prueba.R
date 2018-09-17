Employee_salaries <- read.csv("/home/carlos/Escritorio/R-science/Employee_Salaries_-_2015.csv")
Employee_salaries 
dim(Employee_salaries)
plot(Current.Annual.Salary)
detach(Employee_salaries)

Cust_Master <- read.csv2("/home/carlos/Escritorio/R-science/Cust_Master.csv")
#x71 es Sensi, va del 1 al 100 segun la sensibilidad al nuevo producto por parte del consumidor
#x7601 es Sales, son las ventas que anuales que tiene
attach(Cust_Master)
summary(Cust_Master)
boxplot(X71)
boxplot(X7601)
plot(X71)
plot(X7601)
plot(X71, X7601)

#podriamos eliminar columnas asi
#Cust_Master[,-1][,-1][,(1:2)]
#Cust_Master[,(3:4)]
#Cust_Master2 <- cbind(Cust_Master[1],Cust_Master[,3:4])  mas la linea Cust_Master2$V1 <- NULL
#pero tambien podemos hacer una nueva tabla
Sensi <- X71
Sensi
Sales <- X7601
Sales
aux_cust <- data.frame(Sensi, Sales)
aux_cust
#veamos la matriz de distancias
dist(aux_cust)^2

#normalizando?? https://rstudio-pubs-static.s3.amazonaws.com/266731_da0e310180c848d89b4075de5ae9c46a.html
aux_cust.scaled <-  scale(aux_cust, center= TRUE, scale=TRUE)
summary(aux_cust.scaled)
hc <- hclust(dist(aux_cust.scaled), "ward.D2")
plot(aux_cust.scaled)

# Visualizamos en pantalla un dendograma para elegir el n?mero de cl?steres
plot(hc, hang = -1, labels= Cust_Master2$V1)
# Marcamos con un rect?ngulo los tres cl?steres principales
rect.hclust(hc, k=3)

detach(Cust_Master)
