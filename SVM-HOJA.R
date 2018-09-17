setwd("~/Escritorio")
library(e1071)
#Breast Cancer Wisconsin
#Aqui se intenta separar dos clases a pacientes a los cuales se les ha dignasticado cancer y se intenta
#averiguar si sufren o no sufren la enfermedad apartir de una serie de muestras
dataRaw <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data", sep=",")
summary(dataRaw)
head(dataRaw)

#tiene 32 variables, de las cuales la primera es la id que no nos interesa y la 2nd nos indica si tiene tumor maligno o benigno
#por lo que guardamos la segunda variable
labels <- dataRaw[,2]
data <- dataRaw[,3:32]

NObs <- nrow(data)
NTrain <- round(NObs*0.9)
NTest <- NObs-NTrain

train <- data[1:NTrain,]
labelsTrain <- labels[1:NTrain]

test <- data[(NTrain+1):NObs,]
labelsTest <- labels[(NTrain+1):NObs]

pca <- princomp(train)
train <- predict(pca, train)[,1:2]
test <- predict(pca, test)[,1:2]

#SVM, kernel lineal
trainFact = data.frame(train, y=as.factor(labelsTrain))
svmfit = svm(y~.,data=trainFact,kernel="linear", cross=5)
out = predict(svmfit, train)
NTrain
print(sum(out==labelsTrain))/NTrain

out = predict(svmfit, test)
NTest
print(sum(out==labelsTest))/NTest

#SVM, kernel Radial
svmfit = svm(y~.,data=trainFact,kernel="radial", cross=5)
out = predict(svmfit, test)
print(sum(out==labelsTest))/NTest

#SVM, kernel Polynomial
svmfit = svm(y~.,data=trainFact,kernel="polynomial", cross=5)
out = predict(svmfit, test)
print(sum(out==labelsTest))/NTest

#SVM, kernel sigmoide
svmfit = svm(y~.,data=trainFact,kernel="sigmoid", cross=5)
out = predict(svmfit, test)
print(sum(out==labelsTest))/NTest


#############3
#SVM sin PCA

train <- data[1:NTrain,]
labelsTrain <- labels[1:NTrain]
test <- data[(NTrain+1):NObs,]
labelsTest <- labels[(NTrain+1):NObs]

trainFact = data.frame(train, y=as.factor(labelsTrain))
svmfit = svm(y~., data=trainFact, kernel = "linear", cross=5)
print(svmfit)
out=predict(svmfit,train)
print(sum(out==labelsTrain))/NTrain

