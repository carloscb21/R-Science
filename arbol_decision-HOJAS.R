install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

dataRaw <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wpbc.data", sep=",")

summary(dataRaw)
head(dataRaw)

N = dim(dataRaw)[1]
all = seq(1,N)
test = seq(1,N,3)
test
train = setdiff(all, test)

xtrain=dataRaw[train,]
xtest=dataRaw[test,]

sum(xtrain[,2] == 'N') *100 /nrow(xtrain)
sum(xtest[,2] == 'N') *100 /nrow(xtest)

fvars <- paste("V2 ~", paste(paste("V", 3:32,sep=""),collapse = "+"))
t1 <- rpart(fvars, data=xtrain, method = "class")

y1train <- predict(t1, xtrain, type="class")
#matriz de confusion
tab.train <- table(xtrain$V2, y1train)
tab.train
#calculamos el porcentaje de acierto
(tab.train[1,1]+tab.train[2,2]) * 100/sum(tab.train)

y1test <- predict(t1, xtest, type="class")
#matriz de confusion
tab.test <- table(xtest$V2, y1test)
tab.test
#calculamos el porcentaje de acierto
(tab.test[1,1]+tab.test[2,2]) * 100/sum(tab.test)


c <- rpart.control(minsplit = 0, cp=0.0)
t2 <- rpart(fvars, data=xtrain, method= "class", control=c)
prp(t2,extra=1)

y2test <- predict(t2,xtest,type="class")
tab2.test <- table(xtest$V2, y2test)
(tab2.test[1,1]+tab2.test[2,2]) * 100/sum(tab2.test)

str(t2)
