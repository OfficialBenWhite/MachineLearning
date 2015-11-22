# - quizzes

# - Question 1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50, list = FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

dim(training)
dim(testing)
summary(testing)

# - Question 2

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

qplot(training$Superplasticizer)
dim(subset(x = training, subset = training$Superplasticizer == 0))
# skewness of non zero values
qplot(log(training$Superplasticizer))
qplot(log(training$Superplasticizer+1))


# - Question 3
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
str(training)
summary(training)
names(training)

colnamesIL<- names(training[,grep("^IL", colnames(training))])
summary(training[,colnamesIL])

preProc <- preProcess(training[, colnamesIL], method = "pca", thresh = 0.8)
preProc$rotation

# as you can see there are 7 compoents
