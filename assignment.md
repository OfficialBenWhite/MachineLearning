# Practical Machine Learning Assignment
Ben White  
22 November 2015  


The data consists of information from set of sensors measuring acceleration when a human subject performs a variety of tasks.

The aim of this project is to see if we can predict the movement/task which is described in the factor varaible "classe" from the other information
The first step is to download the data.


```r
setwd("/Users/bw/RCourse/machineLearning")

urlTrain <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
urlTest <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

fileTrain <- "fileTrain"
fileTest <- "fileTest"

if(!file.exists(fileTrain)){ 
download.file(urlTrain,fileTrain, method = "curl")
}

if(!file.exists(fileTest)){ 
download.file(urlTest,fileTest,method = "curl")
}
```

Read the data in using read.csv


```r
pmlTrain <- read.csv(fileTrain, na.strings=c("NA",""), strip.white=TRUE)
pmlTest <- read.csv(fileTest, na.strings=c("NA",""), strip.white=TRUE)
```

In order to build a model quickly, it makes sense to remove any variables with NAs and also lets get rid of obvious metadata such as timestamp and user_name.


```r
isNA <- apply(pmlTrain, 2, function(x) { sum(is.na(x)) })
preProcessed <- subset(pmlTrain[, which(isNA == 0)], 
select=-c(X, 
user_name,
new_window, 
num_window,
raw_timestamp_part_1, 
raw_timestamp_part_2, 
cvtd_timestamp))

dim(preProcessed)
```

```
## [1] 19622    53
```

We need to split up the "train" set into an actual training and test set for the purpose of model building and validation. For this we use caret and we set the seed. A 60:40 training to test ratio was used.


```
## Loading required package: lattice
## Loading required package: ggplot2
```

##Training a Random Forest

Do not want to waste time with other models so using a random forst without intensive boostrapping.

```r
ctrl <- trainControl(allowParallel=TRUE, method="cv", number=4)
model <- train(classe ~ ., data=training, model="rf", trControl=ctrl)
```

```
## Loading required package: randomForest
## randomForest 4.6-12
## Type rfNews() to see new features/changes/bug fixes.
```

```r
predictions <- predict(model, newdata=testing)
```

View the model


```r
print(model)
```

```
## Random Forest 
## 
## 11776 samples
##    52 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (4 fold) 
## Summary of sample sizes: 8832, 8833, 8831, 8832 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
##    2    0.9889600  0.9860329  0.003022223  0.003823873
##   27    0.9886205  0.9856051  0.002684675  0.003396231
##   52    0.9819118  0.9771171  0.004083260  0.005169329
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 2.
```

Now view the confusion matrix


```r
confusionMatrix(testing$classe, predictions)$table
```

```
##           Reference
## Prediction    A    B    C    D    E
##          A 2232    0    0    0    0
##          B   12 1502    4    0    0
##          C    0   11 1351    6    0
##          D    0    0   29 1256    1
##          E    0    0    3    3 1436
```

```r
#varImp(model)
```

The model is 98.8% accurate. This will do for the scope of the project.

#Submit the Assignment

Model got 100% accuracy, a good result.


```r
answers = predict(model, newdata=pmlTest)

pml_write_files = function(x){
n = length(x)
for(i in 1:n){
filename = paste0("problem_id_",i,".txt")
write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
}
}

pml_write_files(answers)
```


