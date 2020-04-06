---
output:
  html_document:
    df_print: paged
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Practical Machine Learning Project: Prediction Assignment Writeup
_Chandani Patel_

_06-April-2020_

### Overview 
This document is created for final project submission for Courseara course `Practical Machine Learning` which is part of Machine learning specialization course.
There is data of human activity is provided by courseara which contains accelerometers data on the belt, forearm, arm, and dumbell of 6 participants like the acticity people regularly do, how much of a particular activity they do, how well they do it. The Project is to predict the manner in which they did exercise.

### Tools & Languages
* R
* R Studio
* [Caret package](http://topepo.github.io/caret/index.html) 

## Problem Statement
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

# Datasets

* Training Data: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
* Test Data: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

# Load the libraries
```{r, echo=TRUE}
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(corrplot)
```

# Load Data from CSV & Cleaning the data
This step will load data from csv to dataframe.
Following steps as part of this process
* Load the data from the CSV
* Clean data: remove the variables which has missing values, this will reduce the dataset

```{r, echo=TRUE}
# Load Data sets
trainingData <- read.csv("pml-training.csv", header = TRUE)
validationData <- read.csv("pml-testing.csv", header = TRUE)

# Clean Data
trainData<- trainingData[, colSums(is.na(trainingData)) == 0]
validData <- validationData[, colSums(is.na(validationData)) == 0]
dim(trainData)
dim(validData)

# first seven variables have little impact on the outcome classe, let's remove
trainData <- trainData[, -c(1:7)]
validData <- validData[, -c(1:7)]

# Print Data
dim(trainData)
dim(validData)
```

# Splitting of Data

There will be 2 datasets, 

* training data: this will be 70% of provided training data and used to train the model
* test data: this will be 30% of provided training data and used to test data and quiz results

```{r, echo=TRUE}
set.seed(1234) 
inTrain <- createDataPartition(trainData$classe, p = 0.7, list = FALSE)
trainData <- trainData[inTrain, ]
testData <- trainData[-inTrain, ]
dim(trainData)

```

# Clean Data with near-zero-variance
```{r, echo=TRUE}
NZV <- nearZeroVar(trainData)
trainData <- trainData[, -NZV]
testData  <- testData[, -NZV]
dim(trainData)
dim(testData)
```

After above step the dataset will have 53 vaeiables.

## Correlation plot
Let's put the corelation plot uses the following parameters.
```{r, echo=TRUE}
cor_mat <- cor(trainData[, -53])
corrplot(cor_mat, order = "FPC", method = "color", type = "upper", 
         tl.cex = 0.8, tl.col = rgb(0, 0, 0))
```

In the above graph the correlated predictors (variables) are those with a dark color intersection.

## Find the names of variables
```{r, echo=TRUE}
highlyCorrelated = findCorrelation(cor_mat, cutoff=0.75)
names(trainData)[highlyCorrelated]
```

# Build a Model

This project will build the models with classification trees and random forests to predict the outcome.

Following are three diffrent ways used to predict
* Classification Tree
* Random Forest
* Generalized Boosted Model

## Classification Tree

### Create Model
* Obtain model using rpart
* use fancyRpartPlot to plot the classification tree as a dendogram
```{r, echo=TRUE}
set.seed(12345)
decisionTreeModel <- rpart(classe ~ ., data=trainData, method="class")
fancyRpartPlot(decisionTreeModel)
```

### Prediction
validate the "decisionTreeModel" on testData to find accuracy
```{r, echo=TRUE}
predicttionDecisionTreeModel <- predict(decisionTreeModel, testData, type = "class")
cmtree <- confusionMatrix(predicttionDecisionTreeModel, testData$classe)
cmtree
```

Accuracy Rate: *0.7642* which is low beacuse of this out-of-sample-error is about *0.23* which is considerable

### Plot matrix Results
```{r echo = TRUE}
plot(cmtree$table, col = cmtree$byClass, 
     main = paste("Decision Tree - Accuracy =", round(cmtree$overall['Accuracy'], 4)))
```

## Random Forest

### Create Model
* create trainControl
* Train the Random Forest Model
```{r, echo=TRUE}
# Prediction with Random Forest Tree
controlRandomForest <- trainControl(method="cv", number=3, verboseIter=FALSE)
randomForestModel <- train(classe ~ ., data=trainData, method="rf", trControl=controlRandomForest)
randomForestModel$finalModel
```

### Prediction
validate the "randomForestModel" on testData to find accuracy
```{r, echo=TRUE}
predictRandomForestModel <- predict(randomForestModel, newdata=testData)
cmRandomForestModel <- confusionMatrix(predictRandomForestModel, testData$classe)
cmRandomForestModel
```
Accuracy Rate: *1* which is very high so out-of-sample-error is *0*. This might be case of overfitting

### Plot matrix Results
```{r, echo=TRUE}
plot(cmRandomForestModel$table, col = cmRandomForestModel$byClass, main = paste("Random Forest Confusion Matrix: Accuracy =", round(cmRandomForestModel$overall['Accuracy'], 4)))

```

## Generalized Boosted Model

### Create Model
* create trainControl
* Train the Generalized Boosted Model
```{r, echo=TRUE}
set.seed(12345)
controlGBM <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
modelGBM  <- train(classe ~ ., data=trainData, method = "gbm", trControl = controlGBM, verbose = FALSE)
modelGBM$finalModel
print(modelGBM)
```

### Prediction
validate the "modelGBM" on testData to find accuracy
```{r, echo=TRUE}
predictGBM <- predict(modelGBM, newdata=testData)
cmGBM <- confusionMatrix(predictGBM, testData$classe)
cmGBM
```
Accuracy Rate: *0.9731* which is very high so out-of-sample-error is *0.06*. This might be case of overfitting

# Applying the validation data to Best Model

The accuracy of the 3 regression modeling methods above are:

Random Forest : 1
Decision Tree : 0.7368
GBM : 0.97

Apply the 20 validation results to randomforest model
```{r, echo=TRUE}
results <- predict(randomForestModel, newdata=validData)
results
```