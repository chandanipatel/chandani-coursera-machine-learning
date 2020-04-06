library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

# Load Data sets
trainingData <- read.csv("pml-training.csv", header = TRUE)
validationData <- read.csv("pml-testing.csv", header = TRUE)

trainData<- trainingData[, colSums(is.na(trainingData)) == 0]
validData <- validationData[, colSums(is.na(validationData)) == 0]
dim(trainData)
dim(validData)

trainData <- trainData[, -c(1:7)]
validData <- validData[, -c(1:7)]
dim(trainData)
dim(validData)

set.seed(1234) 
inTrain <- createDataPartition(trainData$classe, p = 0.7, list = FALSE)
trainData <- trainData[inTrain, ]
testData <- trainData[-inTrain, ]
dim(trainData)

NZV <- nearZeroVar(trainData)
trainData <- trainData[, -NZV]
testData  <- testData[, -NZV]
dim(trainData)
dim(testData)

cor_mat <- cor(trainData[, -53])
corrplot(cor_mat, order = "FPC", method = "color", type = "upper", 
         tl.cex = 0.8, tl.col = rgb(0, 0, 0))

highlyCorrelated = findCorrelation(cor_mat, cutoff=0.75)

names(trainData)[highlyCorrelated]

# Prediction with classification Tree
set.seed(12345)
decisionTreeModel <- rpart(classe ~ ., data=trainData, method="class")
fancyRpartPlot(decisionTreeModel)

predicttionDecisionTreeModel <- predict(decisionTreeModel, testData, type = "class")
cmtree <- confusionMatrix(predicttionDecisionTreeModel, testData$classe)
cmtree

plot(cmtree$table, col = cmtree$byClass, 
     main = paste("Decision Tree - Accuracy =", round(cmtree$overall['Accuracy'], 4)))

# Prediction with Random Forest Tree
controlRandomForest <- trainControl(method="cv", number=3, verboseIter=FALSE)
randomForestModel <- train(classe ~ ., data=trainData, method="rf", trControl=controlRandomForest)
randomForestModel$finalModel

predictRandomForestModel <- predict(randomForestModel, newdata=testData)
cmRandomForestModel <- confusionMatrix(predictRandomForestModel, testData$classe)
cmRandomForestModel

plot(cmRandomForestModel$table, col = cmRandomForestModel$byClass, main = paste("Random Forest Confusion Matrix: Accuracy =", round(cmRandomForestModel$overall['Accuracy'], 4)))

# Prediction with Boosted Regression Model
set.seed(12345)
controlGBM <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
modelGBM  <- train(classe ~ ., data=trainData, method = "gbm", trControl = controlGBM, verbose = FALSE)
modelGBM$finalModel
print(modelGBM)

predictGBM <- predict(modelGBM, newdata=testData)
cmGBM <- confusionMatrix(predictGBM, testData$classe)
cmGBM

results <- predict(randomForestModel, newdata=validData)
results