---
title: "Machine Learning course project"
author: "Naomi"
date: "3/25/2018"
output: html_document
---
## Project summary

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively.

One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, the goal is to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. The goal of this project is to predict the manner in which they did the exercise. 

##Data Import and Cleaning
```{r setup, echo = TRUE}
library(readr)
library(lattice)
library(ggplot2)
library (caret)
library(randomForest)
library(rpart)
library(rpart.plot)

training = read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
testing = read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

set.seed(13)
inTrain  <- createDataPartition(training$classe, p=0.75, list=FALSE)
training2 <- training[inTrain, ]
testing2 <- training[-inTrain, ]
dim(training2)
dim(testing2)

#Remove variables with nearly zero variance
nearzero<-nearZeroVar(training2)
training2<-training2[,-nearzero]
testing2<-testing2[,-nearzero]

dim(training2)
dim(testing2)

#Remove variables with mostly NAs
training2 <- training2[,colSums(is.na(training2))==0]
testing2<-testing2[,colSums(is.na(testing2))==0]

dim(training2)
dim(testing2)

#Remove ID columns
training2<-training2[,-(1:5)]
testing2<-testing2[,-(1:5)]

dim(training2)
dim(testing2)
```

##Prediction algorithms
This section will use classification trees, random forests, and gbm to predict the outcome. 
```{r exploratory}
#Test different prediction models and accuracy
control <- trainControl(method = "cv", number = 5)

#Decision tree using rpart
set.seed(13)
modFitRPart<-train(classe~., data=training2, method="rpart", trControl=control)
prp(modFitRPart$finalModel)
print(modFitRPart)

#Predict on test set
pred_rpart<-predict(modFitRPart, testing2)
confusionMatrix(pred_rpart, testing2$classe)
```

##Random Forest model
```{r model2}
#Random Forest
set.seed(13)
modFit_RF<-randomForest(classe~., data=training2)

#Predict on test set
pred_rf<-predict(modFit_RF, testing2)
confusionMatrix(pred_rf, testing2$classe)


```

##Boosted model (gbm)
```{r model3}
#Boosted model (GBM)
set.seed(13)
modFitGBM<-train(classe~., method ="gbm", data=training2, trControl=control, verbose=FALSE)

#Predict on test set
pred_gbm<-predict(modFitGBM, testing2)
confMat<-confusionMatrix(pred_gbm, testing2$classe)

```

The accuracy for each method was as follows.
Decision tree: 0.5228
Random forest: 0.9971
Gradient boosting: 0.9882 

As random forest is the most accurate, with a `r 1- .9971` out of sample error. We now use the random forest to predict the outcome variable (classe) on the test set.

```{r testprediction}
predict(modFit_RF, testing)

```
