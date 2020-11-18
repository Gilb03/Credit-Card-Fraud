#IMPORT RESOURCES
library(dplyr)
library(knitr)
library(class)
library(readr)
library(ggplot2)
library(caret)
library(randomForest)
library(Hmisc)
library(party)
setwd("~/zrc/credit-fraud/data")
set.seed(1937028)



#READ DATA IN; BASIC STATISTICS && CLEAN
cardData <- read.csv("creditcard.csv")
head(cardData)
tail(cardData)
summary(cardData)
glimpse(cardData)



#CONVERSIONS; SAMPLE SPLIT
cardData$Class <- factor(cardData$Class)
train <- cardData[1:150000, ]
test <- cardData[150001:284807, ]

train %>%
  select(Class) %>%
  group_by(Class) %>%
  summarise(count = n()) %>%
  glimpse

test %>%
  select(Class) %>%
  group_by(Class) %>%
  summarise(count = n()) %>%
  glimpse




#RANDOM FOREST (done)
rfModel <- randomForest(Class ~ . , data = train)
test$predicted <- predict(rfModel, test)
library(caret)
confusionMatrix(test$Class, test$predicted)

#OBTAIN BENCHMARK & TOP 10
library(MLmetrics)
F1_all <- F1_Score(test$Class, test$predicted)
F1_all

options(repr.plot.width=5, repr.plot.height=4)
varImpPlot(rfModel,
           sort=T,
           n.var=10,
           main="Top 10 most Important Variables")




#PERFORMANCE TUNING W/ 1
rfModelTrim1 <- randomForest(Class ~ V17,
                             data=train)
test$predictedTrim1 <- predict(rfModelTrim1, test)
F1_1 <- F1_Score(test$Class, test$predictedTrim1)
F1_1


#PERFORMANCE TUNING W/ 2
rfModelTrim2 <- randomForest(Class ~  V17 + V12, 
                             data = train)
test$predictedTrim2 <- predict(rfModelTrim2, test)
F1_2 <- F1_Score(test$Class, test$predictedTrim2)
F1_2


#PERFORMANCE TUNING W/ 3
rfModelTrim3 <- randomForest(Class ~ V17 + V12 + V14,
                             data=train)
test$predictedTrim3 <- predict(rfModelTrim3, test)
F1_3 <- F1_Score(test$Class, test$predictedTrim3)
F1_3


#PERFORMANCE TUNING W/ 4
rfModelTrim4 <- randomForest(Class ~  V17 + V12 + V14 + V10, 
                             data = train)
test$predictedTrim4 <- predict(rfModelTrim4, test)
F1_4 <- F1_Score(test$Class, test$predictedTrim4)
F1_4


#PERFORMANCE TUNING W/ 5 
rfModelTrim5 <- randomForest(Class ~  V17 + V12 + V14 + V10 + V16, 
                             data = train)
test$predictedTrim5 <- predict(rfModelTrim5, test)
F1_5 <- F1_Score(test$Class, test$predictedTrim5)
F1_5


#PERFORMANCE TUNING W/10
rfModelTrim10 <- randomForest(Class ~  V17 + V12 + V14 + V10 + V16 
                              + V11 + V9 + V4 + V18 + V26, 
                              data = train)
test$predictedTrim10 <- predict(rfModelTrim10, test)
F1_10 <- F1_Score(test$Class, test$predictedTrim10)
F1_10


#VISUALIZATIONS
numVariables <- c(1,2,3,4,5,10,17)
F1_Score <- c(F1_1, F1_2, F1_3, F1_4, F1_5, F1_10, F1_all)
variablePerf <- data.frame(numVariables, F1_Score)

options(repr.plot.width=4, repr.plot.height=3)
ggplot(variablePerf, aes(numVariables, F1_Score)) + geom_point() + labs(x = "Number of Variables", y = "F1 Score", title = "F1 Score Performance")




rf10 = randomForest(Class ~  V17 + V12 + V14 + V10 + V16 
                    + V11 + V9 + V4 + V18 + V26,  
                    ntree = 1000,
                    data = train)
options(repr.plot.width=6, repr.plot.height=4)
plot(rf10)
options(repr.plot.width=6, repr.plot.height=4)
plot(rf10, xlim=c(0,100))



#KNN MODEL (done)
str(cardData)
cardData$Class <- as.factor(cardData$Class)
set.seed(1991)
samp <- sample(1:nrow(cardData), round(0.2*nrow(cardData)))
cards <- cardData[samp, ]
index <- createDataPartition(cards$Class, p = 0.75, list = F)
train <- cards[index, ]
test <- cards[-index, ]

library(caret)
library(class)
knn1 <- knn(train = train[,-31], test = test[,-31], cl = train$Class, k = 5)
confusionMatrix(knn1, test$Class, positive = "1")


#NAIVE BAYES
library(class)
bayes <- naiveBayes(Class~., data=train, laplace = 1)
bayes$apriori

pred <- predict(bayes, test)
confusionMatrix(pred, test$Class, positive = "1")

rawpred <- predict(bayes, test, type = "raw")
ptest <- prediction(rawpred[,2], test$Class)
perf <- performance(ptest, "tpr", "fpr")
plot(perf, colorize = T)
performance(ptest, "auc")@y.values

#LINEAR PROBABILITY MODEL 



#DASHBOARD CONFIG 
  # ONLY THE OUTPUTS OF MODEL COMPARISONS WILL LIVE ONT HE DASHBOARD. 