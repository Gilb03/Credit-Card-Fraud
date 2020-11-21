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




#REGRESSION ANALYSIS
str(cardData)
summary(cardData)
hist(cardData$V1, col="red", main="Transaction Distributions")
hist(cardData$Time, col="blue", main="Transaction Volume")

set.seed(1937028)
train_ind <- sample(nrow(cardData),round(0.75*nrow(cardData)))
train     <- cardData[train_ind,]
test      <- cardData[-train_ind,]


Model1 <- lm(Time~V17,data=train)
Model2 <- lm(Time~V17+ V12,data = train)
prediction1 <- predict(Model1, newdata = test)
prediction2 <- predict(Model2, newdata = test)

pe1 <- residuals(Model1, newdata=test)
pe2 <- residuals(Model2, newdata=test)

#MSE VS RMSE
MSE1 <- mean(pe1^2)
MSE2 <- mean(pe2^2)
RMSE1 <- MSE1^0.5
RMSE2 <- MSE2^0.5

#PERFORMANCE COMPARISON 
print(c(RMSE1, RMSE2))




#RANDOM FOREST 
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



#KNN MODEL 
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


#NAIVE BAYES (?)
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


#DASHBOARD 
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader( title="Creditcard Fraud Dashboard"),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output){
  
}

shinyApp(ui,server)




#DASHBOARD CONFIG 
  # ONLY THE OUTPUTS OF MODEL COMPARISONS WILL LIVE ON THE DASHBOARD. 