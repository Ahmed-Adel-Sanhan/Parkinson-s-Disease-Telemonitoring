#Loading required packages
install.packages('tidyverse')
library(tidyverse)
install.packages('ggplot2')
library(ggplot2)
install.packages('caret')
library(caret)
install.packages('caretEnsemble')
library(caretEnsemble)
#install.packages('rpart')
library(rpart)
library(randomForest)
library(caret)
parkinsons<- read.csv('C:/Users/ahmed/Downloads/parkinsons.csv',sep=',', header=T)
##check for missing data and make sure no missing data
parkinsons[!complete.cases(parkinsons),]
parkinsons <- subset(parkinsons, test_time >= 0)
summary(parkinsons)

print(parkinsons[parkinsons$subject. == "36",])
print(median(parkinsons$NHR))

parkinsons$NHR[parkinsons$subject. == "36"] <- median(parkinsons$NHR, na.rm = TRUE)
parkinsons <- parkinsons[ , -which(colnames(parkinsons)=="subject.")]
parkinsons <- parkinsons[ , -which(colnames(parkinsons)=="motor_UPDRS")]
parkinsons <- parkinsons[ , -which(colnames(parkinsons)=="test_time")]
#re-code qualitative (factor) variables into numeric
parkinsons$total_UPDRS[parkinsons$total_UPDRS <= 20] <- 1

summary(parkinsons)

parkinsons$total_UPDRS <- replace(parkinsons$total_UPDRS, parkinsons$total_UPDRS > 20 & parkinsons$total_UPDRS <= 40, 2)
parkinsons$total_UPDRS[parkinsons$total_UPDRS >40] <- 3
summary(parkinsons)
sum(is.na(parkinsons))

dim(parkinsons)
sapply(parkinsons, class)
parkinsons$total_UPDRS<- factor(parkinsons$total_UPDRS, 
                                levels = c(1, 2, 3), 
                                labels = c("Low", "Moderate","High"))
missmap(parkinsons)



#Building a model
#split data into training and test data sets
indxTrain <- createDataPartition(y = parkinsons$total_UPDRS,p = 0.80,list = FALSE)
training <- parkinsons[indxTrain,]
testing <- parkinsons[-indxTrain,] 


# applying Random Forest ANALYSIS 

metric <- "Accuracy"
#grid_search
mtry1 <- expand.grid( mtry = c(1,2,4,6,8,10,12))

rf_model <- train(total_UPDRS ~ ., data = training, method = "rf", metric = metric,tuneGrid = mtry1,
                                    trControl = trainControl(method = "cv", number = 10))

plot(rf_model)
predictions <- predict(rf_model, newdata = testing)
confusionMatrix(predictions, testing$total_UPDRS)



# Perform Radial SVM  alghorithem

sigma_values <- c(0.1, 0.5, 1, 2)
C_values <- c(0.1, 1, 10)
tune_grid <- expand.grid(sigma = sigma_values, C = C_values)


#tune_grid <- expand.grid(tuneLength = c(2, 4, 6, 8, 10))
svm_model <- train(total_UPDRS ~ ., data = training, method = "svmRadial", 
                   tuneGrid = tune_grid,preProc = c("center","scale"), metric = metric,
                  trControl = trainControl(method = "cv", number = 10))

plot(svm_model)
best_sigma <- svm_model$bestTune$sigma
best_C <- svm_model$bestTune$C
cat("The best sigma value is", best_sigma, " and C value is", best_C)

# validate our model 
predict <- predict(svm_model, newdata = testing)
confusionMatrix(predict, testing$total_UPDRS)

# specify the range of k values to test
kValues <- data.frame(k = 1:30)

# train the KNN  model
knnModel <- train(total_UPDRS ~ ., data = training, method = "knn", trControl = trainControl(method = "cv", number = 10), tuneGrid = kValues)
plot(knnModel)
testPredictions <- predict(knnModel, newdata = testing)
#summary(knnModel)
bestK <- knnModel$bestTune$k
bestK
confusionMatrix(as.factor(testPredictions), as.factor(testing$total_UPDRS))