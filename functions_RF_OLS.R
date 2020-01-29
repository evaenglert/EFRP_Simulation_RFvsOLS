## FUNCTIONS for comparison of RF and OLS##
library(randomForest)
library(forecast)


### OLS #####
ols_calculator <- function(trainset, testset){
  #OLS_CALCULATOR fits a normal linear model on the input dataset "trainset"
  #and gives predictions based on the data "testset". 
  #Return value: error terms for prediction
  
  
  #fitting linear model on training set
  linearmodel <- lm(y ~ x1 + x2, data = trainset)
  
  #calculating MAE for the Linear Model
  test_x <- testset %>% select(1:2) 
  predictedValue <- predict(linearmodel, test_x)
  trueValue <- testset$y
  predictionErrorTerms <- trueValue - predictedValue
  return(predictionErrorTerms)
}


#### RANDOM FOREST #####
rf_calculator <- function(trainset, testset){
  #RF_CALCULATOR fits a Random Forest on the dataset "trainset" and 
  #gives predictions on the dataset "testset"
  #Return value: error terms for prediction
  
  train_x <- trainset %>% select(1:2)
  train_y <- trainset$y
  
  test_x <- testset %>% select(1:2) 
  
  #fitting Random Forest
  rf <- randomForest(train_x, train_y)
  
  #predictions, and calculating the MAE
  predictedValue <- predict(rf, test_x)
  trueValue <- testset$y
  predictionErrorTerms <- trueValue - predictedValue
  
  return(predictionErrorTerms)
}


#### FUNCTIONS FOR EVALUATING RESULTS #####

mae <- function(truevalue, predicted){
  #MAE calculates the mean average error of prediction
  error <- truevalue - predicted
  return(mean(abs(error)))
}


averageError <- function(N, dataSet, smp_size){
  #AVERAGEERROR calculates the error terms of the predictions
  #given by OLS and RandomForest when applied to "dataSet"
  #Return value: averaged error terms over 'N' simulations
  
  trainset_withouterror <- dataSet[0: smp_size,]
  testset_withouterror <- dataSet[(smp_size+1):nrow(dataSet),]
  
  sizeOfTest <- nrow(dataSet) - smp_size
  
  errorLinearModel <- data.frame("V1" = 1:sizeOfTest)
  errorRandomForest <- data.frame("V1" = 1:sizeOfTest)
  
  #Running the simulation N times with different error terms
  for (i in 1:N) {
    errors <- rnorm(nrow(dataSet), mean = 0)*parameters$DGP$noiseSTD
    
    #update y with error terms
    trainset <- trainset_withouterror
    trainset$y <- trainset$y + errors[0:smp_size]
    
    testset <- testset_withouterror
    testset$y <- testset$y +errors[(smp_size+1):length(errors)]
    
    
    errorLinearModel[[i]] <- ols_calculator(trainset, testset)
    errorRandomForest[[i]] <- rf_calculator(trainset, testset)
  }
  
  #calculating average error for each target value
  averageErrorLinearModel <- apply(errorLinearModel, 1, mean)
  averageErrorRandomForest <- apply(errorRandomForest, 1, mean) 
  
  averageError <- list('LinearModel' = averageErrorLinearModel, 'RandomForest' = averageErrorRandomForest )
  
  return(averageError)
}




