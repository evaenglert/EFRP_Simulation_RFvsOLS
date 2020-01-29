library(dplyr)
library(tidyr)
library(randomForest)

set_parameters <- function() {
  #SET_PARAMETERS sets the parameters of the simulation
  
  parameters <- list()
  
  parameters$NumberOfSimulations <- 100 #how many times should the simulation run 
  
  parameters$DGP$coefficient <- c(0.5, 0, -0.3) # y = 0.5x1 - 0.3 x1x2 + error
  parameters$DGP$xmin <- -5
  parameters$DGP$xmax <- 5
  parameters$DGP$samplesize <- 30
  
  #dividing up interval [xmin, xmax] into subintervals of equal length
  x1 <- seq(parameters$DGP$xmin, parameters$DGP$xmax, length.out = parameters$DGP$samplesize) 
  x2 <- seq(parameters$DGP$xmin, parameters$DGP$xmax, length.out = parameters$DGP$samplesize)

  
  parameters$DGP$generatedData <- expand.grid( x1 = x1, x2 = x2)
  parameters$DGP$generatedData$x1x2 <- parameters$DGP$generatedData$x1 * parameters$DGP$generatedData$x2
  covarMatrix <- data.matrix(parameters$DGP$generatedData) #converting dataframe to matrix
  
  #calculating the targetvalues without error
  parameters$DGP$generatedData$y <- covarMatrix %*% parameters$DGP$coefficient
  
  
  parameters$DGP$noiseSTD <- 5
  parameters$DGP$rnormstart <- 20 #setting starting point so that simulation is reproducible
  
  parameters$model$trainratio = 0.7 # ratio of trainset to dataset
  
  
  return(parameters)
  
}


