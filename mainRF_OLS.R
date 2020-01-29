library(dplyr)
library(tidyr)
library(randomForest)
library(randomForest)
library(forecast)

source("dataParameters.R")
source("functions_RF_OLS.R")


parameters <- set_parameters()

smp_size <- floor(parameters$model$trainratio * nrow(parameters$DGP$generatedData))

average <- averageError(parameters$NumberOfSimulations, parameters$DGP$generatedData, smp_size)

#running a Diebold-Mariano Test
dm.test(average$LinearModel, average$RandomForest, 
        alternative = "two.sided", h = 1, power = 1)

