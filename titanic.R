setwd("~/Documents/Titanic")

titanic.train <- read.csv(file= "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file= "test.csv", stringsAsFactors = FALSE, header = TRUE)

titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

titanic.test$Survived <- NA

titanic.full <- rbind(titanic.train, titanic.test)

