setwd("~/Documents/Titanic")

titanic.train <- read.csv(file= "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file= "test.csv", stringsAsFactors = FALSE, header = TRUE)

titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

titanic.test$Survived <- NA

titanic.full <- rbind(titanic.train, titanic.test)

#clean missing values of embarked
titanic.full[titanic.full$Embarked== '',"Embarked"] <- 'S'

#clean missing values of age
upperage.whisker <- boxplot.stats(titanic.full$Age)$stats[5]
outlierage.filter <- titanic.full$Age<upperage.whisker
titanic.full[outlierage.filter,]
age.equation = "Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked"
age.model <- lm(
  formula = age.equation,
  data = titanic.full[outlierage.filter,]
)

age.row <- titanic.full[
  is.na(titanic.full$Age),
  c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Embarked")
  ]

age.predictions <- predict(age.model, newdata = age.row )
titanic.full[is.na(titanic.full$Age), "Age"] <- age.predictions


#clean missing values of fare
upper.whisker <- boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter <- titanic.full$Fare<upper.whisker
titanic.full[outlier.filter,]
fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model <- lm(
  formula = fare.equation,
  data = titanic.full[outlier.filter,]
)

fare.row <- titanic.full[
  is.na(titanic.full$Fare),
  c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")
]

fare.predictions <- predict(fare.model, newdata = fare.row )
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.predictions


#categorical casting
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

#split dataset back out into train and set
titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE,]

titanic.train$Survived <- as.factor(titanic.train$Survived)

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)
#install.packages("randomForest")
library(randomForest)

titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree= 500, mtry = 3, nodesize = 0.01 * nrow(titanic.train))

features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanic.model, newdata = titanic.test)

PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv(output.df, file = "kaggle_submission_2.csv", row.names = FALSE)
