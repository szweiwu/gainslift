# Load needed libraries
library(readr)
library(rpart)
library(randomForest)
library(e1071)

##### First dataset #####
# Import the bank dataset
bank.df <- read_csv("./data/bank.csv")

# Turn categorical columns into factors
bank_col_names <- c('job', 'marital', 'education', 'default', 'housing',
               'loan', 'contact', 'month', 'DOW', 'pdays', 'poutcome','y')
bank.df[bank_col_names] <- lapply(bank.df[bank_col_names] , factor)

# Partition Data into training(70%) and test(30%) set
trainrows <- sample(rownames(bank.df), dim(bank.df)[1]*0.7)
bank.train <- bank.df[trainrows, ]
testrows <- sample(setdiff(rownames(bank.df), trainrows), dim(bank.df)[1]*0.3)
bank.test <- bank.df[testrows, ]

# Run prediction models
## Run Classification Tree
bank.rp <- rpart(y ~., data = bank.train)
bank.rp.pred <- predict(bank.rp, bank.test, type = "prob")

## Run Random Forest
bank.rf <- randomForest(y ~., data = bank.train)
bank.rf.pred <- predict(bank.rf, bank.test, type = "prob")

## Run Naive Bayes
bank.nb <- naiveBayes(y ~., data = bank.train)
bank.nb.pred <- predict(bank.nb, bank.test, type = "raw")


##### Second dataset #####
# Import the credit dataset
credit.df <- read_csv("./data/credit.csv")

# Turn categorical columns into factors
credit.df$y <- as.factor(credit.df$y)

# Partition Data into training(70%) and test(30%) set
trainrows <- sample(rownames(credit.df), dim(credit.df)[1]*0.7)
credit.train <- credit.df[trainrows, ]
testrows <- sample(setdiff(rownames(credit.df), trainrows), dim(credit.df)[1]*0.3)
credit.test <- credit.df[testrows, ]

# Run prediction models
## Run Classification Tree
credit.rp <- rpart(y ~., data = credit.train)
credit.rp.pred <- predict(credit.rp, credit.test, type = "prob")

## Run Random Forest
credit.rf <- randomForest(y ~., data = credit.train)
credit.rf.pred <- predict(credit.rf, credit.test, type = "prob")

## Run Naive Bayes
credit.nb <- naiveBayes(y ~., data = credit.train)
credit.nb.pred <- predict(credit.nb, credit.test, type = "raw")
