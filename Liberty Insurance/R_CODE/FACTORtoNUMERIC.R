### Creates a set of new columns for your data set that summarize factor levels by descriptive statistics such as mean,
### sd, skewness etc.

### Variables : train = training set
###             test = test set
###             response = response variable (Hazard in this competition)
###             variables = vector of column names you wise to summarize (T1_V4 for example). Must be strings.
###             metrics = vector of desctriptive statistics you wish to compute for each factor level. Must Be Strings.

### ex: factorToNumeric(train, test, "Hazard", "T1_V4", "mean") will return a column of hazard means by factor level
###     in column T1_V4

### You can specify the test parameter as train to get a column to add to your training set

###### REQUIRES package qdapTools  #####
library(qdapTools)



factorToNumeric <- function(train, test, response, variables, metrics){
  temp <- data.frame(c(rep(0,nrow(test))), row.names = NULL)
  
  for (variable in variables){
    for (metric in metrics) {
      x <- tapply(train[, response], train[,variable], metric)
      x <- data.frame(row.names(x),x, row.names = NULL)
      temp <- data.frame(temp,round(lookup(test[,variable], x),2))
      colnames(temp)[ncol(temp)] <- paste(metric,variable, sep = "_")
    }
  }
  return (temp[,-1])
}



#### Sample Usage #### 
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")

#### Returns mean, median, and sd for factor T1_V4 by factor level to be added to the training set.
data.frame(head(train$T1_V4))
head(factorToNumeric(train, train, "Hazard", "T1_V4", c("mean","median","sd")))

#### Returns mean, median, and sd for factor T1_V4 by factor level to be added to the test set.
data.frame(head(test$T1_V4))
head(factorToNumeric(train, test, "Hazard", "T1_V4", c("mean","median","sd")))