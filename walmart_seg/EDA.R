require(xgboost); require(readr); require(caret); require(doParallel)

train <- read_csv("D:/kaggle/walmart_seg/train.csv")

test <- read_csv("D:/kaggle/walmart_seg/test.csv")

str(train)

length(unique(train$VisitNumber)) #95674

length(unique(test$VisitNumber)) #95674

#check whether visit number from both test and train are same

length(setdiff((unique(train$VisitNumber)), (unique(test$VisitNumber)))) #95674

