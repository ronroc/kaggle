require(caret)

rm(list=ls())

train <- read_csv("~/Kaggle/Liberty Insurance/CSV/train.csv")

test <- read_csv("~/Kaggle/Liberty Insurance/CSV/test.csv")

#drop ID column in train set

train<-data.frame(train)[,-1]

test<-data.frame(test)

#No missing values

#length(train[is.na(train)])

#length(test[is.na(test)])

#Gini code from kaggle

SumModelGini <- function(solution, submission) {
  
  df = data.frame(solution = solution, submission = submission)
  
  df <- df[order(df$submission, decreasing = TRUE),]
  
  df$random = (1:nrow(df))/nrow(df)
  
  totalPos <- sum(df$solution)
  
  df$cumPosFound <- cumsum(df$solution) # this will store the cumulative number of positive examples found (used for computing "Model Lorentz")
  
  df$Lorentz <- df$cumPosFound / totalPos # this will store the cumulative proportion of positive examples found ("Model Lorentz")
  
  df$Gini <- df$Lorentz - df$random # will store Lorentz minus random
  
  return(sum(df$Gini))
}

  
NormalizedGini <- function(data, lev = NULL, model = NULL) {
  solution=data$obs
  
  submission=data$pred
  
  result=SumModelGini(solution, submission) / SumModelGini(solution, solution)
  
  names(result) <- "Gini"
  
  result
}

#simple linear model LB score 0.326353

#lm<- lm(Hazard ~ .,data = train)



cv.ctrl <- trainControl(method = "repeatedcv",number=3, repeats = 1,summaryFunction = NormalizedGini,verboseIter = TRUE)

rf.grid <- expand.grid(.mtry = c(5:9)) 

#boosting tree LB score 0.350240

#rf

set.seed(30)

rf <- train(Hazard ~ ., data=train, method = "rf", metric = "Gini", trControl = cv.ctrl, verbose = TRUE,importance = TRUE
            ,tuneGrid=rf.grid)

rf$results

rf$bestTune

plot(varImp(rf, scale = FALSE))





##submission

response <- predict(rf, test)

submissionFile<-cbind(test$Id,response)

colnames(submissionFile)<-c("Id","Hazard")

options("scipen"=100, "digits"=8)

write.csv(submissionFile,"gbm.csv",row.names=FALSE,quote=FALSE)