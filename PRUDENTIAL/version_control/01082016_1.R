library(readr)
library(xgboost) 
library(Hmisc)
library(caret)
library("Metrics"); require(data.table)


set.seed(01092016) 
rounds = 2000

SQWKfun = function(x = seq(1.5, 7.5, by = 1), pred) {
  preds = pred$predict
  true = pred$Response
  cuts = c(min(preds), x[1], x[2], x[3], x[4], x[5], x[6], x[7], max(preds))
  preds = as.numeric(Hmisc::cut2(preds, cuts))
  err = Metrics::ScoreQuadraticWeightedKappa(preds, true, 1, 8)
  return(-err)
}

train <- fread("D:\\kaggle\\PRUDENTIAL\\Data\\train.csv", data.table = F)

train[is.na(train)] <- -1



test  <- fread("D:\\kaggle\\PRUDENTIAL\\Data\\test.csv", data.table = F)

test[is.na(test)] <- -1


feature.names <- names(train)[3:ncol(train)-1]
feature.names1 <- feature.names
print(feature.names)
cat("Factor char fields\n")
for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}


  
  trainIndex <- createDataPartition(train$Response, p = .5, list = FALSE, times = 1)
  Ptrain <- train[trainIndex,]
  Pval  <- train[-trainIndex,]
  
  #train$Train_Flag <- 1 #Add in a flag to identify if observations fall in train data, 1 train, 0 test
  #test$Train_Flag <- 0 #Add in a flag to identify if observations fall in train data, 1 train, 0 test
  #test$Response <- NA #Add in a column for Response in the test data and initialize to NA
  
  #All_Data <- rbind(train,test) #79,146 observations, 129 variables 
  
  #preProcValues <- preProcess(All_Data, method = c("center", "scale"))
  #trainTr <- predict(preProcValues, train)
  #testTr <- predict(preProcValues, test)
  #summary(testTr)
  feature.names2 <- c("BMI", "Medical_History_15", "Medical_History_4", "Insurance_History_2") #, "Product_Info_4", "Medical_Keyword_15", "Medical_History_23", "Medical_Keyword_3", "Wt", "Ins_Age", "Medical_History_40", "Medical_History_24", "Family_Hist_4", "InsuredInfo_6", "Medical_History_30", "Medical_History_28", "Family_Hist_2", "Family_Hist_3", "Medical_History_32", "Medical_History_5", "Medical_History_13", "InsuredInfo_5", "Employment_Info_2", "Employment_Info_3", "Family_Hist_1", "Medical_History_39", "Medical_History_18", "Medical_Keyword_38", "Medical_Keyword_23", "Medical_History_20", "InsuredInfo_7", "Insurance_History_2", "Medical_History_33", "Medical_History_1", "Product_Info_1", "Family_Hist_5", "Employment_Info_1", "InsuredInfo_2", "Medical_History_16", "Employment_Info_6", "cvResponse")
  #feature.names1 <- c("BMI", "Insurance_History_2", "InsuredInfo_5", "Insurance_History_1", "InsuredInfo_2", "Insurance_History_8", "Product_Info_7", "Product_Info_5", "Medical_History_4", "Medical_History_23", "Medical_History_39", "Medical_History_40", "Medical_History_28", "Medical_History_30", "Medical_History_5", "Medical_History_18", "Medical_Keyword_15", "Medical_Keyword_3", "Medical_Keyword_23", "Medical_Keyword_38", "Medical_Keyword_48", "Medical_Keyword_42", "Medical_Keyword_34", "Medical_Keyword_35")
  #str(train$Response) 
  # Tuning syntax from http://stats.stackexchange.com/questions/171043/how-to-tune-hyperparameters-of-xgboost-trees
  #fitControl <- trainControl(method = "none", number = 3) # , repeats = 3
  
  #tune_grid = expand.grid(nrounds = c(100), eta = c(0.3), max_depth = c(8))
  
  #gbmGrid <- expand.grid(interaction.depth = 9, n.trees = 356, shrinkage = 0.1, n.minobsinnode=10)
  
  date()
  cat("Fitting model\n")
  #fit <- train(x=data.matrix(train[,feature.names]), y=train$Response, #distribution="multinomial", 
  #            method = "xgbTree", #nTrain = round(nrow(train) *0.8), 
  #            trControl = fitControl,
  #            tuneGrid = tune_grid,
  #            metric = "Kappa", 
  #            verbose = 1)
  
  
  dtrain <- xgb.DMatrix(data.matrix(Ptrain[,feature.names1]), label=Ptrain$Response)
  vtrain <- xgb.DMatrix(data.matrix(train[,feature.names1]), label=train$Response)
  dval <- xgb.DMatrix(data.matrix(Pval[,feature.names1]), label=Pval$Response)
  dtest <- xgb.DMatrix(data.matrix(test[,feature.names1]))
  
  Watchlist <- list(train=dtrain, test=dval)
  
  fit <- xgb.train(data=dtrain, #label=train$Response,  
                   max.depth=6, eta=0.03, nround=rounds, watchlist=Watchlist, #objective = "multi:softmax", num_class = 8,
                   objective = "reg:linear",
                   verbose=1)
  date()       
  summary(fit)
  
  importance_matrix <- xgb.importance(model = fit)
  head(importance_matrix)
  #write_csv(importance_matrix, "Imp_Matrix.csv")
  #xgb.plot.importance(importance_matrix = importance_matrix)
  
  cat("Predict model\n")
  test$cvResponse <- predict(fit, newdata=data.matrix(test[,feature.names1]))
  #test$cvResponse <- round(predict(fit, newdata=dtest))
  
  date()   
  cat("saving the submission file\n")
  #submission <- data.frame(Id=test$Id, Response=test$cvResponse)
  
  #submission[submission$Response<1, "Response"] <- 1
  #submission[submission$Response>8, "Response"] <- 8
  
  
  #write_csv(submission, "Pru_xgbTree_10t.csv")
  
  cat("Validation predict model\n")
  #train$cvResponse <- predict(fit, newdata=data.matrix(train[,feature.names1]))
  train$cvResponse <- predict(fit, newdata=vtrain)
  date()   
  #cat("saving the validation file\n")
  #val <- data.frame(Id=train$Id, Response=train$Response, cvResponse=train$cvResponse )
  
  #write_csv(val, "Pru_xgbTree_10t_val.csv")
  round((table(round(train$cvResponse),train$Response)/nrow(train))*100,1)
  
  ScoreQuadraticWeightedKappa(as.numeric(round(train$cvResponse)),as.numeric(train$Response))
  
  # Now use the regression output and try to classify.... 
  feature.names1 <- c(feature.names1, "cvResponse")
  
  #trainIndex <- createDataPartition(train$Response, p = .5, list = FALSE, times = 1)
  
  Ptrain <- train[-trainIndex,]
  Pval  <- train[trainIndex,]
  dtrain <- xgb.DMatrix(data.matrix(Ptrain[,feature.names1]), label=Ptrain$Response)
  vtrain <- xgb.DMatrix(data.matrix(train[,feature.names1]), label=train$Response)
  dval <- xgb.DMatrix(data.matrix(Pval[,feature.names1]), label=Pval$Response)
  dtest <- xgb.DMatrix(data.matrix(test[,feature.names1]))
  
  Watchlist <- list(train=dtrain, test=dval)
  
  fit <- xgb.train(data=dtrain, #label=train$Response,  
                   max.depth=7, eta=0.03, nround=rounds,  watchlist=Watchlist, 
                   #objective = "multi:softmax", num_class = 8,
                   objective = "reg:linear",
                   verbose=1)
  importance_matrix <- xgb.importance(model = fit)
  head(importance_matrix)
  #write_csv(importance_matrix, "Imp_Matrix2.csv") #print(importance_matrix)
  
  cat("Predict model\n")
  #test$Response <- round(predict(fit, newdata=data.matrix(test[,feature.names])))
  test$cvResponse1 <- predict(fit, newdata=dtest)
  train$cvResponse1 <- predict(fit, newdata=vtrain)
  
  pred = data.frame(Id=train$Id, Response=train$Response, predict=train$cvResponse1)
  optCuts = optim(seq(1.5, 7.5, by = 1), SQWKfun, pred = pred)
  print(optCuts)
  preds = as.numeric(Hmisc::cut2(test$cvResponse1, c(-Inf, optCuts$par, Inf)))
  
  date()   
  cat("saving the submission file\n")
  submission <- data.frame(Id=test$Id, Response=preds)
  
  #submission[submission$Response<1, "Response"] <- 1
  #submission[submission$Response>8, "Response"] <- 8
  
  
  write_csv(submission, "01092016_2.csv")
  
  