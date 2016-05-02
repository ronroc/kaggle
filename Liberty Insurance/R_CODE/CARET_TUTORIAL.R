require(readr)
require(Matrix)
require(caret)
require(caretEnsemble)

print("Building xgbTree caret model")

# read in data

train <- read_csv("D:/Kaggle/Liberty Insurance/DATA/CSV/train.csv")

test <- read_csv("D:/Kaggle/Liberty Insurance/DATA/CSV/test.csv")

# keep copy of ID variables for test and train data

train_Id <- train$Id

test_Id <- test$Id

# response variable from training data

train_y <- train$Hazard

# predictor variables from training

train_x <- subset(train, select = -c(Id, Hazard))

train_x <- sparse.model.matrix(~., data = train_x)

# predictor variables from test

test_x <- subset(test, select = -c(Id))

test_x <- sparse.model.matrix(~., data = test_x)


#xgtrain = xgb.DMatrix(data = sparse.model.matrix(~., data = predictor),label = response)

#xgtrain <- xgb.DMatrix(data = train_xx , label= train_y)

#xgval = xgb.DMatrix(data = sparse.model.matrix(~., data = valPredictor),label = valResponse)


#CODE ADJUSTMENTS TO RUN IN PARALLEL ,CARET

NormalizedGini <- function(data, lev = NULL, model = NULL) {
  
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
  
  solution=data$obs
  
  submission=data$pred
  
  result=SumModelGini(solution, submission) / SumModelGini(solution, solution)
  
  names(result) <- "Gini"
  
  result
}

grid <- expand.grid(eta = 0.035,
                    
                    max_depth = 5, 
                    
                    nrounds = 400)

trainControlxgbTree <- trainControl(method = "repeatedcv", number = 5,
                                    
                                    repeats = 3, summaryFunction = NormalizedGini ,
                                    
                                    verboseIter = T
)
  
xgbTree_caret_max <- train(x = train_x,
                       
                       y = train_y,
                       
                       method = "xgbTree" ,
                       
                        trControl = trainControlxgbTree,
                       
                        metric = "Gini" ,
                       
                       tuneGrid = grid,
                       
                        savePredictions=TRUE, 
                       
                       "min_child_weight" = 20,
                       
                       "subsample" = .7,
                       
                       "colsample_bytree" = .8,
                       
                       "scale_pos_weight" = 1.5
                       
)
  

#TEST THE SIMPLEST METHOD TO SAVE TRAINING

save(xgbTree_caret_first_5, file = "xgbTree_caret.RData15072015_1")


#CODE TO SAVE THE MODEL GENERATED

# Copy all model structure info from existing model type
tune_second <- xgbTree_caret_first$results

cust.mdl <- getModelInfo("xgbTree", regex=FALSE)[[1]]

# Override fit function so that we can save the iteration

cust.mdl$fit <- function(x=x, y=y, wts=wts, param=param, lev=lev, last=last, classProbs=classProbs, ...) {

    # Dont save the final pass (dont train the final model across the entire training set)
  
  if(last == TRUE) return(NULL) 
  
  # Fit the model
  
  fit.obj <- getModelInfo("xgbTree", regex=FALSE)[[1]]$fit(x, y, wts, param, lev, last, classProbs, ...)
  
  # Create an object with data to save and save it
  
  fit.data <- list(resample=rownames(x),
                   mdl=fit.obj,
                   #x, y, wts,
                   param=param, lev=lev, last=last, classProbs=classProbs, 
                   other=list(...))
  
  # Create a string representing the tuning params
  
  param.str <- paste(lapply(1:ncol(param), function(x) {
  
      paste0(names(param)[x], param[1,x])
  }), collapse="-")
  
  save(fit.data, file=paste0("rf_modeliter_", sample(1000:9999,1), "_", param.str, ".RData"))

  
  return (fit.obj)
}

# submission predict

pred <- predict(xgbTree_caret_first_5, newdata=test_x)

sub <- data.frame("Id" = test_Id, "Hazard" = pred)

write.csv(sub, "tenth.csv", row.names = FALSE)

----------------------------------------------------------------------------------
  #rf model
  
require(readr)
require(Matrix)
require(caret)
require(caretEnsemble)

print(" Building rf caret model")

# read in data

train <- read_csv("~/Kaggle/Liberty Insurance/DATA/CSV/train.csv")

test <- read_csv("~/Kaggle/Liberty Insurance/DATA/CSV/test.csv")

# keep copy of ID variables for test and train data

train_Id <- train$Id

test_Id <- test$Id

# response variable from training data

train_y <- train$Hazard

# predictor variables from training

train_x <- subset(train, select = -c(Id, Hazard))

# predictor variables from test

test_x <- subset(test, select = -c(Id))

#CODE ADJUSTMENTS TO RUN IN PARALLEL ,CARET

NormalizedGini <- function(data, lev = NULL, model = NULL) {
  
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
  
  solution=data$obs
  
  submission=data$pred
  
  result=SumModelGini(solution, submission) / SumModelGini(solution, solution)
  
  names(result) <- "Gini"
  
  result
}

grid <- expand.grid(mtry = c(10, 12, 14))

trainControlrf <- trainControl(method = "repeatedcv", number = 3,
                                    
                                    repeats = 1,summaryFunction = NormalizedGini ,
                                    
                                    verboseIter = T
)

rf_caret <- train(Hazard ~., data = train,
                               
                               method = "rf" ,
                               
                               trControl = trainControlrf,
                               
                               metric = "Gini" ,
                               
                               
                               
                               savePredictions=TRUE
                               
)

rf_caret

plot(rf_caret)

plot(varImp(rf_caret))


#TEST THE SIMPLEST METHOD TO SAVE TRAINING

save(rf_caret, file = "rf_caret.RData")


# submission predict

pred <- predict(rf_caret, newdata=test_x)

sub <- data.frame("Id" = test_Id, "Hazard" = pred)

write.csv(sub, "rf.csv", row.names = FALSE)


#####################################################################################

#script for tuning a fr and gbm model in H20

require(readr)

require(caret)


#script for tuning a fr and gbm model in CARET

train <- read_csv("~/Kaggle/Liberty Insurance/DATA/CSV/train.csv")

test <- read_csv("~/Kaggle/Liberty Insurance/DATA/CSV/test.csv")

gbmGrid <-  expand.grid(interaction.depth =  9,
                        n.trees = 1000,
                        shrinkage =  0.2,
                        
                        n.minobsinnode = c(500, 800, 1000))
                        
trainControlgbm <- trainControl(method = "repeatedcv", number = 5,
                                                        
                                repeats = 1,summaryFunction = NormalizedGini ,
                                                        
                                verboseIter = T
)
                        
gbm_caret <- train(Hazard ~., data = train,method = "gbm" ,trControl = trainControlgbm,
                                           
                    metric = "Gini" , tuneGrid = gbmGrid 
                                      
)



  
                             
                              
                             