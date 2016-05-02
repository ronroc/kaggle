require(beepr)
require(readr)
require(Matrix)
require(caret)

train <- read_csv("~/Kaggle/Liberty Insurance/DATA/CSV/train.csv")

test <- read_csv("~/Kaggle/Liberty Insurance/DATA/CSV/test.csv")

# keep copy of ID variables for test and train data

train_Id <- train$Id

test_Id <- test$Id

# response variable from training data

train_y <- train$Hazard

# predictor variables from training

train_x <- subset(train, select = -Id)

train_sparse <- sparse.model.matrix(~., data = train_x)

# predictor variables from test

test_x <- subset(test, select = -Id)

test_sparse <- sparse.model.matrix(~., data = test_x)



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



pred_xgb <- predict(xgbTree_caret_first_3, newdata=test_sparse)

#######################################################################################

#rf Model

train <- read_csv("~/Kaggle/Liberty Insurance/DATA/CSV/train.csv")

test <- read_csv("~/Kaggle/Liberty Insurance/DATA/CSV/test.csv")

train_x <- subset(train, select = -c(Id))

test_x <- subset(test, select = -c(Id))

grid_rf <- expand.grid(mtry = c(10, 12, 14))

trainControlrf <- trainControl(method = "repeatedcv", number = 3,
                               
                               repeats = 1,summaryFunction = NormalizedGini ,
                               
                               verboseIter = T
)


caret_rf <- train(Hazard ~., data = train_x,
                  
                  method = "rf" ,
                  
                  trControl = trainControlrf,
                  
                  metric = "Gini" ,
                  
                  savePredictions=TRUE,
                  
                  tuneGrid = grid_rf
                  
)

save(rf_caret, file = "rf_caret.RData")


# submission predict

pred_rf <- predict(rf_caret, newdata=test_x)

####################################################################################

#gbm 

gbmGrid <-  expand.grid(interaction.depth =  c(9, 15, 20),
                        n.trees = c(100,200,500),
                        shrinkage =  c(0.2, 0.01, 0.05),
                        n.minobsinnode = c(200, 400, 500))

gbmGrid_one <-  expand.grid(interaction.depth =  20,
                        n.trees = 200,
                        shrinkage =  0.05,
                        n.minobsinnode = 200)


trainControlgbm <- trainControl(method = "repeatedcv", number = 3,
                                
                                repeats = 1,summaryFunction = NormalizedGini ,
                                
                                verboseIter = T
)

gbm_caret_best <- train(Hazard ~., data = train_x, method = "gbm" ,trControl = trainControlgbm,
                   
                   metric = "Gini" , tuneGrid = gbmGrid_one 
                   
)

save(gbm_caret_new, file = "gbm_caret_new.RData")

pred_gbm <- predict(gbm_caret_best, test_x)

beep("fanfare")

sub <- data.frame("Id" = test_Id, "Hazard" = pred_gbm)

write.csv(sub, "maxGBM.csv", row.names = FALSE)


#####################################################################################

#blending

###################################################################################

train <- read_csv("~/Kaggle/Liberty Insurance/DATA/CSV/train.csv")

test <- read_csv("~/Kaggle/Liberty Insurance/DATA/CSV/test.csv")


#seperate train into train.data & validation.data

seperate <- createDataPartition(train$Hazard, p = 0.60, list = F)

##########################################################################################################
train.data <- train[seperate,]

validation.data <- train[-seperate, ]

test.data <- test
###########################################################################################################

labelName <- 'Hazard'

predictor <- names(train.data)[names(train.data) != labelName]

control <- trainControl(method = "repeatedcv", number = 3, repeats = 1, verboseIter = T,
                        
                        summaryFunction = NormalizedGini
                        )
###########################################################################################################

#BECHMARK MODEL 

gbm_grid <- expand.grid(interaction.depth =  20,
                        n.trees = 200,
                        shrinkage =  0.05,
                        n.minobsinnode = 200
                          )

benchmark <- train(validation.data[, labelName], validation.data[, predictor], 
                    
                    method = 'gbm', trControl = control,  metric = "Gini", tuneGrid = gbm_grid 
                    
                     )

pred_gbm <- predict(benchmark, test_x ) 

#LEADERBOARD SCORE :0.372604

############################################################################################################
#trying xgboost, rf, gbm
 
#RF

 
grid_rf <- expand.grid(mtry = 14)
 
 
caret_rf <- train(train.data[,predictor], train.data[,labelName],
                   
                   method = "rf" ,
                   
                   trControl = control,
                   
                   metric = "Gini" ,
                   
                   savePredictions=TRUE,
                   
                   tuneGrid = grid_rf
                   
                  )
##########################################################################################################
#GBM
 
 grid_gbm <- expand.grid(interaction.depth =  20,
                         n.trees = 200,
                         shrinkage =  0.05,
                         n.minobsinnode = 200
 )
 
 caret_gbm <- train(train.data[,predictor], train.data[,labelName],
                   
                   method = "gbm" ,
                   
                   trControl = control,
                   
                   metric = "Gini" ,
                   
                   savePredictions=TRUE,
                   
                   tuneGrid = gbm_grid
                   
 )

############################################################################################################

 #xgboost
 
 grid_xgb <- 
 