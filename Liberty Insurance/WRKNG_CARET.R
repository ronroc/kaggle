#####################################################################################

#blending

###################################################################################
require(Matrix)
require(readr)
require(caret)

train <- read_csv("~/Kaggle/Liberty Insurance/DATA/CSV/train.csv")

test <- read_csv("~/Kaggle/Liberty Insurance/DATA/CSV/test.csv")


train[sapply(train, is.character)] <- lapply(sapply(train, is.character), as.factor)

test[sapply(test, is.character)] <- lapply(sapply(test, is.character), as.factor)

#seperate train into train.data & validation.data

split <- floor(nrow(train)/3)

##########################################################################################################
train.data <- train[0:split, ]

validation.data <- train[(split+1):(split*2), ]

test.data <- train[(split*2+1):nrow(train),]
###########################################################################################################

labelName <- 'Hazard'

predictor <- names(train.data)[names(train.data) != labelName]

control <- trainControl(method = "repeatedcv", number = 10, repeats = 1, verboseIter = T,
                        
                        summaryFunction = NormalizedGini
)

###########################################################################################################

#BECHMARK MODEL 

gbm_grid <- expand.grid(interaction.depth =  20,
                        n.trees = 200,
                        shrinkage =  0.05,
                        n.minobsinnode = 200
)

benchmark <- train(validation.data[,predictor], validation.data[,labelName], 
                   
                   method = 'gbm', trControl = control,  metric = "Gini", tuneGrid = gbm_grid 
                   
)

pred_gbm <- predict(benchmark, test.data ) 

#LEADERBOARD SCORE :0.372604

############################################################################################################
#trying xgboost, rf, gbm

#RF


grid_rf <- expand.grid(mtry = 14)


caret_rf <- train(train.data[,predictor], train.data[,labelName],
                  
                  method = "rf" ,
                  
                  trControl = control,
                  
                  metric = "Gini" ,
                  
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
                   
                   tuneGrid = grid_gbm
                   
)

############################################################################################################

#xgboost

train <- read_csv("~/Kaggle/Liberty Insurance/DATA/CSV/train.csv")

test <- read_csv("~/Kaggle/Liberty Insurance/DATA/CSV/test.csv")


train_x <- subset(train, select = -c(Id,Hazard))

train_x <- sparse.model.matrix(~., data = train_x)

test_x <- subset(test, select = -c(Id))

test_x <- sparse.model.matrix(~., data = test_x)

test_Id <- test$Id

train_y <- train$Hazard

split <- floor(nrow(train_x)/3)

##########################################################################################################
train.data <- train_x[0:split, ]

validation.data <- train_x[(split+1):(split*2), ]

test.data <- train_x[(split*2+1):nrow(train_x),]
###########################################################################################################
train.Hazard <- train_y[0:split]

validation.Hazard <- train_y[(split+1):(split*2)]

test.Hazard <- train_y[(split*2+1):nrow(train_y)]


control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = T,
                        
                        summaryFunction = NormalizedGini
)


grid_xgb <- expand.grid(eta = 0.035,
                        
                        max_depth = 5, 
                        
                        nrounds = 400)


caret_xgb <- train(train.data, train.Hazard,
                   
                   method = "xgbTree" , trControl = control,
                   
                   metric = "Gini" ,
                   
                   tuneGrid = grid_xgb,
                   
                   savePredictions=TRUE, 
                   
                   "min_child_weight" = 20,
                   
                   "subsample" = .7,
                   
                   "colsample_bytree" = .8,
                   
                   "scale_pos_weight" = 1.5
                   
)

###########################################################################################################
grid_xgb_1 <- expand.grid(eta = 0.05,
                        
                        max_depth = 5, 
                        
                        nrounds = 400)


caret_xgb_1 <- train(train.data, train.Hazard,
                   
                   method = "xgbTree" , trControl = control,
                   
                   metric = "Gini" ,
                   
                   tuneGrid = grid_xgb_1,
                   
                   savePredictions=TRUE, 
                   
                   "min_child_weight" = 20,
                   
                   "subsample" = .7,
                   
                   "colsample_bytree" = .8,
                   
                   "scale_pos_weight" = 1.6
                   
)

#############################################################################################################
grid_xgb_2 <- expand.grid(eta = 0.04,
                          
                          max_depth = 5, 
                          
                          nrounds = 400)


caret_xgb_2 <- train(train.data, train.Hazard,
                     
                     method = "xgbTree" , trControl = control,
                     
                     metric = "Gini" ,
                     
                     tuneGrid = grid_xgb_2,
                     
                     savePredictions=TRUE, 
                     
                     "min_child_weight" = 20,
                     
                     "subsample" = .8,
                     
                     "colsample_bytree" = .8,
                     
                     "scale_pos_weight" = 1.7
                     
)

############################################################################################################

validation.data$rf <- predict(caret_rf, validation.data[,predictor])

validation.data$gbm <- predict(caret_gbm, validation.data[,predictor])

validation.data$xgb <- predict(caret_xgb, validation.data[,predictor])



test.data$rf <- predict(caret_rf, test.data[,predictor])

test.data$gbm <- predict(caret_gbm, test.data[,predictor])

test.data$xgb <- predict(caret_xgb, test.data[,predictor])

#########################################################################################################

xgb_1 <- predict(caret_xgb, validation.data)

xgb_2 <- predict(caret_xgb_1, validation.data)

xgb_3 <- predict(caret_xgb_2, validation.data)



txgb_1 <- predict(caret_xgb, test.data)

txgb_2 <- predict(caret_xgb_1, test.data)

txgb_3 <- predict(caret_xgb_2, test.data)

##########################################################################################################

#FINAL BLENDING MODEL

predictor <- names(validation.data)[names(validation.data) != labelName]

final_blender_model <- train(validation.data[,predictor], validation.data[,labelName], method="xgbTree", trControl=control)

#########################################################################################################

split <- floor(nrow(train)/3)

Id = train$Id[(split*2+1):nrow(train)]

control_1 <- trainControl(method = "repeatedcv", number = 10, repeats = 5, verboseIter = T,
                        
                        summaryFunction = NormalizedGini
)

final_blender_model <- train(validation.data, validation.Hazard, method="xgbTree", trControl=control_1, 
                             
                             tuneGrid = grid_xgb, savePredictions=TRUE, metric = "Gini" , 
                             
                             "min_child_weight" = 20, "subsample" = .7,
                             
                             "colsample_bytree" = .8, "scale_pos_weight" = 1.5
)

###########################################################################################################

pred <- predict(final_blender_model, test.data)

beep("fanfare")

sub <- data.frame("Id" = Id, "Hazard" = pred)

write.csv(sub, "blend_s.csv", row.names = FALSE)

###########################################################################################################

labelName <- 'Hazard'

predictor <- names(test)[names(test) != labelName]

test$rf <- predict(caret_rf, test[,predictor])

test$gbm <- predict(caret_gbm, test[,predictor])

test$xgb <- predict(caret_xgb, test[,predictor])




test$xgb_1 <- predict(caret_xgb, test)

test$xgb_2 <- predict(caret_xgb_1, test)

test$xgb_3 <- predict(caret_xgb_2, test)

###########################################################################################################

predictor <- names(test)[names(test) != labelName]

preds <- predict(final_blender_model, test[,predictor])

subs <- data.frame("Id" = test$Id, "Hazard" = preds)

write.csv(subs, "blend.csv", row.names = FALSE)




preds <- predict(final_blender_model, test_x)

subs <- data.frame("Id" = test_Id, "Hazard" = preds)

write.csv(subs, "blend.csv", row.names = FALSE)
