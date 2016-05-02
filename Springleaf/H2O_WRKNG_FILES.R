require("h2o") ;require("h2oEnsemble"); require("SuperLearner"); require("cvAUC")

library(readr); library(xgboost); library(doParallel); library(caret); library(Metrics)

rm(list = ls())

train <- read_csv("D:/kaggle/Springleaf/DATA/modify/training.csv")

test <- read_csv("D:/kaggle/Springleaf/DATA/modify/testing.csv")

set.seed(8675309)

train_raw <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

# save ID and response

train_target <- train_raw$target

train_ID <- train_raw$ID

train$VAR_0241 <- as.numeric(as.character(train$VAR_0241))

test_raw <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")

# save ID and response

test_ID <- test_raw$ID

test$VAR_0241 <- as.numeric(as.character(test$VAR_0241))

print(dim(train)); print(dim(test))

rm(train_raw); rm(test_raw)

gc()

localH2O <- h2o.init(max_mem_size = "10g")

train_target <- as.factor(train_target)

train$ID <- train_ID; train$target <- train_target

test$ID <- test_ID

feature.names <- names(train[1:(ncol(train) -2) ])

train.hex <- as.h2o(localH2O, object = train)

test.hex <- as.h2o(localH2O, object = test)

split <- h2o.runif(train.hex, seed = 1234)

training_frame <- h2o.assign(train.hex[split<0.9,], "training_frame")

validation_frame <- h2o.assign(train.hex[split>=0.9,], "validation_frame")

###############################################################################


#rf

start <- Sys.time()

test_rf <- h2o.randomForest(x = feature.names,
                            
                            y = "target",
                            
                            training_frame = training_frame, 
                            
                            validation_frame = validation_frame,
                            
                            model_id = "rf_09292015", 
                            
                            ntrees = 2000, 
                            
                            max_depth = 10, 
                            
                            binomial_double_trees = T, 
                            
                            balance_classes = T, 
                            
                            seed = 8675309 
                            
)

rf_time <- Sys.time() - start #17.68586 mins #Total - 165.xx mins

######################################################################################################

test_rf@model$training_metrics@metrics$AUC

test_rf@model$validation_metrics@metrics$AUC

h2o.performance(model = test_rf, validation_frame)

pred_rf <- h2o.predict(object = test_rf, newdata = test.hex)

pred_rf <- as.data.frame(pred_rf)

submission <- data.frame(ID = test_ID)

submission$target <- pred_rf$p1

write_csv(submission, "D:/kaggle/Springleaf/SUBMISSION/rf_09292015.csv")


h2o.saveModel(object = test_rf, path = "D:/kaggle/Springleaf/rf_09272015")


##PLOT METHOD TO FIND SEE HOW AUC VARIES OVER DIFFERENT TUNING PARAMETERS

test_rf@model$scoring_history$number_of_trees

test_rf@model$scoring_history$training_AUC

test_rf@model$scoring_history$validation_AUC

#Didn`t the graph much

plot(test_rf@model$training_metrics, type = "roc", 
     
     col = "black", typ = "b")

par(new = T)

plot(test_rf@model$validation_metrics, type = "roc", 
     
     col = "red", typ = "b", axes = FALSE)

#######################################################################

#Might use a interactive graph to make it more easier to interpret


plot(x = test_rf@model$scoring_history$number_of_trees,
     
     y = test_rf@model$scoring_history$training_AUC, col = "red",
     
     typ = "b", xlab = "Number of Trees", ylab = "AUC"
)

par(new = T)


plot(x = test_rf@model$scoring_history$number_of_trees,
     
     y = test_rf@model$scoring_history$validation_AUC, col = "black",
     
     typ = "b", axes = F, xlab = "Number of Trees", ylab = "AUC"
)



##############################################################################

#GLM

start <- Sys.time()

test_glm <- h2o.glm( x = feature.names,
                     
                     y = "target",
                     
                     training_frame = training_frame,
                     
                     validation_frame = validation_frame,
                     
                     family = "binomial",
                     
                     lambda_search = TRUE,
                     
                     nlambdas = 10, 
                     
                     model_id = "glm_test", 
                     
                     solver = "L_BFGS",
                     
                     keep_cross_validation_predictions = T,
                     
                     alpha = c(0, 0.1, 0.2, 0.3, 0.4, 0.6, 0.8, 1), 
                     
                     link = "logit", 
                     
                     standardize = T 
)

glm_time <- Sys.time() - start #5.086208 mins


h2o.saveModel(object = test_glm, path = "D:\\kaggle\\Springleaf\\glm_09272015")

test_glm@model$training_metrics@metrics$AUC

test_glm@model$validation_metrics@metrics$AUC

#code working

#add validation dataset to check during training 

#PREDICTIONS ON TEST DATASET

#SAVE THE PREDICTION VECTOR IN A CSV FILE(09272015_glm)

##############################################################################


########################################################################################################

#GBM

start <- Sys.time()

test_gbm <- h2o.gbm(x = feature.names,
                    
                    y = "target",
                    
                    training_frame = training_frame,
                    
                    validation_frame = validation_frame,
                    
                    model_id = "gbm_09272015", 
                    
                    ntrees =  100, 
                    
                    max_depth = 20, 
                    
                    learn_rate = 0.014, 
                    
                    seed = 8675309, 
                    
                    balance_classes = T, 
                    
                    min_rows = 9 
)


gbm_time <- Sys.time() - start #1.496173 hours

h2o.saveModel(object = test_gbm, path = "D://kaggle//Springleaf//gbm_09282015")

#code working


#add validation dataset to check during training 

test_gbm@model$training_metrics@metrics$AUC #Now showing AUC on train

test_gbm@model$scoring_history$number_of_trees

test_gbm@model$scoring_history$training_AUC

test_gbm@model$scoring_history$validation_AUC



########################################################################################################


#####################################################################################################

test_gbm@model$training_metrics@metrics$AUC

h2o.performance(model = test_gbm, validation_frame)

pred_gbm <- h2o.predict(object = test_gbm, newdata = test.hex)

submission <- data.frame(ID = test_ID)

submission$target <- pred_gbm

write_csv(submission, "D:/kaggle/Springleaf/SUBMISSION/gbm_09272015.csv")

#####################################################################################################

test_glm@model$training_metrics@metrics$AUC

h2o.performance(model = test_glm, validation_frame)

pred_glm <- h2o.predict(object = test_glm, newdata = test.hex)

pred_glm <- as.data.frame(pred_glm)

submission <- data.frame(ID = test_ID)

submission$target <- pred_glm$p1

write_csv(submission, "D:/kaggle/Springleaf/SUBMISSION/glm_09272015.csv")


###################################################################################################