rm(list = ls())

require("h2o") ;require("h2oEnsemble"); require("SuperLearner"); require("cvAUC")

library(readr); library(xgboost); library(doParallel); library(caret); library(Metrics)

set.seed(8675309)

train_raw <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

# save ID and response

train_target <- train_raw$target

train_raw$target <- NULL

train_ID <- train_raw$ID

train_raw$ID <- NULL

train$VAR_0241 <- as.numeric(as.character(train$VAR_0241))

test_raw <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")

# save ID and response

test_ID <- test_raw$ID

test$ID <- NULL

test$VAR_0241 <- as.numeric(as.character(test$VAR_0241))

print(dim(train)); print(dim(test))

datecolumns = c("VAR_0073", "VAR_0075", "VAR_0156", "VAR_0157", "VAR_0158",

                "VAR_0159", "VAR_0166", "VAR_0167", "VAR_0168", "VAR_0176",

                "VAR_0177", "VAR_0178", "VAR_0179", "VAR_0204", "VAR_0217")

train_cropped <- train[datecolumns]

train_cc <- data.frame(apply(
  
  train_cropped, 2, function(x) 
    
    as.double(strptime(x,
                       
                       format='%d%b%y:%H:%M:%S', tz="UTC"))))

#check how resonse varies with change in representation of date (month, year etc also check which day/month/year
# has highest loan buyers i.e. check for seasonality trends)
for (dc in datecolumns){
  
  train[dc] <- NULL
train[dc] <- train_cc[dc]
}
train_cc <- NULL
train_cropped <- NULL
gc()
test_cropped <- test[datecolumns]
test_cc <- data.frame(apply(test_cropped, 2, function(x) as.double(strptime(x,
format='%d%b%y:%H:%M:%S', tz="UTC"))))
for (dc in datecolumns){
test[dc] <- NULL
test[dc] <- test_cc[dc]
}
test_cc <- NULL
test_cropped <- NULL
gc()
feature.names <- names(train)[1:ncol(train)]
for (f in feature.names) {
if (class(train[[f]])=="character") {
levels <- unique(c(train[[f]], test[[f]]))
train[[f]] <- as.integer(factor(train[[f]], levels=levels))
test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
}
}
print(dim(train)); print(dim(test))

#dates seem to be skewed towards right , applying Box Cox transforms to both train and test

train_pre <-  preProcess(x = train[datecolumns], method = ("BoxCox"))

train_pre_pred <- predict(train_pre, train[datecolumns])

train[datecolumns] <- train_pre_pred

test_pre <-  preProcess(test[datecolumns], method = ("BoxCox"))

test_pre_pred <- predict(test_pre, test[datecolumns])

test[datecolumns] <- test_pre_pred

print(dim(train)); print(dim(test))

##########################################################################################

#train_pre <-  preProcess(x = train, method = ("knnImpute"), k = 5)

#gc()

#train_pre_pred <- predict(train_pre, train)

#train <- train_pre_pred

#gc()

#test_pre <-  preProcess(test[datecolumns], method = ("knnImpute"), k = 5)

#gc()

#test_pre_pred <- predict(test_pre, test)

#test <- test_pre_pred

#print(dim(train)); print(dim(test))

#gc()
########################################################################################

#This data set has large number of nzv removing them

nzv <- nearZeroVar(train)

nzv_test <- nearZeroVar(test)

train <- train[, -nzv]

test <- test[, -nzv_test]

train[is.na(train)] <- 0

test[is.na(test)]   <- 0

print(dim(train)); print(dim(test))

write_csv(train, "pp_train.csv")

write_csv(test, "pp_test.csv")

list = ls()

train <- read_csv("D:/kaggle/Springleaf/pp_train.csv")


test <- read_csv("D:/kaggle/Springleaf/pp_test.csv")

#########################################################################################

gc()

localH2O <- h2o.init(max_mem_size = "10g")

train_target <- as.factor(train_target)

train$ID <- train_ID; train$target <- train_target

test$ID <- test_ID

feature.names <- names(train[1:(ncol(train) -2) ])

train.hex <- as.h2o(localH2O, object = train)

test.hex <- as.h2o(localH2O, object = test)

split <- h2o.runif(train.hex, seed = 1234)

training_frame <- h2o.assign(train.hex[split<0.8,], "training_frame")

validation_frame <- h2o.assign(train.hex[split>=0.8,], "validation_frame")

hyper_params = list(ntrees=c(50,100,200),max_depth=c(2,3,4), 
                    
                    learn_rate=c(0.1,0.2))

#Did not save the grid 

gbm_grid <- h2o.grid(algorithm = "gbm", x = feature.names, y = "target", 
         
         , training_frame = training_frame, hyper_params = hyper_params ) 
###############################################################################################

glm_grid <- h2o.glm( x = feature.names,
                        
                     y = "target",
                              
                     training_frame = training_frame,
                     
                     family = "binomial",
                              
                     lambda_search = TRUE,
                              
                     nlambdas = 10, 
                     
                     model_id = "glm_test", 
                     
                     solver = "L_BFGS",
                     
                     keep_cross_validation_predictions = T,
                     
                     alpha = c(0, 0.25, 0.5, 0.75, 1)
                     
                     )

glm_grid@conn

glm_grid@model_id

glm_grid@algorithm

glm_grid@parameters

glm_grid@allparameters

glm_grid@finalizers

pred <- h2o.performance(model = glm_grid, data = validation_frame, valid = T)


#############################################################################################

rf_test <- h2o.randomForest( x = feature.names,
                              
                              y = "target",
                              
                              training_frame = training_frame,
                              
                              model_id =  "rf_09262015",
                              
                              mtries = 500,
                              
                              sample_rate = 0.632,
                              
                              build_tree_one_node = FALSE,
                              
                              ntrees = 50,
                              
                              max_depth = 20,
                              
                              min_rows = 1,
                              
                              nbins = 20,
                              
                              nbins_cats = 1024,
                              
                              binomial_double_trees = FALSE,
                              
                              balance_classes = FALSE,
                              
                              max_after_balance_size = 5,
                              
                              seed = 999,
                              
                              nfolds = 3,
                             
                              fold_assignment = ("Random"),
                              
                              keep_cross_validation_predictions = T
                              
                              )

pred <- h2o.performance(model = rf_test, data = validation_frame, valid = T)

h2o.getModel(model_id = rf_test@model$)

#############################################################################################
rf_grid <- h2o.grid(algorithm = "rf", x = feature.names, y = "target", 
                     
                     hyper_params = list(ntrees=c(50,100,200),max_depth=c(2,3,4),
                                         
                     mtries = c(500, 400), training_frame = training_frame) 


                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper",

             "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")
metalearner <- "h2o.deeplearning.wrapper"

family <- "binomial"

fit <- h2o.ensemble(x = feature.names , y = "target",

                    training_frame = training_frame,

                    family = family,

                    learner = learner,

                    metalearner = metalearner,

                    cvControl = list(V = 5, shuffle =

                                       TRUE))


pred <- predict.h2o.ensemble(fit, test.hex)

predictions <- as.data.frame(pred$pred)[,3]  

labels <- as.data.frame(validation_frame[,c("target")])[,1]

cvAUC::AUC(predictions = predictions , labels = labels)

L <- length(learner)

auc <- sapply(seq(L), function(l) cvAUC::AUC(predictions = 
                                               
                                               as.data.frame(pred$basepred)[,l], labels = labels)) 

data.frame(learner, auc)