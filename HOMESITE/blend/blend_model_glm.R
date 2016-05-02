
# combine bag_xgb, bag_rf, bag_gbm, bag_glm

# use dataset_blend_train to train a second level model-------------------------------------------

train <- cbind(train, data.frame(dataset_blend_train))

test <- cbind(test, data.frame(dataset_blend_test))


# train a second level using glm(log_reg)-------------------------------------------------------------

target <- as.factor(response)

train$target <- target

myX <- names(train)

myY <- "target"

print(paste("training glm iteration :", j, "for Fold ; number :", i))

train.hex <- as.h2o(train)

test_glm <- h2o.glm( x = myX,
                     
                     y = myY,
                     
                     training_frame = train.hex,
                     
                     family = "binomial",
                     
                     lambda_search = TRUE,
                     
                     link = "logit", 
                     
                     standardize = T
                     )

test.hex <- as.h2o(test)

pred_glm <- h2o.predict(object = test_glm, newdata = test.hex)

pred_glm <- as.data.frame(pred_glm)

preds <- pred_glm$p1

## create submission file-----------------------------------------------------------------------

submission = data.frame(Id = id)

submission$Response = as.integer(preds)

write_csv(submission, "D:\\kaggle\\PRUDENTIAL\\submission\\02062016_glm.csv")


#############################################################################################
#############################################################################################

# run xgboost--------------------------------------------------------------------------------

# independant of glm------------------------------------------------------------------------

dataset_blend_train <- read_csv("D:\\kaggle\\HOMESITE\\blend\\bag\\FINAL\\blend_train_02052016.csv")

dataset_blend_test <- read_csv("D:\\kaggle\\HOMESITE\\blend\\bag\\FINAL\\blend_test_02052016.csv")

train <- cbind(train, data.frame(dataset_blend_train))

test <- cbind(test, data.frame(dataset_blend_test))

set.seed(2*07*2016)

feature.names <- names(train)

dtrain<-xgb.DMatrix(data=data.matrix(train),label=response, missing = NaN)

param <- list(  objective           = "binary:logistic", 
                
                booster = "gbtree",
                
                eval_metric = "auc",
                
                eta                 = 0.023, # 0.06, #0.01,
                
                max_depth           = 6, #changed from default of 8
                
                subsample           = 0.83, # 0.7
                
                colsample_bytree    = 0.77, # 0.7
                
                num_parallel_tree = 2
                
)


mod <- xgb.train(   params              = param,
                    
                    data                = dtrain,
                    
                    nrounds             = 3000,
                    
                    verbose             = 1,
                    
                    maximize            = T,
                    
                    nthread = 4)


pred <- predict(mod, data.matrix(test), missing = NaN)


## create submission file-----------------------------------------------------------------------

submission <- data.frame(QuoteNumber = id, QuoteConversion_Flag = pred)

write_csv(submission, "D:\\kaggle\\HOMESITE\\submission\\02072016_1_xgb.csv")
