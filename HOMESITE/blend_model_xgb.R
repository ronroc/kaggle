# combine bag_xgb, bag_rf, bag_gbm, bag_glm

# use dataset_blend_train to train a second level model-------------------------------------------

train <- cbind(train, data.frame(dataset_blend_train))

test <- cbind(test, data.frame(dataset_blend_test))


# train a second level using xgboost-------------------------------------------------------------

## use crossval setup to get optimum cutpoints equation

feature.names <- names(train)

dtrain<-xgb.DMatrix(data=data.matrix(x_train),label=y_train, missing = NaN)

param <- list(  objective           = "binary:logistic", 
                
                booster = "gbtree",
                
                eval_metric = "auc",
                
                eta                 = 0.023, # 0.06, #0.01,
                
                max_depth           = 6, #changed from default of 8
                
                subsample           = 0.83, # 0.7
                
                colsample_bytree    = 0.77, # 0.7
                
                num_parallel_tree = 2
                
)

# start training------------------------------------------------------------------------------

print(paste("training xgboost for iteration :", j, "Fold ; number :", i))

mod <- xgb.train(   params              = param,
                    
                    data                = dtrain,
                    
                    nrounds             = 3000,
                    
                    verbose             = 1,  #1
                    
                    #early.stop.round    = 150,
                    
                    watchlist           = watchlist,
                    
                    maximize            = T,
                    
                    nthread = 2)

predict_test = predict(mod, data.matrix((test)), missing = NaN)


## create submission file-----------------------------------------------------------------------

submission = data.frame(Id = id)

submission$Response = as.integer(preds)

write_csv(submission, "D:\\kaggle\\PRUDENTIAL\\submission\\01232016.csv")
