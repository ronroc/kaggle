
# combine bag_xgb, bag_rf, bag_gbm, bag_glm

# use dataset_blend_train to train a second level model-------------------------------------------

require(readr); require(xgboost)

dataset_blend_train <- read_csv("D:\\kaggle\\PRUDENTIAL\\blend\\bag\\FINAL\\02142016\\xgb_all_train_02142016.csv")

dataset_blend_test <- read_csv("D:\\kaggle\\PRUDENTIAL\\blend\\bag\\FINAL\\02142016\\xgb_all_test_02142016.csv")

train <- cbind(train, data.frame(dataset_blend_train))

test <- cbind(test, data.frame(dataset_blend_test))

# train a second level using xgboost-------------------------------------------------------------

## use crossval setup to get optimum cutpoints equation

require(caret)

skf = createFolds(response, k = 10)

dataset_blend_train  = matrix(0, nrow(train), 1)

### Loop over the folds

i <- 0

for (sk in skf) {
  
  i <- i + 1
  
  print(paste("Fold", i))
  
  
  ### Extract and fit the train/test section for each fold
  
  tmp_train <- unlist(skf[i])
  
  x_train = train[-tmp_train, ]
  
  y_train = response[-tmp_train]
  
  x_test  = train[tmp_train,]
  
  y_test  = response[tmp_train]
  
  
  dtrain <- xgb.DMatrix(data = data.matrix(x_train),label = y_train, missing = NaN )
  
  param <- list(  print.every.n       = 20,
                  
                  objective           = "reg:linear",
                  
                  depth = 21,
                  
                  min_child_weight = 5,
                  
                  subsample = 0.71,
                  
                  eta = 0.01,
                  
                  silent = 0
                  
  )
  
  
  
  start <- Sys.time()
  
  
  mod <- xgb.train(   params              = param,
                      
                      booster = "gbtree",
                      
                      data                = dtrain,
                      
                      nrounds             = 3000,
                      
                      verbose             = 1,
                      
                      maximize            = T,
                      
                      nthread = 4
                      
  )
  
  
  dataset_blend_train[tmp_train, 1] <- predict(mod, data.matrix(x_test), missing = NaN)
  
}


#############################################################################################################

SQWKfun = function(x = seq(1.5, 7.5, by = 1), pred) {
  preds = pred$predict
  true = pred$Response
  cuts = c(min(preds), x[1], x[2], x[3], x[4], x[5], x[6], x[7], max(preds))
  preds = as.numeric(Hmisc::cut2(preds, cuts))
  err = Metrics::ScoreQuadraticWeightedKappa(preds, true, 1, 8)
  return(-err)
}

# optimise using optim on train and predict on test---------------------------------------------- 

pred = data.frame(Id=train_id, Response=response, predict=dataset_blend_train)

# on further iterations instead of using oof preds, actual train preds may be used | {might overfit}

optCuts = optim(seq(1.5, 7.5, by = 1), SQWKfun, pred = pred)

print(optCuts)


# predict using optimal cut points---------------------------------------------------------------

dtrain <- xgb.DMatrix(data = data.matrix(train),label = response, missing = NaN )

param <- list(  print.every.n       = 20,
                
                objective           = "reg:linear",
                
                depth = 21,
                
                min_child_weight = 5,
                
                subsample = 0.71,
                
                eta = 0.01,
                
                silent = 0
                
)



start <- Sys.time()


mod <- xgb.train(   params              = param,
                    
                    booster = "gbtree",
                    
                    data                = dtrain,
                    
                    nrounds             = 3000,
                    
                    verbose             = 1,
                    
                    maximize            = F,
                    
                    nthread = 4
                    
)

predict_test = predict(mod, data.matrix((test)), missing = NaN)

preds = as.numeric(Hmisc::cut2(predict_test, c(-Inf, optCuts$par, Inf)))



## create submission file-----------------------------------------------------------------------

submission = data.frame(Id = id)

submission$Response = as.integer(preds)

write_csv(submission, "D:\\kaggle\\PRUDENTIAL\\submission\\02142016_4.csv")
