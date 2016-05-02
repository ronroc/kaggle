
blend_train <- read_csv("D:\\kaggle\\PRUDENTIAL\\blend\\bag\\train_01232016.csv")

blend_test <- read_csv("D:\\kaggle\\PRUDENTIAL\\blend\\bag\\test_01232016.csv")

train <- cbind(train, blend_train)

test <- cbind(test, blend_test)


# train a second level using xgboost-------------------------------------------------------------

## use crossval setup to get optimum cutpoints equation

require(caret); require(xgboost)

set.seed(01242016)

skf = createFolds(response, k = 5 )

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
                  
                  min_child_weight = 3,
                  
                  subsample = 0.71,
                  
                  eta = 0.01,
                  
                  silent = 0
                  
  )
  
  
  mod <- xgb.train(   params              = param,
                      
                      booster = "gbtree",
                      
                      data                = dtrain,
                      
                      nrounds             = 3000,
                      
                      verbose             = 1,
                      
                      maximize            = F
                      
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
                
                min_child_weight = 3,
                
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
                    
                    maximize            = F
                    
)

predict_test = predict(mod, data.matrix((test)), missing = NaN)

preds = as.numeric(Hmisc::cut2(predict_test, c(-Inf, optCuts$par, Inf)))



## create submission file-----------------------------------------------------------------------

submission = data.frame(Id = id)

submission$Response = as.integer(preds)

write_csv(submission, "D:\\kaggle\\PRUDENTIAL\\submission\\01252016.csv")

