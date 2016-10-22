require(doParallel); require(xgboost); require(caret)

cl <- makeCluster(4); registerDoParallel(cl)

# create folds------------------------------------------------------------------------------------------

dataset_blend_train = matrix(0, nrow(train), 1)

dataset_blend_test_j = matrix(0, nrow(test), 2) # this should always be number of folds 

dataset_blend_test = matrix(0, nrow(test), 1)

# start iteration loop---------------------------------------------------------------------------------

  
  j = 1
  
  print(paste("starting xgboost iteration ; number :", j))
  
  set.seed(2*05*2016*j)
  
  require(caret)
  
  skf = createFolds(response, k = 2)
  
  print(paste(nrow(dataset_blend_test_j),ncol(dataset_blend_test_j)))
  
  # start fold loop------------------------------------------------------------------------------------
  
  ### Loop over the folds
  
  i <- 0
  
  for (sk in skf) {
    
    i <- i + 1
    
    print(paste("Fold", i))
    
    ### Extract and fit the train/test section for each fold
    
    tmp_train <- unlist(skf[i])
    
    x_train = train[-tmp_train,]
    
    y_train = response[-tmp_train]
    
    x_test  = train[tmp_train,]
    
    y_test  = response[tmp_train]
    
    
    feature.names <- names(train)
    
    dtrain<-xgb.DMatrix(data=data.matrix(x_train),label=y_train, missing = NaN)
    
    param <- list(  objective           = "binary:logistic", 
                    
                    booster = "gbtree",
                    
                    eval_metric = "auc",
                    
                    eta                 = 0.023, # 0.06, #0.01,
                    
                    max_depth           = 8, #changed from default of 8
                    
                    subsample           = 0.83, # 0.7
                    
                    colsample_bytree    = 0.77, # 0.7
                    
                    num_parallel_tree = 2
                    
    )
    
    # start training------------------------------------------------------------------------------
    
    print(paste("training xgboost for iteration :", j, "Fold ; number :", i))
    
        
    mod <- xgb.train(   params              = param,
                        
                        data                = dtrain,
                        
                        nrounds             = 3000,
                        
                        verbose             = 1,
                        
                        maximize            = T,
                        
                        nthread = 4)
    
    dataset_blend_train[tmp_train, j] <- predict(mod, data.matrix(x_test), missing = NaN)
    
    
    
    print(paste("predicting xgboost for test set iteration:", j, " ; Fold :", i))
    
    dataset_blend_test_j[, i] <- predict(mod, data.matrix(test), missing = NaN)
    
  }
  
  dataset_blend_test[, j] <- rowMeans(dataset_blend_test_j)
  


require(readr)

write_csv(data.frame(dataset_blend_train), "D:\\kaggle\\HOMESITE\\blend\\bag\\xgb\\blend_train_xgb_02052016.csv")

write_csv(data.frame(dataset_blend_test), "D:\\kaggle\\HOMESITE\\blend\\bag\\xgb\\blend_test_xgb_02052016.csv")
