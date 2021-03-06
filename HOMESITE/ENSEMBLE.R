# begin blend-------------------------------------------------------------------------------------------


### Returns train inidices for n_folds using StratifiedKFold

skf = createFolds(response, k = 3)


### Create a list of models to run

clfs <- c("glm", "gbm", "rf")


### Pre-allocate the data

### For each model, add a column with N rows for each model


dataset_blend_train = matrix(0, nrow(train), length(clfs))

dataset_blend_test  = matrix(0, nrow(test), length(clfs))



### Loop over the models------------------------------------------------------------------------------

j <- 0 

for (clf in clfs)
  
{
  
  j <- j + 1
  
  print(paste(j,clf))
  
  
  ### Create a tempory array that is (Holdout_Size, N_Folds).
  
  ### Number of testing data x Number of folds , we will take the mean of the predictions later
  
  dataset_blend_test_j = matrix(0, nrow(test), length(skf))
  
  print(paste(nrow(dataset_blend_test_j),ncol(dataset_blend_test_j)))
  
  
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
    
    y_test  = response[tmp_train] # write a scoring method to score auc from y_test
    
    
    ### fit xgboost--------------------------------------------------------------------------------------
    
#     if(clf == "xgboost_1"){
#       
#       
#       
#       feature.names <- names(train)
      
      # h<-sample(nrow(train),2000)
      
      # dval<-xgb.DMatrix(data=data.matrix(train[h,]),label=response[h])
      
      # dtrain<-xgb.DMatrix(data=data.matrix(train[-h,]),label=response[-h])
      
      # dtrain<-xgb.DMatrix(data=data.matrix(x_train),label=y_train, missing = NaN)
      
      # watchlist<-list(val=dval,train=dtrain)
      
#       param <- list(  objective           = "binary:logistic",
#                       
#                       booster = "gbtree",
#                       
#                       eval_metric = "auc",
#                       
#                       eta                 = 0.023, # 0.06, #0.01,
#                       
#                       max_depth           = 6, #changed from default of 8
#                       
#                       subsample           = 0.83, # 0.7
#                       
#                       colsample_bytree    = 0.77, # 0.7
#                       
#                       num_parallel_tree = 2,
#                       
#                       min_child_weight = 5
#                       
#       )
#       
#       start <- Sys.time()
#       
#       require(doParallel)
#       
#       cl <- makeCluster(2); registerDoParallel(cl)
#       
#       set.seed(1142016)
#       
#       mod <- xgb.train(   params              = param,
#                           
#                           data                = dtrain,
#                           
#                           nrounds             = 2000,
#                           
#                           verbose             = 1,  #1
#                           
#                           #early.stop.round    = 150,
#                           
#                           # watchlist           = watchlist,
#                           
#                           maximize            = T,
#                           
#                           nthread = 2)
#       
#       
#       dataset_blend_train[tmp_train, j] <- predict(mod, data.matrix(x_test), missing = NaN)
#       
#       xgb.save(clf, "D:\\kaggle\\HOMESITE\\ensemble\\011402016_xgb.R")
#       
#     }
    
    
    # fit rf---------------------------------------------------------------------------------------------
    
    if (clf == "rf"){
      
      # combining them in a df for maintaining format
      
      require(h2o)
      
      localH2O <- h2o.init(max_mem_size = "10g")
      
      x_train$target <- y_train
      
      train.hex <- as.h2o(localH2O, object = x_train)
      
      test.hex <- as.h2o(localH2O, object = x_test)
      
      
      myX <- names(train)
      
      myY <- "target"
      
      
      test_rf <- h2o.randomForest(x = myX,
                                  
                                  y = myY,
                                  
                                  training_frame = train.hex,
                                  
                                  model_id = "rf_1152016", 
                                  
                                  ntrees = 1000, 
                                  
                                  max_depth = 10, 
                                  
                                  binomial_double_trees = T, 
                                  
                                  balance_classes = T, 
                                  
                                  seed = 01152016
                                  
      )
      
      pred_rf <- h2o.predict(object = test_rf, newdata = test.hex)
      
      pred_rf <- as.data.frame(pred_rf)
      
      
      dataset_blend_train[tmp_train, j] <- pred_rf$predict
      
      
      
    }
    
    
    
    # fit gbm--------------------------------------------------------------------------------------------
    
    else if(clf == "gbm"){
      
      require(h2o)
      
      localH2O <- h2o.init(max_mem_size = "10g")
      
      x_train$target <- y_train
      
      train.hex <- as.h2o(localH2O, object = x_train)
      
      test.hex <- as.h2o(localH2O, object = x_test)
      
      
      myX <- names(train)
      
      myY <- "target"
      
      
      
      test_gbm <- h2o.gbm(x = myX,
                          
                          y = myY,
                          
                          training_frame = train.hex,
                          
                          model_id = "gbm_01152016", 
                          
                          ntrees =  1000, 
                          
                          max_depth = 20, 
                          
                          learn_rate = 0.014, 
                          
                          seed = 01152016, 
                          
                          balance_classes = T, 
                          
                          min_rows = 9,
                          
                          family = "binomial"
      )
      
      
      pred_gbm <- h2o.predict(object = test_gbm, newdata = test.hex)
      
      pred_gbm <- as.data.frame(pred_gbm)
      
      
      dataset_blend_train[tmp_train, j] <- pred_gbm$predict
      
      
      
    }
    
    
    
    # fit glm--------------------------------------------------------------------------------------------
    
    else if(clf == "glm"){
      
      require(h2o)
      
      localH2O <- h2o.init(max_mem_size = "10g")
      
      x_train$target <- y_train
      
      train.hex <- as.h2o(localH2O, object = x_train)
      
      test.hex <- as.h2o(localH2O, object = x_test)
      
      
      myX <- names(train)
      
      myY <- "target"
      
      
      test_glm <- h2o.glm( x = myX,
                           
                           y = myY,
                           
                           training_frame = train.hex,
                           
                           family = "binomial",
                           
                           lambda_search = TRUE,
                           
                           nlambdas = 5, 
                           
                           model_id = "glm_test", 
                           
                           solver = "L_BFGS",
                           
                           keep_cross_validation_predictions = T,
                           
                           alpha = c(0, 0.1, 0.2, 0.3, 0.4, 0.6, 0.8, 1), 
                           
                           link = "logit", 
                           
                           standardize = T
                           
      )
      
      pred_glm <- h2o.predict(object = test_glm, newdata = test.hex)
      
      pred_glm <- as.data.frame(pred_glm)
      
      
      dataset_blend_train[tmp_train, j] <- pred_glm$p0
      
      
      
    }
    
    
    # predict xgboost for test set---------------------------------------------------------------------
    
#     if(clf == "xgboost_1")
#       
#     {
#       
#       print(paste("predicting xgboost for test set ; Fold :", i))
#       
#       dataset_blend_test_j[, i] <- predict(mod, data.matrix(test), missing = NaN)
#       
#     }
    
    # predict rf for test set------------------------------------------------------------------
    
    
    if(clf == "rf"){
      
      print(paste("predicting rf for test set ; Fold :", i))
      
      test.hex <- as.h2o(localH2O, object = test)
      
      pred_rf <- h2o.predict(object = test_rf, newdata = test.hex)
      
      pred_rf <- as.data.frame(pred_rf)
      
      
      dataset_blend_test_j[, i] <- pred_rf$predict
      
    }
    
    # predict gbm for test set---------------------------------------------------------------
    
    
    else if(clf == "gbm"){
      
      print(paste("predicting gbm for test set ; Fold :", i))
      
      test.hex <- as.h2o(localH2O, object = test)
      
      pred_gbm <- h2o.predict(object = test_gbm, newdata = test.hex)
      
      pred_gbm <- as.data.frame(pred_gbm)
      
      
      dataset_blend_test_j[, i] <- pred_gbm$predict
    }
    
    
    # predict glm for test set------------------------------------------------------------------
    
    else if(clf == "glm"){
      
      print(paste("predicting glm for test set ; Fold :", i))
      
      test.hex <- as.h2o(localH2O, object = test)
      
      pred_glm <- h2o.predict(object = test_glm, newdata = test.hex)
      
      pred_glm <- as.data.frame(pred_glm)
      
      
      dataset_blend_test_j[, i] <- pred_glm$p0
      
      
      
    }
    
  }
  
  dataset_blend_test[,j] = rowMeans(dataset_blend_test_j)  
  
}


write_csv(as.data.frame(dataset_blend_test), "dataset_blend_test.csv")

write_csv(as.data.frame(dataset_blend_train), "dataset_blend_train.csv")
