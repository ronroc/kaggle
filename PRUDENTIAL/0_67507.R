
#set.seed(01*22*2016)

# read in the data file--------------------------------------------------------------------------------

require(data.table); require(xgboost); require(h2o); require(caret)


train_raw <- fread("D:\\kaggle\\PRUDENTIAL\\Data\\train.csv", data.table = F)

test  <- fread("D:\\kaggle\\PRUDENTIAL\\Data\\test.csv", data.table = F)

train_id <- train_raw$Id

train$Id <- NULL;

id <- test$Id; test$Id <- NULL

response <- train$Response; train$Response <- NULL


tmp <- rbind(train, test)


row_NA <- apply(tmp, 1, function(x) sum(is.na(x)))

tmp$row_NA <- row_NA


# dummify varible------------------------------------------------------------------------------

tmp_dummy <- data.frame(tmp[,"Product_Info_2"])

tmp_dummy[ , 1] <- as.factor(tmp_dummy[ , 1])


dummies <- dummyVars( ~ ., data = tmp_dummy)

gc()

tmp_dummy <- predict(dummies, newdata = tmp_dummy)

tmp_dummy <- data.frame(tmp_dummy)

dim(tmp_dummy)


# count the number of keywords row wise--------------------------------------------------------

keywords <- paste("Medical_Keyword_", 1:48, sep="")

tmp_count <- tmp[, keywords]


count <- apply(tmp_count, 1, function(x) sum(x))

tmp$count <- count


##############################################################################################

tmp[is.na(tmp)] <- -1


# interaction features-------------------------------------------------------------------------


tmp_int <- tmp[ , c("Ins_Age", "BMI")]

for (i in 1:ncol(tmp_int)) {
  
  for (j in (i + 1) : (ncol(tmp_int) + 1)) {
    
    #    a = i; b= j
    
    var.x <- colnames(tmp_int)[i]
    
    var.y <- colnames(tmp_int)[j]
    
    var.new <- paste0(var.x, '_plus_', var.y)
    
    tmp_int[ , paste0(var.new)] <- tmp_int[, i] + tmp_int[, j]
    
  }
}


gc()


tmp$Medical_History_10 <- NULL

tmp$Medical_History_24 <- NULL


############################################################################################################



feature.names <- names(tmp)

for (f in feature.names) {
  
  if (class(tmp[[f]])=="character") {
    
    levels <- unique(c(tmp[[f]]))
    
    tmp[[f]] <- as.integer(factor(tmp[[f]], levels=levels))
    
  }
}


tmp_new <- cbind(tmp, tmp_dummy, tmp_int)

tmp_new <- tmp_new[ , !(names(tmp_new) %in% c("Product_Info_2"))]

train <- tmp_new[c(1:59381),]

test <- tmp_new[c(59382:79146),]


rm(list = c("tmp", "tmp_category", "tmp_dummy", "tmp_high_card", "train_high_card",
            
            "test_high_card", "tmp_new", "tmp_count"))

gc()


# begin blend-------------------------------------------------------------------------------------------

### Returns train inidices for n_folds using StratifiedKFold

require(caret)


skf = createFolds(response, k = 5)


### Create a list of models to run

clfs <- c("rf", "gbm", "xgboost_1","glm")


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
    
    if(clf == "xgboost_1"){
      
      
      
      feature.names <- names(train)
      
      # h<-sample(nrow(train),2000)
      
      # dval<-xgb.DMatrix(data=data.matrix(train[h,]),label=response[h])
      
      # dtrain<-xgb.DMatrix(data=data.matrix(train[-h,]),label=response[-h])
      
      dtrain<-xgb.DMatrix(data=data.matrix(x_train),label=y_train, missing = NaN)
      
      # watchlist<-list(val=dval,train=dtrain)
      
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
      
      dataset_blend_train[tmp_train, j] <- predict(mod, data.matrix(x_test), missing = NaN)
      
    }
    
    
    # fit rf---------------------------------------------------------------------------------------------
    
    else if (clf == "rf"){
      
      # combining them in a df for maintaining format
      
      require(h2o)
      
      localH2O <- h2o.init(nthreads = -1, max_mem_size = '12g')
      
      
      x_train$target <- (y_train)
      
      train.hex <- as.h2o(localH2O, object = x_train)
      
      test.hex <- as.h2o(localH2O, object = x_test)
      
      
      myX <- names(train)
      
      myY <- "target"
      
      
      test_rf <- h2o.randomForest(x = myX,
                                  
                                  y = myY,
                                  
                                  training_frame = train.hex,
                                  
                                  model_id = "rf_1152016", 
                                  
                                  ntrees = 1500, 
                                  
                                  max_depth = 10, 
                                  
                                  binomial_double_trees = T, 
                                  
                                  balance_classes = T, 
                                  
                                  seed = 01*22*2016
                                  
      )
      
      pred_rf <- h2o.predict(object = test_rf, newdata = test.hex)
      
      pred_rf <- as.data.frame(pred_rf)
      
      
      dataset_blend_train[tmp_train, j] <- pred_rf$predict
      
      
      
    }
    
    
    
    # fit gbm--------------------------------------------------------------------------------------------
    
    else if(clf == "gbm"){
      
      require(h2o)
      
      localH2O <- h2o.init(nthreads = -1, max_mem_size = '12g')
      
      
      
      x_train$target <- as.factor(y_train)
      
      train.hex <- as.h2o(localH2O, object = x_train)
      
      test.hex <- as.h2o(localH2O, object = x_test)
      
      
      myX <- names(train)
      
      myY <- "target"
      
      
      
      test_gbm <- h2o.gbm(x = myX,
                          
                          y = myY,
                          
                          training_frame = train.hex,
                          
                          model_id = "gbm_01152016", 
                          
                          ntrees =  2000, 
                          
                          max_depth = 20, 
                          
                          learn_rate = 0.014, 
                          
                          seed = 01*22*2016, 
                          
                          distribution= "multinomial", 
                          
                          min_rows = 9
                          
      )
      
      
      pred_gbm <- h2o.predict(object = test_gbm, newdata = test.hex)
      
      pred_gbm <- as.data.frame(pred_gbm)
      
      dataset_blend_train[tmp_train, j] <- pred_gbm$predict
      
      
      
    }
    
    
    
    # fit glm--------------------------------------------------------------------------------------------
    
    else if(clf == "glm"){
      
      require(h2o)
      
      localH2O <- h2o.init(nthreads = -1)
      
      
      
      
      x_train$target <- y_train
      
      train.hex <- as.h2o(localH2O, object = x_train)
      
      test.hex <- as.h2o(localH2O, object = x_test)
      
      
      myX <- names(train)
      
      myY <- "target"
      
      
      test_glm <- h2o.glm( x = myX,
                           
                           y = myY,
                           
                           training_frame = train.hex,
                           
                           family = "poisson",
                           
                           lambda_search = TRUE,
                           
                           nlambdas = 10, 
                           
                           model_id = "glm_test", 
                           
                           solver = "L_BFGS",
                           
                           #keep_cross_validation_predictions = T,
                           
                           alpha = c(0, 0.1, 0.2, 0.3, 0.4, 0.6, 0.8, 1), 
                           
                           link = "log", 
                           
                           standardize = T
                           
      )
      
      pred_glm <- h2o.predict(object = test_glm, newdata = test.hex)
      
      pred_glm <- as.data.frame(pred_glm)
      
      
      dataset_blend_train[tmp_train, j] <- pred_glm$predict
      
    }
    
    
    
    
    
    # predict xgboost for test set---------------------------------------------------------------------
    
    if(clf == "xgboost_1")
      
    {
      
      print(paste("predicting xgboost for test set ; Fold :", i))
      
      dataset_blend_test_j[, i] <- predict(mod, data.matrix(test), missing = NaN)
      
    }
    
    # predict rf for test set------------------------------------------------------------------
    
    
    else if(clf == "rf"){
      
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
      
      
      dataset_blend_test_j[, i] <- pred_glm$predict
      
    }
    
  }
  
  dataset_blend_test[,j] = rowMeans(dataset_blend_test_j)  
  
}

require(readr)

# save train set and oof test datasets for higher level training----------------------------------


write_csv(as.data.frame(dataset_blend_test), "D:\\kaggle\\PRUDENTIAL\\blend\\blend_test_01232016")

write_csv(as.data.frame(dataset_blend_train), "D:\\kaggle\\PRUDENTIAL\\blend\\blend_train_01232016")


# check whether oof df's are saved before proceeding further


# use dataset_blend_train to train a second level model-------------------------------------------

train <- cbind(train, data.frame(dataset_blend_train))

test <- cbind(test, data.frame(dataset_blend_test))


# train a second level using xgboost-------------------------------------------------------------

## use crossval setup to get optimum cutpoints equation

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
  
  
  
  start <- Sys.time()
  
  
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

write_csv(submission, "D:\\kaggle\\PRUDENTIAL\\submission\\01232016.csv")