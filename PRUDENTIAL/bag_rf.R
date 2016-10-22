require(h2o); require(xgboost); require(readr)

train <- read_csv("D:\\kaggle\\PRUDENTIAL\\Data\\New folder\\train_02072016.csv")

test <- read_csv("D:\\kaggle\\PRUDENTIAL\\Data\\New folder\\\\test_02072016.csv")

response_1 <- read_csv("D:\\kaggle\\PRUDENTIAL\\Data\\response.csv")

response <- response_1$response

id_1 <- read_csv("D:\\kaggle\\PRUDENTIAL\\Data\\id.csv")

id <- id_1$id


# create folds------------------------------------------------------------------------------------------


dataset_blend_train = matrix(0, nrow(train), 1)

dataset_blend_test_j = matrix(0, nrow(test), 5)

dataset_blend_test = matrix(0, nrow(test), 1)

# start iteration loop---------------------------------------------------------------------------------



j = 1

  print(paste("starting rf iteration ; number :", j))
  
  set.seed(02*07*2016*j)
  
  require(caret)
  
  skf = createFolds(response, k = 5)
  
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
    
    
    require(h2o)
    
    localH2O <- h2o.init(nthreads = -1, max_mem_size = '12g')
    
    x_train$target <- as.factor(y_train)
    
    train.hex <- as.h2o(localH2O, object = x_train)
    
    test.hex <- as.h2o(localH2O, object = x_test)
    
    
    myX <- names(train)
    
    myY <- "target"
    
    
    
    require(h2o)
    
    localH2O <- h2o.init(nthreads = -1)
    
    
    x_train$target <- (y_train)
    
    train.hex <- as.h2o(localH2O, object = x_train)
    
    test.hex <- as.h2o(localH2O, object = x_test)
    
    
    myX <- names(train)
    
    myY <- "target"
    
    print(paste("training rf for iteration : ", j, "Fold ; number :", i))
    
    test_rf <- h2o.randomForest(x = myX,
                                
                                y = myY,
                                
                                training_frame = train.hex, 
                                
                                ntrees = 1000, 
                                
                                max_depth = 12, 
                                
                                binomial_double_trees = T, 
                                
                                balance_classes = T
                                
    )
    
    pred_rf <- h2o.predict(object = test_rf, newdata = test.hex)
    
    pred_rf <- as.data.frame(pred_rf)
    
    
    dataset_blend_train[tmp_train, j] <- pred_rf$predict
    
    
    print(paste("predicting rf for test set iteration :", j, "; Fold :", i))
    
    test.hex <- as.h2o(localH2O, object = test)
    
    pred_rf <- h2o.predict(object = test_rf, newdata = test.hex)
    
    pred_rf <- as.data.frame(pred_rf)
    
    
    dataset_blend_test_j[, i] <- pred_rf$predict
    
    
  }
  
  dataset_blend_test[, j] <- rowMeans(dataset_blend_test_j)
  


require(readr)

write_csv(data.frame(dataset_blend_train), "D:\\kaggle\\PRUDENTIAL\\blend\\bag\\blend_train_rf_02152016.csv")

write_csv(data.frame(dataset_blend_test), "D:\\kaggle\\PRUDENTIAL\\blend\\bag\\blend_test_rf_02152016.csv")
