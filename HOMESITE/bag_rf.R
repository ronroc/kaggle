require(h2o); require(xgboost); require(readr)

response <- train.y

# train <- read_csv("D:\\kaggle\\HOMESITE\\Data\\aws\\train_01262016.csv")
# 
# train_raw <- train
# 
# train <- train_raw[sample(nrow(train_raw), 30000), ]
# 
# test <- read_csv("D:\\kaggle\\HOMESITE\\Data\\aws\\test_01262016.csv")
# 
# test_raw <- test
# 
# test <- test_raw[sample(nrow(test_raw), 30000), ]
# 
# response_1 <- read_csv("D:\\kaggle\\HOMESITE\\Data\\aws\\response_01262016.csv")
# 
# response <- response_1$response
# 
# response <- response_raw[sample(length(response),30000)]

# create folds------------------------------------------------------------------------------------------

dataset_blend_train = matrix(0, nrow(train), 5)

dataset_blend_test_j = matrix(0, nrow(test), 5) # this should always be number of folds

dataset_blend_test = matrix(0, nrow(test), 5)

# start iteration loop---------------------------------------------------------------------------------

for(j in 1:5)

  {

  print(paste("starting rf iteration ; number :", j))
  
  set.seed(2*05*2016*j)
  
  sample_train <- sample(x = nrow(train), size = nrow(train), replace = T )
  
  sample_test <- sample(x = nrow(test), size = nrow(test), replace = T )
  
  train <- train[sample_train, ]
  
  test <- train[sample_test, ]
  
  
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
    
    localH2O <- h2o.init(nthreads = -1, max_mem_size = '12g', assertion = F)
    
    x_train$target <- as.factor(y_train)
    
    train.hex <- as.h2o(x_train)
    
    test.hex <- as.h2o(x_test)
    
    
    myX <- names(train)
    
    myY <- "target"
    
    print(paste("training rf for iteration : ", j, "Fold ; number :", i))
    
    test_rf <- h2o.randomForest(x = myX,
                                
                                y = myY,
                                
                                training_frame = train.hex, 
                                
                                ntrees = 1000, 
                                
                                max_depth = 10, 
                                
                                binomial_double_trees = T, 
                                
                                balance_classes = T
                                
    )
    
    pred_rf <- h2o.predict(object = test_rf, newdata = test.hex)
    
    pred_rf <- as.data.frame(pred_rf)
    
    
    dataset_blend_train[tmp_train, j] <- pred_rf$p0
    
    
    print(paste("predicting rf for test set iteration :", j, "; Fold :", i))
    
    test.hex <- as.h2o(test)
    
    pred_rf <- h2o.predict(object = test_rf, newdata = test.hex)
    
    pred_rf <- as.data.frame(pred_rf)
    
    
    dataset_blend_test_j[, i] <- pred_rf$p0
    
    
  }
  
  dataset_blend_test[, j] <- rowMeans(dataset_blend_test_j)
  
}

require(readr)
  
write_csv(data.frame(dataset_blend_train), "D:\\kaggle\\SANTANDER\\copy\\ENSM\\RF\\TRAIN\\blend_train_rf_04232016_sample.csv")
  
write_csv(data.frame(dataset_blend_test), "D:\\kaggle\\SANTANDER\\copy\\ENSM\\RF\\TEST\\blend_test_rf_04232016_sample.csv")