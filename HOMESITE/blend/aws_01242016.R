install.packages("doParallel"); install.packages("xgboost"); install.packages("caret"); install.packages("readr")

require(doParallel); require(xgboost); require(caret); require(readr)

detectCores()

train <- read_csv("train_01242016.csv")

train <- read_csv("C:\\Users\\amulya\\Documents\\Kaggle\\PRUDENTIAL\\Data\\train_01242016.csv")

test <- read_csv("test_01242016.csv")

test <- read_csv("C:\\Users\\amulya\\Documents\\Kaggle\\PRUDENTIAL\\Data\\test_01242016.csv")

response_1 <- read_csv("C:\\Users\\amulya\\Documents\\Kaggle\\PRUDENTIAL\\Data\\response.csv")

response <- response_1$response

id_1 <- read_csv("id.csv")

id <- id_1$id

# create folds------------------------------------------------------------------------------------------

dataset_blend_train = matrix(0, nrow(train), 25)

dataset_blend_test_j = matrix(0, nrow(test), 5)

dataset_blend_test = matrix(0, nrow(test), 25)

# start iteration loop---------------------------------------------------------------------------------

for(j in 1:25)
  
{

  Sys.time()
  
  print(paste("starting xgboost iteration ; number :", j))
  
  set.seed(1*23*2016*j)
  
  require(caret)
  
  skf = createFolds(response, k = 5)
  
  print(paste(nrow(dataset_blend_test_j),ncol(dataset_blend_test_j)))
  
  # start fold loop------------------------------------------------------------------------------------
  
  ### Loop over the folds
  
  i <- 0
  
  Sys.time()
  
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
    
    # missing = nan not required
    
    dtrain<-xgb.DMatrix(data=data.matrix(x_train),label=y_train)
    
    param <- list( objective           = "reg:linear",
                   
                   depth = 21,
                   
                   min_child_weight = 3,
                   
                   subsample = 0.71,
                   
                   eta = 0.01,
                   
                   silent = 0
    )
    
    # start training------------------------------------------------------------------------------
    
    print(paste("training xgboost for iteration :", j, "Fold ; number :", i))
    
    mod <- xgb.train(   params              = param,
                        
                        booster = "gbtree",
                        
                        data                = dtrain,
                        
                        nrounds             = 3000,
                        
                        verbose             = 1,
                        
                        maximize            = F,
                        
                        nthread = detectCores()
                        
    )
    
    dataset_blend_train[tmp_train, j] <- predict(mod, data.matrix(x_test))
    
    
    
    print(paste("predicting xgboost for test set iteration:", j, " ; Fold :", i))
    
    dataset_blend_test_j[, i] <- predict(mod, data.matrix(test))
    
  }
  
  dataset_blend_test[, j] <- rowMeans(dataset_blend_test_j)
  
}

require(readr)

write_csv(data.frame(dataset_blend_train), "blend_train_xgb_01252016.csv")

write_csv(data.frame(dataset_blend_test_j), "blend_test_xgb_01252016.csv")

