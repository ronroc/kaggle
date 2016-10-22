
# create folds------------------------------------------------------------------------------------------

dataset_blend_train = matrix(0, nrow(train), 5)

dataset_blend_test_j = matrix(0, nrow(test), 5)

dataset_blend_test = matrix(0, nrow(test), 5)

# start iteration loop---------------------------------------------------------------------------------

for(j in 1:5)
  
{
  
  print(paste("starting glm iteration ; number :", j))
  
  set.seed(1*23*2016*j)
  
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
    
    x_train$target <- y_train
    
    train.hex <- as.h2o(localH2O, object = x_train)
    
    test.hex <- as.h2o(localH2O, object = x_test)
    
    
    myX <- names(train)
    
    myY <- "target"
    
    print(paste("training glm iteration :", j, "for Fold ; number :", i))
    
    test_glm <- h2o.glm( x = myX,
                         
                         y = myY,
                         
                         training_frame = train.hex,
                         
                         family = "poisson",
                         
                         lambda_search = TRUE,
                         
                         nlambdas = 5, 
                         
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
    
  
    print(paste("predicting glm for test set iteration :", j, "; Fold :", i))
    
    test.hex <- as.h2o(localH2O, object = test)
    
    pred_glm <- h2o.predict(object = test_glm, newdata = test.hex)
    
    pred_glm <- as.data.frame(pred_glm)
    
    dataset_blend_test_j[, i] <- pred_glm$predict
    
  }
  
  dataset_blend_test[, j] <- rowMeans(dataset_blend_test_j)
  
}


require(readr)

write_csv(data.frame(dataset_blend_train), "D:\\kaggle\\PRUDENTIAL\\blend\\bag\\glm\\blend_train_glm_02022016.csv")

write_csv(data.frame(dataset_blend_test), "D:\\kaggle\\PRUDENTIAL\\blend\\bag\\glm\\blend_test_glm_02022016.csv")
