
# create folds------------------------------------------------------------------------------------------

dataset_blend_train = matrix(0, nrow(train), 5)

dataset_blend_test_j = matrix(0, nrow(test), 5) # this should always be number of folds

dataset_blend_test = matrix(0, nrow(test), 5)

response <- train.y

# start iteration loop---------------------------------------------------------------------------------

for(j in 1:5)

{  
#  j = 1
  
  print(paste("starting glm iteration ; number :", j))
  
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
    
    localH2O <- h2o.init(nthreads = 4, max_mem_size = '12g')
    
    x_train$target <- (y_train)
    
    train.hex <- as.h2o(localH2O, object = x_train)
    
    test.hex <- as.h2o(localH2O, object = x_test)
    
    
    myX <- names(train)
    
    myY <- "target"
    
    print(paste("training glm iteration :", j, "for Fold ; number :", i))
    
    test_glm <- h2o.glm( x = myX,
                    
                    y = myY,
                    
                    training_frame = train.hex,
                    
                    family = "binomial",
                    
                    solver = "L_BFGS",
                    
                    link = "logit", 
                    
                    standardize = T, 
                    
                    lambda_search = T
                    
                    )

    pred_glm <- h2o.predict(test_glm, newdata = test.hex)
    
    pred_glm <- as.data.frame(pred_glm)
    
    
#     test_glm <- glm(formula = target ~ ., family = binomial(link = "logit"), data = x_train)
        
    
    
    dataset_blend_train[tmp_train, j] <- pred_glm$p1
    
    
    print(paste("predicting glm for test set iteration :", j, "; Fold :", i))
    
    test.hex <- as.h2o(localH2O, object = test)
    
    pred_glm <- h2o.predict(object = test_glm, newdata = test.hex)
    
    pred_glm <- as.data.frame(pred_glm)
    
    dataset_blend_test_j[, i] <- pred_glm$p1
    
  }
  
  dataset_blend_test[, j] <- rowMeans(dataset_blend_test_j)
  
}

require(readr)

write_csv(data.frame(dataset_blend_train), "D:\\kaggle\\SANTANDER\\copy\\ENSM\\GLM\\TRAIN\\blend_train_glm_04232016_sample_1.csv")

write_csv(data.frame(dataset_blend_test), "D:\\kaggle\\SANTANDER\\copy\\ENSM\\GLM\\TEST\\blend_test_glm_04232016_1_sample_1.csv")

########################################################################################################################