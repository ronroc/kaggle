library(caret); library(gbm)


train <- tmp_new[c(1:260753), ]

test <- tmp_new[c(260754:434589), ]

rm(tmp_new)

gc()



### Returns train indices for n_folds using StratifiedKFold


skf = createFolds(y, k = n_folds , list = TRUE, returnTrain = TRUE)


### Create a list of models to run

clfs <- c("xgboost", "glm")


### Pre-allocate the data

### For each model, add a column with N rows for each model


dataset_blend_train = matrix(0, nrow(train), length(clfs))


### Loop over the models


j <- 0 


for (clf in clfs)
  
  {
  
  j <- j + 1
  
  print(paste(j,clf))
  
  
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
    
    
    ### Stupid hack to fit the model
    
    if (clf == "xgboost") {
      
      feature.names <- names(train)
      
      dtrain <- xgb.DMatrix(data=data.matrix(x_train),label=y_train )
      
      param <- list(  objective           = "binary:logistic",
                      
                      booster = "gbtree",
                      
                      eval_metric = "auc",
                      
                      eta                 = 0.023, # 0.06, #0.01,
                      
                      max_depth           = 6, #changed from default of 8
                      
                      subsample           = 0.83, # 0.7
                      
                      colsample_bytree    = 0.77, # 0.7
                      
                      num_parallel_tree = 2,
                      
                      min_child_weight = 5
                      
      )
      
      start <- Sys.time()
      
      require(doParallel)
      
      cl <- makeCluster(2); registerDoParallel(cl)
      
      set.seed(1*2*16)
      
      mod <- xgb.train(   params              = param,
                          
                          data                = dtrain,
                          
                          nrounds             = 3000,
                          
                          verbose             = 1,  #1
                          
                          #early.stop.round    = 150,
                          
                          #watchlist           = watchlist,
                          
                          maximize            = T,
                          
                          nthread = 2)
      
      dataset_blend_train[tmp_train, j] <- predict(mod, data.matrix(x_test))
      
    }
    
    
    else if (clf == "glm") {
      
      start <- Sys.time()
      
      mod <- h2o.glm( x = feature.names,
                      
                      y = "target",
                      
                      training_frame = training_frame,
                      
                      validation_frame = validation_frame,
                      
                      family = "binomial",
                      
                      lambda_search = TRUE,
                      
                      nlambdas = 10, 
                      
                      model_id = "glm_test", 
                      
                      solver = "L_BFGS",
                      
                      keep_cross_validation_predictions = T,
                      
                      alpha = c(0, 0.1, 0.2, 0.3, 0.4, 0.6, 0.8, 1), 
                      
                      link = "logit", 
                      
                      standardize = T)
      
      pred_glm <- h2o.predict(object = mod, newdata = x_test)
      
      dataset_blend_train[tmp_train, j] <- as.data.frame(pred_glm)
      
      
    }
    
  }
}


# train a model (linear regression) using RESPONSE against each of your model's predictions  

logit <- glm(y ~ X1 + X2, data = data.frame( response, dataset_blend_train ), family = binomial(logit))

logit


# record the coefficient estimates (excluding intercept) -- these are your model's weights

w1 = coefficient(1) / sum(coefficient(1) + coefficient(2))

w2 = coefficient(2) / sum(coefficient(1) + coefficient(2))



# fit original training data to each model, run predictions on your real test data | 


mod_1 <- gbm(y ~ ., data = data.frame(y, x), n.trees = 1000, 
           
           interaction.depth =8, train.fraction = 0.8

           )

           
pred_1 <- predict(mod_1, X_submission, n.trees = best.iter, type = "response")


mod_2 <- gbm( y ~ ., data = data.frame( y, x ), n.trees = 10000, 
             
             interaction.depth =20, train.fraction = 0.9

             )


pred_2 <- predict(mod_1, X_submission, n.trees = best.iter, type = "response")



preds = w1*pred_1 + w2*pred_2

# write submission code and export it to csv
