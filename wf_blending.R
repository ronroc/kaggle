
##########################################################################################

### Returns train inidices for n_folds using StratifiedKFold

skf = createFolds(response, k = n_folds)

### Create a list of models to run

clfs <- c("xgboost_1","xgboost_2")

### Pre-allocate the data

### For each model, add a column with N rows for each model


dataset_blend_train = matrix(0, nrow(train), length(clfs))

dataset_blend_test  = matrix(0, nrow(test), length(clfs))



### Loop over the models

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
    
      y_test  = response[tmp_train]
    
    
      ### Stupid hack to fit the model
    
      if (clf == "xgboost_1") {
        
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
    
    
      ### Predict the probability of current folds test set and store results.
    
      ### This output will be the basis for our blended classifier to train against,
    
      ### which is also the output of our classifiers
    
      #dataset_blend_train[tmp_train, j] <- predict(mod, data.matrix(x_test))
    
    
      ### Predict the probabilty for the holdout set and store results
    
      if(clf == "xgboost")
      
      {
      
        dataset_blend_test_j[, i] <- predict(mod, data.matrix(test))
      
      }
      
      else if(clf == "glm")
        
        {
        
        pred_glm <- h2o.predict(object = mod, newdata = test)
        
        dataset_blend_test_j[, i] <- as.data.frame(pred_glm)
      
        }
  
      
      }
  
  
    ### Take mean of final holdout set folds
  
    dataset_blend_test[,j] = rowMeans(dataset_blend_test_j)

    }

    




print ("Blending....")

# response can be a df and instead of data.frame cbind could be used

mod <- gbm(y ~ X1 + X2 + X3 + X1.1 + X2.1, data = data.frame( response, train, dataset_blend_train), 
           
           n.trees=1000, interaction.depth=8, train.fraction = 0.8)



y_submission = predict(mod, data.frame(test,dataset_blend_test))


### We now have a new dataset with dimensions (N_train X N_models)

### Fit a logistic regression and predict on blended holdout set


print ("Blending....")


logit <- glm(y ~ X1 + X2, data = data.frame(response,dataset_blend_train), family = binomial(logit))


y_submission = predict(logit, data.frame(dataset_blend_test), type="response")


print ("Linear stretch of predictions to [0,1]")

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

y_submission <- range01(y_submission)


# write submission code and export it to csv