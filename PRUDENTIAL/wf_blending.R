

require(data.table); require(xgboost)

rm(list = ls())

train <- fread("D:\\kaggle\\PRUDENTIAL\\Data\\train.csv", data.table = F)

test  <- fread("D:\\kaggle\\PRUDENTIAL\\Data\\test.csv", data.table = F)


train$Id <- NULL; 

id <- test$Id; test$Id <- NULL

response <- train$Response; train$Response <- NULL

tmp <- rbind(train, test)

tmp[is.na(tmp)] <- -1

row_NA <- apply(tmp, 1, function(x) sum(x == -1))

feature.names <- names(train)

for (f in feature.names) {
  
  if (class(train[[f]])=="character") {
    
    levels <- unique(c(train[[f]], test[[f]]))
    
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}


cat_var <- c(paste("Product_Info_", c(1:3,5:7), sep=""), paste("Employment_Info_", c(2,3,5), sep=""),
             
             paste("InsuredInfo_", 1:7, sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""), 
             
             "Family_Hist_1", paste("Medical_History_", c(2:9, 11:14, 16:23, 25:31, 33:41), sep=""))



cont_var <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", 
              
              "Employment_Info_6", "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", 
              
              "Family_Hist_5")




disc_var <- c("Medical_History_1", "Medical_History_15", "Medical_History_24", "Medical_History_32", 
              
              "Medical_History_10")




tmp_cat <- tmp[, cat_var]

tmp_cont <- tmp[, cont_var]

tmp_disc <- tmp[, disc_var]


tmp_dummy <- cbind(tmp_cat, tmp_disc)

# calculate approx number of rows

len_dummy <- lapply(tmp_dummy, function(x) length(unique(x)))


len = length(names(tmp_dummy))


for (i in 1:len) {
  
  print(paste0( i / (len) *100, "%"))
  
  tmp_dummy[ , i] <- as.factor(tmp_dummy[ , i])
  
}



str(tmp_dummy, list.len = 999)

require(caret)

dummies <- dummyVars( ~ ., data = tmp_dummy)


gc()


tmp_dummy <- predict(dummies, newdata = tmp_dummy)

tmp_dummy <- data.frame(tmp_dummy)

dim(tmp_dummy)



require(caret)

tmp_pre <-  preProcess(tmp_cont, method = ("BoxCox"))

tmp_cont_new <- predict(tmp_pre, tmp_cont)


tmp$row_NA <- row_NA


tmp_new <- cbind(tmp, tmp_dummy, tmp_cont)



train <- tmp_new[c(1:59381),]

test <- tmp_new[c(59382:79146),]

gc()

sum(is.na(train))

sum(is.na(test))

##########################################################################################

### Returns train inidices for n_folds using StratifiedKFold

skf = createFolds(response, k = 5 , list = TRUE, returnTrain = TRUE)

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
    
      x_train = train[-tmp_train, ]
    
      y_train = response[-tmp_train]
    
      x_test  = train[tmp_train,]
    
      y_test  = response[tmp_train]
    
    
      # y_test could be used to calculate metric
      
      ### Stupid hack to fit the model
    
      if (clf == "xgboost_1") {
        
        feature.names <- names(train)
        
        dtrain <- xgb.DMatrix(data=data.matrix(x_train[, feature.names]),label=y_train, missing = NaN )
        
        param <- list(  print.every.n       = 20,
                        
                        objective           = "reg:linear",
                        
                        depth = 21,
                        
                        min_child_weight = 3,
                        
                        subsample = 0.71,
                        
                        eta = 0.01
        )
        
        
        
        start <- Sys.time()
        
        require(doParallel)
        
        cl <- makeCluster(2); registerDoParallel(cl)
        
        set.seed(1*2*16)
        
        mod <- xgb.train(   params              = param,
                            
                            booster = "gbtree",
                            
                            data                = dtrain,
                            
                            nrounds             = 4000,
                            
                            verbose             = 1,  #1
                            
                            #early.stop.round    = 150,
                            
                            #watchlist           = watchlist,
                            
                            maximize            = T,
                            
                            nthread = 2)
        
        dataset_blend_train[tmp_train, j] <- predict(mod, data.matrix(x_test), missing = NaN)
        
      }
        
    
      else if (clf == "xgboost_2") {
        
        feature.names <- names(train)
        
        dtrain <- xgb.DMatrix(data=data.matrix(x_train),label=y_train, missing = NaN )
        
        param <- list(  print.every.n       = 20,
                        
                        objective           = "count:poisson",
                        
                        depth = 21,
                        
                        min_child_weight = 3,
                        
                        subsample = 0.71,
                        
                        eta = 0.01
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
        
        dataset_blend_train[tmp_train, j] <- predict(mod, data.matrix(x_test), missing = NaN)
        
      }
      
    
      ### Predict the probability of current folds test set and store results.
    
      ### This output will be the basis for our blended classifier to train against,
    
      ### which is also the output of our classifiers
    
      #dataset_blend_train[tmp_train, j] <- predict(mod, data.matrix(x_test))
    
    
      ### Predict the probabilty for the holdout set and store results
    
      if(clf == "xgboost_1")
      
      {
      
        dataset_blend_test_j[, i] <- predict(mod, data.matrix(test), missing = NaN)
      
      }
      
      else if(clf == "xgboost_2")
        
        {
        
        dataset_blend_test_j[, i] <- predict(mod, data.matrix(test), missing = NaN)
        
      
        }
  
      
      }
  
  
    ### Take mean of final holdout set folds
  
    dataset_blend_test[,j] = rowMeans(dataset_blend_test_j)

    }

    




print ("Blending....")
dtrain <- xgb.DMatrix(data=data.matrix(data.frame(train, dataset_blend_train)),label=response, missing = NaN )

param <- list(  print.every.n       = 20,
                
                objective           = "reg:linear",
                
                depth = 21,
                
                min_child_weight = 3,
                
                subsample = 0.71,
                
                eta = 0.01
)



start <- Sys.time()

require(doParallel)

cl <- makeCluster(2); registerDoParallel(cl)

set.seed(1*2*16)

mod <- xgb.train(   params              = param,
                    
                    booster = "gbtree",
                    
                    data                = dtrain,
                    
                    nrounds             = 4000,
                    
                    verbose             = 1,  #1
                    
                    #early.stop.round    = 150,
                    
                    #watchlist           = watchlist,
                    
                    maximize            = T,
                    
                    nthread = 2)



predict = predict(mod, data.matrix(data.frame(train,dataset_blend_train)), missing = NaN)

predict_test = predict(mod, data.matrix(data.frame(test,dataset_blend_test)), missing = NaN)

SQWKfun = function(x = seq(1.5, 7.5, by = 1), pred) {
  preds = pred$predict
  true = pred$Response
  cuts = c(min(preds), x[1], x[2], x[3], x[4], x[5], x[6], x[7], max(preds))
  preds = as.numeric(Hmisc::cut2(preds, cuts))
  err = Metrics::ScoreQuadraticWeightedKappa(preds, true, 1, 8)
  return(-err)
}

pred = data.frame(Id=train_id, Response=response, predict=predict)

optCuts = optim(seq(1.5, 7.5, by = 1), SQWKfun, pred = pred)

print(optCuts)


preds = as.numeric(Hmisc::cut2(predict_test, c(-Inf, optCuts$par, Inf)))


submission <- data.frame(Id=id, Response=preds)

require(readr)

write_csv(submission, "01092016_4.csv")
