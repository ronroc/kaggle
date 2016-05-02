
require(data.table); require(lubridate); require(caret); require(sqldf); 

require(xgboost); require(xlsx); require(dplyr); require(readr); 

require(doParallel)


#rm(list = ls())


# Data input-----------------------------------------------------------------------------------------

train_raw <- fread(input = "D:\\kaggle\\HOMESITE\\Data\\train.csv", data.table = F)

response <- train_raw$QuoteConversion_Flag


train_raw$QuoteConversion_Flag <- NULL

train_raw$QuoteNumber <- NULL


test_raw <- fread(input = "D:\\kaggle\\HOMESITE\\Data\\test.csv", data.table = F)

id <- test_raw$QuoteNumber


test_raw$QuoteNumber <- NULL

continous_vars <- c("Field8", "Field9", "Field11", "SalesField8", "Original_Quote_Date", "date")


# categorical and discrete are grouped into a single group

categorical_vars <- c()

remove_vars <- c("PropertyField6", "GeographicField10A")

tmp <- rbind(train_raw, test_raw)

tmp <- tmp[, !(names(tmp) %in% remove_vars)]

require(lubridate)

tmp$Original_Quote_Date <- as.Date(tmp$Original_Quote_Date)

tmp$month <- as.integer(format(tmp$Original_Quote_Date, "%m"))

tmp$year <- as.integer(format(tmp$Original_Quote_Date, "%y"))

tmp$day <- weekdays(as.Date(tmp$Original_Quote_Date))

tmp$week <- week((as.Date(tmp$Original_Quote_Date)))

tmp$date <- (((tmp$year * 52 ) + tmp$week) %% 4)


# dummify character column ------------------------------------------------------------------


a <- lapply(tmp, function(x) length(unique(x)))

len_unique <- rep(0, ncol(tmp))



tmp[is.na(tmp)] <- -1


row_NA <- apply(tmp, 1, function(x) sum(x == -1))

row_zero <- apply(tmp, 1, function(x) sum(x == 0))


tmp$row_NA <- row_NA

tmp$row_zero <- row_zero



# seperate character columns


char <- rep(0, length(names(tmp)))


for(i in names(tmp))
  
{
  if(class(tmp[, i]) == "character"){
    
    char <- c(char, i)
  }
  
  char <- char[char != 0 ]
}


# convert char columns to factors to dummify them


tmp_char <- tmp[, char]


# tmp_char <- tmp_unique


rm(tmp_unique)


for(f in names(tmp_char)){
  
  levels <- unique(tmp_char[, f])
  
  tmp_char[,f] <- factor(tmp_char[,f], levels = levels)
  
}



dummies <- dummyVars( ~., data = tmp_char)


tmp_char <- predict(dummies, newdata = tmp_char)


tmp_char <- data.frame(tmp_char)


rm(dummies)


gc()



for (f in names(tmp)) {
  
  if (class(tmp[[f]])=="character") {
    
    levels <- unique(tmp[[f]])
    
    tmp[[f]] <- as.integer(factor(tmp[[f]], levels=levels))
    
  }
  
}



# create high_card variable name-----------------------------------------------------------------------


a <- lapply(tmp, function(x) length(unique(x)))


high_card <- rep(0, ncol(tmp))


for(i in 1:length(a))
  
{
  
  if(a[[i]] > 30) {
    
    high_card[i] <- (names(a[i]))
    
  }
  
}


high_card <- high_card[high_card != 0]

high_card <- high_card[!(high_card %in% continous_vars)]


tmp_high_card <- tmp[, high_card]


# high_card <- c("PersonalField16", "PersonalField17", "PersonalField14", "PersonalField18", 

#               "PersonalField19" )



str(tmp_high_card, list.len = 999)


cat("assuming text variables are categorical & replacing them with numeric ids\n")



for (f in names(tmp_high_card)) {
  
  if (class(tmp_high_card[[f]])=="character") {
    
    levels <- unique(c(tmp_high_card[[f]]))
    
    tmp_high_card[[f]] <- as.integer(factor(tmp_high_card[[f]], levels=levels))
    
    
  }
  
}



# oof predictions as a replacement for high card vars-----------------------------------------------------

require(h2o)

localH2O <- h2o.init(max_mem_size = "10g")

# train_target <- as.factor(response)



skf = createFolds(response, k = 5 , list = TRUE, returnTrain = TRUE)


### Pre-allocate the data ; For each model, add a column with N rows for each model


dataset_blend_train = matrix(0, nrow(tmp_high_card), 1)


train_high_card <- tmp_high_card[c(1:260753), ]

test_high_card <- tmp_high_card[c(260754:434589), ]


### Loop over the models

### Loop over the folds


i <- 0

for (sk in skf) {
  
  i <- i + 1
  
  print(paste("Fold", i))
  
  
  ### Extract and fit the train/test section for each fold
  
  tmp_train <- unlist(skf[i])
  
  x_train = train_high_card[-tmp_train,]
  
  y_train = response[-tmp_train]
  
  x_test  = test_high_card[tmp_train,]
  
  y_test  = response[tmp_train]

  
  # combining them in a df for maintaining format
  
  x_train$target <- y_train
  
  train.hex <- as.h2o(localH2O, object = x_train)
  
  test.hex <- as.h2o(localH2O, object = x_test)
  
  
  myX <- high_card
  
  myY <- "target"
  
  
  test_glm <- h2o.glm( x = myX,
                       
                       y = myY,
                       
                       training_frame = train.hex,
                       
                       family = "binomial",
                       
                       model_id = "glm_test",
                       
                       alpha = 0, 
                       
                       standardize = T 
                       
                       )
  
  
  
  
  pred_glm <- h2o.predict(object = test_glm, newdata = test.hex)
  
  pred_glm <- as.data.frame(pred_glm)
  
  dataset_blend_train[tmp_train, 1] <- pred_glm$p0
  
  
}


# box - cox of continous variables---------------------------------------------------------------------


tmp_cont <- tmp[, continous_vars]


str(tmp_cont)

# look for binomial transformation of features

# square and cubic transforms working well

tmp_cont$Original_Quote_Date <- NULL

tmp_pre <-  preProcess(tmp_cont, method = ("BoxCox"))

tmp_cont_new <- predict(tmp_pre, tmp_cont)

View(tmp_cont_new); View(tmp_cont)

gc()



# combine oof and high_card dataframes--------------------------------------------------------------------


tmp <- tmp[, !(names(tmp) %in% c(continous_vars, high_card))]

high_card_oof <- data.frame(dataset_blend_train)

tmp_new <- cbind(tmp, tmp_char, tmp_cont_new, high_card_oof)


rm(list = c("test_raw",  "train_raw", "tmp_char", "nms", "tmp_count", "tmp_factors", "tmp_cont_new",

            
            "tmp_cont", "tmp1"))


 


# add positive interaction terms-------------------------------------------------------------------------


imp <- read_csv("D:\\kaggle\\HOMESITE\\FEATURE_IMP\\12062015_1.csv")

top_50 <- imp$Feature[1:5]

tmp_int <- tmp_new[, top_50]

for (f in top_50) {
  
  if (class(tmp_int[[f]])=="character") {
    
    levels <- unique(tmp_int[[f]])
    
    tmp_int[[f]] <- as.integer(factor(tmp_int[[f]], levels=levels))
  }
  
}


gc()

rm(imp);


# plus interaction


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


tmp_new <- cbind(tmp_new, tmp_int)

rm(tmp_int)

gc()


# add negative interaction features----------------------------------------------------------------------


imp <- read_csv("D:\\kaggle\\HOMESITE\\FEATURE_IMP\\12062015_1.csv")

top_50 <- imp$Feature[1:5]

tmp_int <- tmp_new[, top_50]

for (f in top_50) {
  
  if (class(tmp_int[[f]])=="character") {
    
    levels <- unique(tmp_int[[f]])
    
    tmp_int[[f]] <- as.integer(factor(tmp_int[[f]], levels=levels))
  }
  
}


gc()

rm(imp);



for (i in 1:ncol(tmp_int)) {
  
  for (j in (i + 1) : (ncol(tmp_int) + 1)) {
    
    var.x <- colnames(tmp_int)[i]
    
    var.y <- colnames(tmp_int)[j]
    
    var.new <- paste0(var.x, '_minus_', var.y)
    
    tmp_int[ , paste0(var.new)] <- tmp_int[, i] - tmp_int[, j]
    
    }
  
}


gc()


tmp_new <- cbind(tmp_new, tmp_int)

rm(tmp_int)


gc()



# add multipication interaction features-----------------------------------------------------------------


imp <- read_csv("D:\\kaggle\\HOMESITE\\FEATURE_IMP\\12062015_1.csv")

top_50 <- imp$Feature[1:5]

tmp_int <- tmp_new[, top_50]

for (f in top_50) {
  
  if (class(tmp_int[[f]])=="character") {
    
    levels <- unique(tmp_int[[f]])
    
    tmp_int[[f]] <- as.integer(factor(tmp_int[[f]], levels=levels))
    
    }
  
}



gc()

rm(imp);



for (i in 1:ncol(tmp_int)) {
  
  for (j in (i + 1) : (ncol(tmp_int) + 1)) {
    
    var.x <- colnames(tmp_int)[i]
    
    var.y <- colnames(tmp_int)[j]
    
    var.new <- paste0(var.x, '_mult_', var.y)
    
    tmp_int[ , paste0(var.new)] <- tmp_int[, i] * tmp_int[, j]
    
    }
  
}


tmp_new <- cbind(tmp_new, tmp_int)

rm(tmp_int)

gc()


# check for NA terms-------------------------------------------------------------------------------------


a <- lapply(tmp_int, function(x) sum(is.na(x)))


len_unique <- rep(0, ncol(tmp_int))


for(i in 1:length(a))
  
{
  if(a[[i]]  != 0) {
    
    len_unique[i] <- (names(a[i]))
  }
  
}


len_unique <- len_unique[len_unique != 0]




tmp_new <- cbind(tmp_new, tmp_int)

rm(tmp_int)

gc()



# add unique columns of top 5 imp features--------------------------------------------------------------


tmp_new <- tmp_new[, !(names(tmp_new) %in% top_50)]

imp <- read_csv("D:\\kaggle\\HOMESITE\\FEATURE_IMP\\12062015_1.csv")

top_50 <- imp$Feature[1:5]

tmp_int <- tmp[, top_50]


for (f in top_50) {
  
  if (class(tmp_int[[f]])=="character") {
    
    levels <- unique(tmp_int[[f]])
    
    tmp_int[[f]] <- as.integer(factor(tmp_int[[f]], levels=levels))
    
  }
  
}



tmp_new <- cbind(tmp_new, tmp_int)


# create train and test----------------------------------------------------------------------------------

rm(list = c("tmp_int", "tmp", "test_raw", "train_raw", "tmp_char", "imp"))


train <- tmp_new[c(1:260753), ]

test <- tmp_new[c(260754:434589), ]

rm(tmp_new)


gc()

#train[is.na(train)] <- -1

#test[is.na(test)] <- -1


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

# saving the train and oof test set set for further ensembling--------------------------------------------

write_csv(as.data.frame(dataset_blend_test), "dataset_blend_test.csv")

write_csv(as.data.frame(dataset_blend_train), "dataset_blend_train.csv")


# use dataset_blend_train to create a second level model-----------------------------------------------------------------

train <- cbind(train, data.frame(dataset_blend_train))

test <- cbind(test, data.frame(dataset_blend_train))


# train a second level using xgboost-------------------------------------------------------------




    