#######################################################################################################

## RIDGE REGRESSION CODE STARTS FROM LINE 138    

#######################################################################################################

set.seed(01122016)

require(data.table); require(xgboost)

rm(list = ls())

train <- fread("D:\\kaggle\\PRUDENTIAL\\Data\\train.csv", data.table = F)

test  <- fread("D:\\kaggle\\PRUDENTIAL\\Data\\test.csv", data.table = F)


train_id <- train$Id

train$Id <- NULL;

 

id <- test$Id; test$Id <- NULL

response <- train$Response; train$Response <- NULL


tmp <- rbind(train, test)

tmp[is.na(tmp)] <- -1


high_card_vars <- c()


cat_var <- c(paste("Product_Info_", c(1:3,5:7), sep=""), paste("Employment_Info_", c(2,3,5), sep=""),
             
             paste("InsuredInfo_", 1:7, sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""), 
             
             "Family_Hist_1", paste("Medical_History_", c(2:9, 11:14, 16:23, 25:31, 33:41), sep=""))



cont_var <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", 
              
              "Employment_Info_6", "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", 
              
              "Family_Hist_5")




disc_var <- c("Medical_History_1", "Medical_History_15", "Medical_History_24", "Medical_History_32", 
              
              paste("Medical_Keyword_", 1:48, sep=""), "Medical_History_10")


tmp_category <- tmp[, c(cat_var, disc_var)]


len <- lapply(tmp_category, function(x) length(unique(x)))



for(i in 1:length(len)){
  
  if(len[[i]] > 38) high_card_vars <- c(names(len)[i], high_card_vars)
}


tmp_high_card_vars <- tmp[, high_card_vars]

# dummifing high card vars high_card_vars


tmp_dummy <- tmp_high_card_vars


len = length(names(tmp_dummy))

for (i in 1:len) {
  
  print(paste0( i / (len) *100, "%"))
  
  tmp_dummy[ , i] <- as.factor(tmp_dummy[ , i])
  
}



str(tmp_dummy, list.len = 999)


dummies <- dummyVars( ~ ., data = tmp_dummy)

gc()

tmp_dummy <- predict(dummies, newdata = tmp_dummy)

tmp_dummy <- data.frame(tmp_dummy)

dim(tmp_dummy)



# row_NA <- apply(tmp, 1, function(x) sum(x == -1))

# tmp$row_NA <- row_NA


feature.names <- names(tmp)

for (f in feature.names) {
  
  if (class(tmp[[f]])=="character") {
    
    levels <- unique(c(tmp[[f]]))
    
    tmp[[f]] <- as.integer(factor(tmp[[f]], levels=levels))
    
  }
}


tmp_new <- cbind(tmp, tmp_dummy)

train <- tmp_new[c(1:59381),]

test <- tmp_new[c(59382:79146),]

gc()



#######################################################################################################

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
  
  

dtrain <- xgb.DMatrix(data=data.matrix(train),label=train)


param <- list(  print.every.n       = 20,
                
                objective           = "reg:linear",
                
                depth = 21,
                
                min_child_weight = 3,
                
                subsample = 0.71,
                
                eta = 0.01,
                
                silent = 0,
                
                colsample_bytree = 0.8

                )



start <- Sys.time()


mod <- xgb.train(   params              = param,
                    
                    booster = "gbtree",
                    
                    data                = dtrain,
                    
                    nrounds             = 4000,
                    
                    verbose             = 1,
                    
                    maximize            = F
                    
                    )


dataset_blend_train[tmp_train, 1] <- predict(mod, data.matrix(x_test))

}

predict_test = predict(mod, data.matrix((test)))

SQWKfun = function(x = seq(1.5, 7.5, by = 1), pred) {
  preds = pred$predict
  true = pred$Response
  cuts = c(min(preds), x[1], x[2], x[3], x[4], x[5], x[6], x[7], max(preds))
  preds = as.numeric(Hmisc::cut2(preds, cuts))
  err = Metrics::ScoreQuadraticWeightedKappa(preds, true, 1, 8)
  return(-err)
}


pred = data.frame(Id=train_id, Response=response, predict=dataset_blend_train)

optCuts = optim(seq(1.5, 7.5, by = 1), SQWKfun, pred = pred)

print(optCuts)


preds = as.numeric(Hmisc::cut2(predict_test, c(-Inf, optCuts$par, Inf)))


submission <- data.frame(Id=id, Response=preds)

require(readr)

write_csv(submission, "01202016_5.csv")

feature_imp <- xgb.importance( model = mod)

feature_imp$Feature <- as.numeric(feature_imp$Feature)

names <- c()

for(i in 1:length(feature_imp$Feature)){
  
  names <- c(names(train)[feature_imp$Feature[i]], names)
}

feature_imp$names <- names

write_csv(feature_imp, "feature_imp_01122016.csv")




#######################################################################################################
#######################################################################################################


set.seed(01*12*2016)

require(data.table); require(xgboost); require(readr)

rm(list = ls())

train <- fread("D:\\kaggle\\PRUDENTIAL\\Data\\train.csv", data.table = F)

test  <- fread("D:\\kaggle\\PRUDENTIAL\\Data\\test.csv", data.table = F)


train_id <- train$Id

train$Id <- NULL;



id <- test$Id; test$Id <- NULL

response <- train$Response; train$Response <- NULL

tmp <- rbind(train, test)

tmp[is.na(tmp)] <- -1

# row_NA <- apply(tmp, 1, function(x) sum(x == -1))

# tmp$row_NA <- row_NA


feature.names <- names(tmp)

for (f in feature.names) {
  
  if (class(tmp[[f]])=="character") {
    
    levels <- unique(c(tmp[[f]]))
    
    tmp[[f]] <- as.integer(factor(tmp[[f]], levels=levels))
    
  }
}


train <- tmp[c(1:59381),]

test <- tmp[c(59382:79146),]

gc()



#######################################################################################################

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
  
  
  
  dtrain <- xgb.DMatrix(data=data.matrix(x_train),label=y_train)
  
  
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
  
  
  dataset_blend_train[tmp_train, 1] <- predict(mod, data.matrix(x_test))
  
}

predict_test = predict(mod, data.matrix((test)))

SQWKfun = function(x = seq(1.5, 7.5, by = 1), pred) {
  preds = pred$predict
  true = pred$Response
  cuts = c(min(preds), x[1], x[2], x[3], x[4], x[5], x[6], x[7], max(preds))
  preds = as.numeric(Hmisc::cut2(preds, cuts))
  err = Metrics::ScoreQuadraticWeightedKappa(preds, true, 1, 8)
  return(-err)
}


pred = data.frame(Id=train_id, Response=response, predict=dataset_blend_train)

optCuts = optim(seq(1.5, 7.5, by = 1), SQWKfun, pred = pred)

print(optCuts)


preds = as.numeric(Hmisc::cut2(predict_test, c(-Inf, optCuts$par, Inf)))


submission <- data.frame(Id=id, Response=preds)

require(readr)

write_csv(submission, "01202016_2.csv")

feature_imp <- xgb.importance( model = mod)

feature_imp$Feature <- as.numeric(feature_imp$Feature)

names <- c()

for(i in 1:length(feature_imp$Feature)){
  
  names <- c(names(train)[feature_imp$Feature[i]], names)
}

feature_imp$names <- names

write_csv(feature_imp, "feature_imp_raw_1.csv")



######################################################################################################


# Ridge Regression treatment of high cardinality treatment


# Approach 1: using out of fold predictions of each individual columns as input to XGB

#          2: using out of fold predictions of the entire dataset ( high card vars ) as input to XGB

#          3: using out of fold predictions of the entire datatset(high card vars) as inout to blend



###########################################################################################################


train_high_card <- train[, high_card_vars]


skf = createFolds(response, k = 5 )


dataset_blend_train  = matrix(0, nrow(train), 1)

require(glmnet)

### Loop over the folds

i <- 0

for (sk in skf) {
  
  i <- i + 1
  
  print(paste("Fold", i))
  
  
  ### Extract and fit the train/test section for each fold
  
  tmp_train <- unlist(skf[i])
  
  x_train = train_high_card[-tmp_train, ]
  
  y_train = response[-tmp_train]
  
  x_test  = train_high_card[tmp_train,]
  
  y_test = response[tmp_train]
  
  
  x = data.matrix(x_train)
  
  y = (y_train)
  
  start <- Sys.time()
  
  
  fit <- cv.glmnet(x, y, family = "multinomial", alpha = 0)
  
  x =  data.matrix(x_test)
  
  dataset_blend_train[tmp_train, 1]  <-  a <- predict(fit, x, type = "response", s = "lambda.min")
  
}
