
dummy_var = names(tmp_raw)[c(-34)]

tmp_dummy <- tmp_raw[ , dummy_var]

train <- tmp_dummy[c(1:260753), ]

test <- tmp_dummy[c(260754:434589), ]

train[is.na(train)] <- -1

test[is.na(test)] <- -1

gc()

len = length(names(train))

for(i in 1:len){
  
  print(paste0(( i / len) * 100, "%"))
  
  levels <- unique(train[[i]])
  
  train[, i] <- factor(train[, i], levels = levels)
  
}

# memory issues


dummies <- dummyVars( ~ ., data = train)

head(predict(dummies, newdata = train))

############################################################################################


# check function

testing <- train[ 1:nrow(train), ]

testing <- (model.matrix(~  .  , data = testing, 
                         
            contrasts.arg = lapply(testing, contrasts, contrasts=FALSE, sparse=TRUE)))

testing <- data.frame(testing)

testing[1:6, 2:7]


###########################################################################################


# check one way count

testing <- train[ 1:6, 20:24]

#tmp_count <- data.frame(id = 1:nrow(testing))

len = dim(testing)[2]

for(i in 1:len){
  
  print((i / len) * 100 )
  
  testing$x <- testing[, i]
  
  sum1 <- sqldf("select x, count(1) as cnt
                
                from testing  group by 1 ")
  
  tmp1 <- sqldf("select cnt from testing a left join sum1 b on a.x=b.x")
  
  testing[, paste(names(testing)[i], "_one", sep="")] <- tmp1$cnt
  
  testing$x <- NULL
}  

########################################################################################


matrix_train <- (model.matrix(~  .  , data = train, 
                              
                              contrasts.arg = lapply(train, contrasts, contrasts=FALSE)))


matrix_test <- (model.matrix(~  . -1 , data = test, 
                              
                              contrasts.arg = lapply(train, contrasts, contrasts=FALSE)))


feature.names <- names(train)

h <- sample(nrow(train),2000)

dval <- xgb.DMatrix(data=data.matrix(train[h,]),label=response[h])

#dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,]),label=train$QuoteConversion_Flag[-h])

dtrain <- xgb.DMatrix(data = matrix_train,label=response)

watchlist <- list(val=dval,train=dtrain)

param <- list(objective           = "binary:logistic",
              
              booster = "gbtree",
              
              eval_metric = "auc",
              
              eta = 0.02, # 0.06, #0.01,
              
              max_depth = 7, #changed from default of 8
              
              subsample = 0.82, # 0.7
              
              colsample_bytree = 0.66, # 0.7
              
              num_parallel_tree = 2
              
              # alpha = 0.0001,
              
              # lambda = 1
              
)

cl <- makeCluster(2); registerDoParallel(cl)

set.seed(11*23*15)

start <- Sys.time()

clf <- xgboost( params              = param,
                
                data                = matrix_train, 
                
                label               = response,
                
                nrounds             = 1800,
                
                verbose             = 1,  #1
                
                #early.stop.round    = 150,
                
                #watchlist           = watchlist,
                
                maximize            = FALSE,
                
                nthread = 2)

pred <- predict(clf, data.matrix(test[,feature.names]))

submission <- data.frame(QuoteNumber = id, QuoteConversion_Flag = pred)

write_csv(submission, "D:\\kaggle\\HOMESITE\\submission\\11232015.csv")

time_taken <- Sys.time() - start
