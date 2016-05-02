
# start after creating train and test datasets----------------------------------------------------------

train$target <- response

# creating 40% holdout

split <- createDataPartition(y = train$target, p = 0.6, list = F)


training <- train[split, ]; training$target <- NULL

response_tr <- response[split]

holdout <- train[-split, ]; holdout$target <- NULL

response_hol <- response[-split]

h <- sample(nrow(training), 10000)

dval<-xgb.DMatrix(data=data.matrix(training[h,]),label=response_tr[h])

dtrain<-xgb.DMatrix(data=data.matrix(training[-h,]),label=response_tr[-h])

watchlist<-list(val=dval,train=dtrain)

for( depth in c(8, 9, 10))

{

  param <- list(
    
    # general , non specific params - just guessing
    
    "objective"  = "binary:logistic"
    
    , "eval_metric" = "logloss"
    
    , "eta" = 0.01
    
    , "subsample" = 1
    
    , "colsample_bytree" = 0.2
    
    , "min_child_weight" = 5
    
    , "max_depth" = depth
  )
  
  
  start <- Sys.time()
  
  cl <- makeCluster(4); registerDoParallel(cl)
  
  set.seed(1*14*16)
  
  # train----------------------------------------------------------------------------------------------
  
  clf <- xgb.train(   params              = param,
                      
                      data                = dtrain,
                      
                      nrounds             = 10000,
                      
                      verbose             = 1,  #1
                      
                      early.stop.round    = 1500,
                      
                      watchlist           = watchlist,
                      
                      maximize            = F,
                      
                      nthread = 4
                      
  )
  
  
  # local validation------------------------------------------------------------------------------------
  
  pred <- predict(clf, data.matrix(holdout[,feature.names]))
  
  # LogLoss Function
  
  LogLoss <- function(actual, predicted, eps=0.00001) {
    
    predicted <- pmin(pmax(predicted, eps), 1-eps)
    
    -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
    
  }
  
  val = LogLoss(response_hol, pred)

  eval <- c()
  
  eval <- c(eval, val)
    
  print(paste("the value of LogLoss on 40% holdout for value of", depth, "is", val ))
  
  
}