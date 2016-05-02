
# read data-----------------------------------------------------------------------------------------------

require(data.table); require(xgboost); require(caret); require(doParallel); require(readr)

train_raw <- fread("D:\\kaggle\\BNP\\DATA\\train.csv", data.table = F)

test_raw <- fread("D:\\kaggle\\BNP\\DATA\\test.csv", data.table = F)


response <- train_raw$target

id <- test_raw$ID

train_raw$target <- NULL

train_raw$ID <- NULL

test_raw$ID <- NULL

tmp <- rbind(train_raw, test_raw)

feature.names <- names(tmp)

#######################################################################################################

for (f in feature.names) {
  
  if (class(tmp[[f]]) == "character") {
    
    levels <- unique(c(tmp[[f]]))
    
    tmp[[f]] <- as.integer(factor(tmp[[f]], levels=levels))
    
    
  }
}


# count of NA------------------------------------------------------------------------------------------

row_NA <- apply(tmp, 1, function(x) sum(is.na(x)))

tmp$row_NA <- row_NA

tmp[is.na(tmp)] <- -1


  # create train and test sets---------------------------------------------------------------------------- 

# from train create training and hold out---------------------------------------------------------------

# create 20 % validation set-----------------------------------------------------------------------------

# from now on use data from training instead of train--------------------------------------------------


train <- tmp[c(1:nrow(train_raw)), ]

test <- tmp[c((nrow(train_raw) +1) : nrow(tmp)), ]


train$target <- response

split <- createDataPartition(y = train$target, p = 0.8, list = F)


training <- train[split, ]; training$target <- NULL

response_tr <- response[split]

holdout <- train[-split, ]; holdout$target <- NULL

response_hol <- response[-split]

h <- sample(nrow(training), 1000)

dval<-xgb.DMatrix(data=data.matrix(training[h,]),label=response_tr[h])

dtrain<-xgb.DMatrix(data=data.matrix(training[-h,]),label=response_tr[-h])

watchlist<-list(val=dval,train=dtrain)


param <- list(
  
  # general , non specific params - just guessing
  
  "objective"  = "binary:logistic"
  
  , "eval_metric" = "logloss"
  
  , "eta" = 0.01
  
  , "subsample" = 0.8
  
  , "colsample_bytree" = 0.8
  
  , "min_child_weight" = 1
  
  , "max_depth" = 10
)

start <- Sys.time()

cl <- makeCluster(4); registerDoParallel(cl)

set.seed(1*14*16)

# train----------------------------------------------------------------------------------------------

clf <- xgb.train(   params              = param,
                    
                    data                = dtrain,
                    
                    nrounds             = 4000,
                    
                    verbose             = 1,  #1
                    
                    early.stop.round    = 1000,
                    
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

LogLoss(response_hol, pred)

# submission ------------------------------------------------------------------------------------------

pred <- predict(clf, data.matrix(test[,feature.names]))

submission <- data.frame(ID = id, PredictedProb = pred)

write_csv(submission, "D:\\kaggle\\BNP\\submission\\add_na.csv")