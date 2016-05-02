require(xgboost); require(readr); require(caret); require(doParallel)

train <- read_csv("C:/Users/amulya/Documents/Kaggle/Walmart/train.csv")

test <- read_csv("C:/Users/amulya/Documents/Kaggle/Walmart/test.csv")

feature.names <- names(train)[!names(train) %in% c("TripType")]

response <- names(train)[1]

response <- train[ , response] ; response <- as.numeric(response)

class_old <- sort(unique(response))

class_new <- seq(0, 37)

#replace elements of class_old with elements of class_new in response

for( i in 1:38 ){
  
  train$TripType[train$TripType == class_old[i]] <- class_new[i]
  
}

table(train$TripType)

#very basic stuff--------------------------------------------------------------------------------

cat("assuming text variables are categorical & replacing them with numeric ids\n")

for (f in feature.names) {
  
  if (class(train[[f]])=="character") {
    
    levels <- unique(c(train[[f]], test[[f]]))
    
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

train[is.na(train)] <- 0

test[is.na(test)] <- 0

tra <- train[, feature.names]

split <- createDataPartition(y = train$TripType, p = 0.9, list = F) 

response_val <- train$TripType[-split]

response_train <- train$TripType[split]

dval <- xgb.DMatrix(data=data.matrix(tra[-split,]),  label = response_val )

dtrain <- xgb.DMatrix(data=data.matrix(tra[split,]), label = response_train)

watchlist <- list(val=dval, train=dtrain)

#basic training----------------------------------------------------------------------------------

numberOfClasses <- max(train$TripType) + 1

param <- list(objective = "multi:softprob",
              
              eval_metric = "mlogloss",
              
              num_class = numberOfClasses,
              
              nthreads = 4)
gc()

cl <- makeCluster(4); registerDoParallel(cl)

start <- Sys.time()

clf <- xgb.train(params = param, data = dtrain, nrounds = 50, watchlist = watchlist,
                 verbose = 1, maximize = T)

Time_Taken <- Sys.time() - start

##after 900 rounds it increases then falls off hd check it`s behaviour further

pred <- predict(clf, data.matrix(test[, feature.names])) 

pred <- matrix(pred, nrow=38, ncol=length(pred)/38) #there are total 38 classes 

pred = data.frame(t(pred))

sample <- read_csv('sample_submission.csv') 

cnames <- names(sample)[2:ncol(sample)] 

names(pred) <- cnames

submission <- cbind.data.frame(VisitNumber = test$VisitNumber, pred) 

#assuming you consolidated the data by visit number 

write_csv(sub, "1112015.csv")

##------------------------------------------------------------------------------------------------------------
cv.nround <- 5

cv.nfold <- 3

bst.cv = xgb.cv(param=param, data = dtrain, label = train$TripType, 
                
                nfold = cv.nfold, nrounds = cv.nround)

