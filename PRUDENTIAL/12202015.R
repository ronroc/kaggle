library(readr); library(xgboost); require(data.table)

#rm(list = ls())

# Set a random seed for reproducibility

set.seed(12212015)

train <- fread("D:\\kaggle\\PRUDENTIAL\\Data\\train.csv", data.table = F)

test  <- read_csv("D:\\kaggle\\PRUDENTIAL\\Data\\test.csv")

feature.names <- names(train)[2:ncol(train)-1]

cat("assuming text variables are categorical & replacing them with numeric ids\n")

for (f in feature.names) {
  
  if (class(train[[f]])=="character") {
    
    levels <- unique(c(train[[f]], test[[f]]))
    
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

response <- train$Response

train$Response <- NULL

tmp <- rbind(train, test)

tmp[is.na(tmp)] <- -1

train <- tmp[c(1:59381),]

test <- tmp[c(59382:79146),]


cat("training a XGBoost classifier\n")

evalerror <- function(preds, dtrain) {
  
  labels <- getinfo(dtrain, "label")
  
  err <- ScoreQuadraticWeightedKappa(labels,round(preds))
  
  return(list(metric = "kappa", value = err))
  
}


xgbtrain <- xgb.DMatrix(data.matrix(train[, -c(1)]) , label = response )


clf <- xgb.train(data        = xgbtrain,
                 
                 eta         = 0.02,
                 
                 depth       = 21,
                 
                 nrounds     = 4000,
                 
                 objective   = "reg:linear",
                 
                 colsample_bytree=0.65,
                 
                 min_child_weight=3,
                 
                 subsample=0.7,
                 
                 feval = evalerror
                 )


########################################################################################################


xgbtrain <- xgb.DMatrix(data.matrix(train[, -c(1)]) , label = response )

ScoreQuadraticWeightedKappa = function (rater.a , rater.b, 
                                        min.rating,
                                        max.rating) {
  
  if (missing(min.rating)) {
    min.rating = min(min(rater.a),min(rater.b))
  }
  if (missing(max.rating)) {
    max.rating = max(max(rater.a),max(rater.b))
  }
  
  rater.a = factor(rater.a, levels=min.rating:max.rating)
  rater.b = factor(rater.b, levels=min.rating:max.rating)
  
  #pairwise frequencies
  confusion.mat = table(data.frame(rater.a, rater.b))
  confusion.mat = confusion.mat / sum(confusion.mat)
  
  #get expected pairwise frequencies under independence
  histogram.a = table(rater.a) / length(table(rater.a))
  histogram.b = table(rater.b) / length(table(rater.b))
  expected.mat = histogram.a %*% t(histogram.b)
  expected.mat = expected.mat / sum(expected.mat)
  
  #get weights
  labels = as.numeric( as.vector (names(table(rater.a))))
  weights = outer(labels, labels, FUN = function(x,y) (x-y)^2 )
  
  #calculate kappa
  kappa = 1 - sum(weights*confusion.mat)/sum(weights*expected.mat)
  kappa
}



evalerror <- function(preds, dtrain) {

    labels <- getinfo(dtrain, "label")
  
    err <- ScoreQuadraticWeightedKappa(labels,round(preds))
  
    return(list(metric = "kappa", value = err))
    
    }

cat("training a XGBoost classifier\n")


clf <- xgb.cv(data        = xgbtrain,
              
              eta         = 0.025,
              
              depth       = 21,
              
              nrounds     = 4000,
              
              objective   = "count:poisson",
              
              feval=evalerror,
              
              colsample_bytree=0.7,
              
              min_child_weight=3,
              
              subsample=0.7, 
              
              nfold=5
              
              )

 


cat("making predictions\n")

submission <- data.frame(Id=test$Id)

submission$Response <- as.integer(round(predict(clf_1, data.matrix(test[, -c(1)]))))

# I pretended this was a regression problem and some predictions may be outside the range

submission[submission$Response<1, "Response"] <- 1

submission[submission$Response>8, "Response"] <- 8

submission[submission$Response==3,"Response"] <- 2

cat("saving the submission file\n")

write_csv(submission, "D:\\kaggle\\PRUDENTIAL\\submission\\12212015_2.csv")
