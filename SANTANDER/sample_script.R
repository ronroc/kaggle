x <- c("xgboost", "Matrix", "data.table", "bit64", "doParallel", "caret")

lapply(x, require, character.only = T)

set.seed(03*25*2016)

train <- fread("D:\\kaggle\\SANTANDER\\DATA\\train.csv", data.table = F)

test <- fread("D:\\kaggle\\SANTANDER\\DATA\\test.csv", data.table = F)

## removing IDs---------------------------------------------------------------------------------

train$ID <- NULL

ID <- test$ID

test$ID <- NULL

## extracting TARGET---------------------------------------------------------------------------

response <- train$TARGET

train$TARGET <- NULL

## zero count per line-------------------------------------------------------------------------

count0 <- function(x) {

    return( sum(x == 0) )
}

train$n0 <- apply(train, 1, FUN=count0)

test$n0 <- apply(test, 1, FUN=count0)


## removing constant features------------------------------------------------------------------

cat("\n## Removing the constants features. ##\n")

cat("assuming text variables are categorical & replacing them with numeric ids\n")

feature.names <- names(train)

for(f in feature.names) {
  
  if(class(train[[f]]) == "character") {
    
    levels <- unique(c(train[[f]], test[[f]]))
    
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}


for(f in names(train)) {
  
  if (length(unique(train[[f]])) == 1) {
    
    cat(f, "is constant in train. We delete it.\n")
    
    train[[f]] <- NULL
    
    test[[f]] <- NULL
  }
}


train <- train[, feature.names]

test <- test[, feature.names]

#train$TARGET <- response


#train <- sparse.model.matrix(TARGET ~ ., data = train)

dtrain <- xgb.DMatrix(data=data.matrix(train), label=response, missing = NaN)

watchlist <- list(train=dtrain)

cl <- makeCluster(4); registerDoParallel(cl) 

param <- list(  objective           = "binary:logistic", 
                
                booster             = "gbtree",
                
                eval_metric         = "auc",
                
                eta                 = 0.02,
                
                max_depth           = 5,
                
                subsample           = 0.7,
                
                colsample_bytree    = 0.7
)

clf <- xgb.train(   params              = param, 
                    
                    data                = dtrain, 
                    
                    nrounds             = 1000, 
                    
                    verbose             = 2,
                    
                   watchlist           = watchlist,
                    
                    maximize            = FALSE
)

# 
# cv <- xgb.cv(params = param, 
#        
#        data = dtrain, 
#        
#        nrounds = 500, 
#        
#        nfold = 10, 
#        
#        prediction = T, 
#        
#        showsd = T, 
#        
#        maximize = T,
#        
#        nthread = 4)
# 

test$TARGET <- -1

#test <- sparse.model.matrix(TARGET ~ ., data = test)

preds <- predict(clf, data.matrix(test), missing = NaN)

submission <- data.frame(ID=ID, TARGET=preds)

cat("saving the submission file\n")

write.csv(submission, "D:\\kaggle\\SANTANDER\\SUBMISSION\\submission_1.csv", row.names = F)

# importance matrix--------------------------------------------------------------------------

importance_matrix <- xgb.importance(feature_names = names(train), model = clf)

jpeg(filename = "D:\\kaggle\\SANTANDER\\Plots\\imp_03252016.jpeg")

xgb.plot.importance(importance_matrix = importance_matrix[1:20, ])

dev.off()
  