library(caret)

library(readr)

library(xgboost)

train <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

test <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")

feature.names <- names(train)[2:(ncol(train)-1)]


for (f in feature.names) {
  
  if (class(train[[f]])=="character") {
    
    levels <- unique(c(train[[f]], test[[f]]))
    
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
    
  }
}

train[is.na(train)] <- -1

test[is.na(test)]   <- -1

feature_1 <- benchmark$target[1:145231] 

train$feature1 <- feature_1

test$feature1 <- benchmark$target

split <- createDataPartition(train$target, p = .75, list = FALSE)

response <- train$target

train$target <- NULL

feature.names <- names(train)[2:(ncol(train))]

training <- train[ split,]

testing  <- train[-split,]

response_testing <-  response[-split]

response_training <- response[split]

dtrain <- xgb.DMatrix(data.matrix(training[,feature.names]), label= response_training)

dval <- xgb.DMatrix(data.matrix(testing[,feature.names]), label= response_testing)

watchlist <- list(train = dtrain, test = dval)

param <- list(  objective   = "binary:logistic", 

                eta                 = 0.014,
                
                max_depth           = 10,
                
                subsample           = 0.7,
                
                colsample_bytree    = 0.7,
                
                eval_metric         = "auc"
)


xgb.cv(params = param, dtrain)



clf_first <- xgb.train( params = param, 
                        
                        data                = dtrain, 
                        
                        nrounds             = 520, # changed from 300
                        
                        verbose             = 2, 
                        
                        watchlist = watchlist,
                        
                        nthread = 2,
                        
                        maximize = TRUE)

submission_second <- data.frame(ID=test$ID)

submission_second$target <- NA 

submission_second[,"target"] <- predict(clf_first, data.matrix(test[,feature.names]))

write_csv(submission_second, "second.csv")

xgb.dump(model = clf_first, "second.txt", with.stats = T)

xgb.save(clf_first, "xgb_second.R")


































train <- train[sample(nrow(train), 80000),]

h <- sample(nrow(train), 40000)
train <-train[h,]
gc()

cat("Making train and validation matrices\n")

dtrain <- xgb.DMatrix(data.matrix(train[,feature.names]), label=train$target)

val<-train[-h,]
gc()

dval <- xgb.DMatrix(data.matrix(val[,feature.names]), label=val$target)

watchlist <- watchlist <- list(eval = dval, train = dtrain)

param <- list(  objective           = "binary:logistic", 
                # booster = "gblinear",
                eta                 = 0.001,
                max_depth           = 13,  # changed from default of 6
                subsample           = 0.6,
                colsample_bytree    = 0.75,
                eval_metric         = "auc"
                # alpha = 0.0001, 
                # lambda = 1
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 50, # changed from 300
                    verbose             = 2, 
                    early.stop.round    = 10,
                    watchlist           = watchlist,
                    maximize            = TRUE)

cat("making predictions in batches due to 8GB memory limitation\n")

submission <- data.frame(ID=test$ID)
submission$target <- NA 
for (rows in split(1:nrow(test), ceiling((1:nrow(test))/10000))) {
  submission[rows, "target"] <- predict(clf, data.matrix(test[rows,feature.names]))
  
  # submission2 <- data.frame(ID=test$ID)
  # submission2$target2 <- NA 
  # for (rows in split(1:nrow(test), ceiling((1:nrow(test))/10000))) {
  #     submission2[rows, "target2"] <- predict(clf, data.matrix(test[rows,feature.names]))
  
}


# submission <- merge(submission, submission2, by = "ID", all.x = TRUE)
# submission$target <- submission$target1*0.4 + submission$target2*0.6
# submission <- submission[,c(1,4)]

cat("saving the submission file\n")
write_csv(submission, "xgb_b6.csv")