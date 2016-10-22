rm(test_raw); rm(train_raw); rm(cl);rm(class_new); rm(class_old); rm(depth); rm(cnames); rm(dtrain); rm(dval)

rm(eta); rm(f); rm(feature.names); rm(i); rm(j); rm(len); rm(levels); rm(nam); rm(name); rm(numberOfClasses)

rm(start); rm(time_taken); rm(watchlist); rm(my.f2cnt); rm(my.f3cnt); rm(tmp_str); rm(tmp_new); rm(lnth)



#feature hashing

train_hash <- train

train_hash[is.na(train_hash)] <- 0

split <- createDataPartition(y = train_hash$TripType, p = 0.9, list = F) 

training <- train_hash[split,]

validation <- train_hash[-split,]

training_hash = hashed.model.matrix(~., data=training[,feature.names],  hash.size=2^16,  
                                    
                                    transpose=FALSE, create.mapping=TRUE, is.dgCMatrix = TRUE)

validation_hash = hashed.model.matrix(~., data=validation[,feature.names],  hash.size=2^16,  
                                      
                                      transpose=FALSE, create.mapping=TRUE, is.dgCMatrix = TRUE)

response_val <- train_hash$TripType[-split]

response_train <- train_hash$TripType[split]

dval <- xgb.DMatrix(data=validation_hash, label = response_val )

dtrain <- xgb.DMatrix(data=training_hash,  label = response_train)

watchlist <- list(val=dval, train=dtrain)

clf <- xgb.train(params = param, data = dtrain, nrounds = 500, watchlist = watchlist,
                 
                 verbose = 1, maximize = T)



#################################################################################################

#normal training method

feature.names <- names(train)[-c(179) ]

tra <- train[, feature.names]

split <- createDataPartition(y = train_raw$TripType, p = 0.9, list = F) 

response_val <- train_raw$TripType[-split]

response_train <- train_raw$TripType[split]

dval <- xgb.DMatrix( data = data.matrix(tra[-split,]),  label = response_val )

dtrain <- xgb.DMatrix( data = data.matrix(tra[split,]), label = response_train)

#sum(is.na(train)); sum(is.na(test)) # test if found NA error

watchlist <- list(val=dval, train=dtrain)

#basic training----------------------------------------------------------------------------------

numberOfClasses <- max(train_raw$TripType) + 1

param <- list(objective = "multi:softprob",
              
              eval_metric = "mlogloss",
              
              num_class = numberOfClasses,
              
              max_depth = 16,
              
              eta = 0.03,
              
              colsample_bytree = 0.8,
              
              subsample = 0.8
              )

gc()


cl <- makeCluster(detectCores()); registerDoParallel(cl)


start <- Sys.time()


#############################################################################################################


clf <- xgb.train(params = param, data = dtrain, nrounds = 100, watchlist = watchlist,
                 
                 verbose = 1, maximize = T, nthread = 2)


time_taken <- Sys.time() - start


#############################################################################################################


#grid search

for (depth in c(10, 15, 20,25)) {
  
  for(eta in c(0.03, 0.02, 0.01)){
    
    # train
    param <- list(objective = "multi:softprob",
                  
                  eval_metric = "mlogloss",
                  
                  num_class = numberOfClasses,
                  
                  max_depth = depth ,
                  
                  eta = eta,
                  
    )
    
    
    clf <- xgb.train(params = param, data = dtrain, watchlist = watchlist, nrounds = 5,
                     
                     verbose = 1, maximize = T, nthread = 2)
    gc()
    
    
    xgb.save(clf, paste0("D:/kaggle/walmart_seg/models/", "clf","_", 11142015, "_100", ".R") )
    
    #scoring to be done -- issues with function scoring
    
  }     
}



Time_Taken <- Sys.time() - start


# NOT USING THE SUBMISSION FUNCTION PRED FUNCTION FOR NOW 11-10-2015

#submit(clf, test, "1172015.csv")

pred <- predict(clf_extra, data.matrix(test[, feature.names])) 

pred <- matrix(pred, nrow=38, ncol=length(pred)/38) #there are total 38 classes 

pred <-  data.frame(t(pred))

sample <- read_csv("D:/kaggle/walmart_seg/Data/sample_submission.csv") 

cnames <- names(sample)[2:ncol(sample)] 

names(pred) <- cnames

submission <- cbind.data.frame(VisitNumber = visit_num , pred) 

submission <- setDT(submission)

submission <- (submission[ , lapply(.SD, mean), by = VisitNumber])

write_csv(submission, "D:/kaggle/walmart_seg/submission/11152015_2.csv")


####################################################################################################################


#save and retrain model later


ptrain <- predict(clf, dtrain, outputmargin = T)


setinfo(dtrain, "base_margin", ptrain)


clf_extra <- xgboost(params = param, data = dtrain, nround = 100, verbose = 1, nthread = 2, 
                     
                     maximize = T)


xgb.save(clf_extra, paste0("D:/kaggle/walmart_seg/models/", "clf","_", 11142015, "_200", ".R") )