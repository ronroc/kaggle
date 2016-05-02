clf_first <- xgb.train( params = param, 
                        
                        data                = dtrain, 
                        
                        nrounds             = 700, # changed from 300
                        
                        verbose             = 2, 
                        
                        
                        
                        nthread = 2,
                        
                        maximize = TRUE)

submission_second <- data.frame(ID=test$ID)

submission_second$target <- NA 

submission_second[,"target"] <- predict(clf_first, data.matrix(test[,feature.names]))

write_csv(submission_second, "fourth.csv")

xgb.save(clf_first, "xgb_fourth.R")
