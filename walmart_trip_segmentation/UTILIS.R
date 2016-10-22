# implement log loss function and test it with test case from here: 

# http://www.kaggle.com/c/emc-data-science/forums/t/2149/is-anyone-noticing-difference-betwen-validation-and-leaderboard-error

# note: the function only takes in matrices

checkLogLoss <- function(model, data) {
  
  # LogLoss Function
  
  LogLoss <- function(actual, predicted, eps = 1e-15) {
    
    predicted <- pmax(pmin(predicted, 1 - eps), eps)
    
    -sum(actual*log(predicted))/nrow(actual)
  }
  
  # create dummy predictions and compare with fitted model
  
  pred <- as.matrix(predict(model, newdata = data, type = 'prob'))
  
  #check which works -- top or bottom
  
  #pred <- predict(clf, data.matrix(data[, feature.names])) 
  
  dummy.fit <- dummyVars(~ TripType, data = data, levelsOnly = T)  
  
  truth <- predict(dummy.fit, newdata = data)  # predict ground truth using validation set
  
  LogLoss(truth, pred)
}

#############################################################################################################

# function to create submissions

#requires creation of visit_num in init file

submit <- function(model, data, file) {
  
  # create predictions and write out to csv.file
  
  pred <- predict(clf, data.matrix(data[, feature.names])) 
  
  pred <- matrix(pred, nrow=38, ncol=length(pred)/38) #there are total 38 classes 
  
  pred = data.frame(t(pred))
  
  sample <- read_csv('sample_submission.csv') 
  
  cnames <- names(sample)[2:ncol(sample)] 
  
  names(pred) <- cnames
  
  submission <- cbind.data.frame(VisitNumber = visit_num , pred) 
  
  submission <- setDT(submission)
  
  submission <- (submission[ , lapply(.SD, mean), by = VisitNumber])
  
  #write_csv(submission, "D:/kaggle/walmart_seg/submission/file.csv")
  
  write_csv(submission,  file)
}

############################################################################################################

#function for grid search

# grid search

for (depth in c(9, 10, 8)) {
  
  for (rounds in c(2000, 3000)) {
    
    for(eta in c(0.3, 0.2, 0.1)){
      
      # train
      param <- list(objective = "multi:softprob",
                    
                    eval_metric = "mlogloss",
                    
                    num_class = numberOfClasses,
                    
                    max_depth = depth ,
                    
                    eta = eta,
                    
                    nthreads = 4
      )
      
      
      clf <- xgb.train(params = param, data = dtrain, watchlist = watchlist, nrounds = rounds,
                       
                       verbose = 1, maximize = T)
      gc()
      
      
      xgb.save(clf, paste0("clf", "_", rounds, "_",depth, "_", eta) )
      
      #scoring to be done -- issues with function scoring
      
    }     
  }
}  



##########################################################################################
#using owen`s Amazon code approach

my.f2cnt <- function(th2, vn1, vn2, filter=TRUE) {
  
  df <- data.frame(f1=th2[,vn1], f2=th2[,vn2], filter=filter)
  
  sum1 <- sqldf("select f1, f2, count(*) as cnt 
                
                from df 
                
                where filter=1 
                
                group by 1,2")
  
  tmp <- sqldf("select b.cnt 
               
               from df a left join sum1 b 
               
               on a.f1=b.f1 and a.f2=b.f2")
  
  tmp$cnt[is.na(tmp$cnt)] <- 0
  
  return(tmp$cnt)
  
}

#3 way count

my.f3cnt<-function(th2, vn1, vn2, vn3, filter=TRUE) {
  
  df<-data.frame(f1=th2[,vn1], f2=th2[,vn2], f3=th2[, vn3], filter=filter)
  
  sum1<-sqldf("select f1, f2, f3, count(*) as cnt 
              
              from df 
              
              where filter=1 
              
              group by 1,2, 3")
  
  tmp<-sqldf("select b.cnt 
             
             from df a left join sum1 b 
             
             on a.f1=b.f1 and a.f2=b.f2 and a.f3=b.f3")
  
  tmp$cnt[is.na(tmp$cnt)]<-0
  
  return(tmp$cnt)
  
}


#############################################################################################################

addAggFeatures <- function(data) {
  
  
  # add new features
  
  mutate(data, feat_sum = as.integer(rowSums(data[, 1:ncol(tmp_new)])),  # count sum of features by row
         
         feat_var = as.integer(apply(data[, 1:ncol(tmp_new)], 1, var)),  # variance of features by row
         
         feat_filled = as.integer(rowSums(data[, 1:ncol(tmp_new)] != 0))  # count no. of non-empty features
  )
  
}

################################################################################################################

# should write a function for grid search

# should write a function for random search

# random search code found in h2o.ai gbm otto code is a good start ; link below

# https://github.com/harell/Kaggle-Otto-Group-Product-Classification-Challenge/blob/master/models/gbm%20type%201.R


# below function is a random search code found in forums

#https://www.kaggle.com/c/otto-group-product-classification-challenge/forums/t/12947/achieve-0-50776-on-the-leaderboard-in-a-minute-with-xgboost/69427#post69427

random_search <- function(n_set, threads){
  
  #param is a list of parameters
  
  # Set necessary parameter
  
  param <- list("objective" = "multi:softprob",
                
                "max_depth"=6,
                
                "eta"=0.1,
                
                "subsample"=0.7,
                
                "colsample_bytree"= 1,
                
                "gamma"=2,
                
                "min_child_weight"=4,
                
                "eval_metric" = "mlogloss",
                
                "silent"=1,
                
                "num_class" = 9,
                
                "nthread" = threads)
  
  param_list <- list()
  
  for (i in seq(n_set)){
    
    ## n_par <- length(param)
    
    
    param$max_depth <- sample(3:7,1, replace=T)
    
    param$eta <- runif(1,0.01,0.6)
    
    param$subsample <- runif(1,0.1,1)
    
    param$colsample_bytree <- runif(1,0.1,1)
    
    param$min_child_weight <- sample(1:17,1, replace=T)
    
    
    param$gamma <- runif(1,0.1,10)
    
    param$min_child_weight <- sample(1:15,1, replace=T)
    
    param_list[[i]] <- param
    
  }
  
  return(param_list)
  
}