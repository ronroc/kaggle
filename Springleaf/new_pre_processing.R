
x <- c("start  of the first step", "Reading in the data"); cat(x, sep = "\n")

rm(list = ls())

library(readr); library(xgboost); library(caret); library(lubridate)

library(readr); library(xgboost); library(doParallel); library(caret) 

cl <- makeCluster(2); registerDoParallel(cl)

set.seed(8675309)

train <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

# save ID and response

train_target <- train$target

train$target <- NULL

train_ID <- train$ID

train$ID <- NULL

test <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")

# save ID and response

test_ID <- test$ID

test$ID <- NULL

#train$VAR_0241 <- as.numeric(as.character(train$VAR_0241))

#test$VAR_0241 <- as.numeric(as.character(test$VAR_0241))

print(dim(train)); print(dim(test))

#bind train and test for better and easier pre-processing

tmp <- rbind(train, test)

#seperate out date columns in train and test (mostly by using a grep function)

datecolumns = c("VAR_0073", "VAR_0075", "VAR_0156", "VAR_0157", "VAR_0158",
                
                "VAR_0159", "VAR_0166", "VAR_0167", "VAR_0169","VAR_0168", 
                
                "VAR_0176", "VAR_0177", "VAR_0178", "VAR_0179", "VAR_0204", "VAR_0217")

tmp_date <- tmp[datecolumns]

#convert into

tmp_date <- data.frame(apply(tmp_date, 2, 
                             
                      function(x) (strptime(x, format = "%d%b%y: %H:%M:%S",tz = "UTC"))))



tmp_date[ ,c(17:32)] <- data.frame(apply(tmp_date[ , c(1:16)], 2, function(x) wday(x)))

names(tmp_date)[(17:32)] <- paste0("wday_", names(tmp_date)[(1:16)])



tmp_date[ ,c(33:48)] <- data.frame(apply(tmp_date[ , c(1:16)], 2, function(x) mday(x)))

names(tmp_date)[(33:48)] <- paste0("mday_", names(tmp_date)[(1:16)])



tmp_date[, c(49:50)] <- data.frame(apply(tmp_date[ , c(15:16)], 2, function(x) hour(x) )) 

names(tmp_date)[(49:50)] <- paste0("hour_", names(tmp_date)[c(15:16)])



tmp_date[ , c(51:52)] <- data.frame(apply(tmp_date[ , c(49:50)], 2, function(x) 
  
                        ifelse(x <= 6, 1, 
         
                        ifelse(x <= 12, 2,
                
                        ifelse(x <= 18, 3,
                       
                        ifelse(x <= 24, 4))))))


names(tmp_date)[(51:52)] <-  paste0("Dzones_", names(tmp_date)[(15:16)])

#plot histogram of dates

par(mar=c(2,2,2,2),mfrow=c(4,4))

for(i in 1:16){
  
  hist(tmp_date[,i], "weeks" ,format = "%d %b %y", main = names(tmp_date[,i]), 
       
       xlab="", ylab="")
}

for(i in 1:16){
  
  tmp_date[, i] <- as.numeric(tmp_date[, i])
}


tmp_pre <-  preProcess(tmp_date[ , c(1:16)], method = ("BoxCox"))

tmp_pre_pred <- predict(tmp_pre, tmp_date[ , c(1:16)])

tmp_date[ , c(1:16)] <- tmp_pre_pred




dim(tmp)

tmp <- tmp[ , !(names(tmp) %in% (datecolumns))]

tmp <- cbind(tmp, tmp_date)

dim(tmp); dim(tmp_date)

gc()

  nzv <- nearZeroVar(tmp)

nzv <- read_csv("nzv.csv")

tmp <- tmp[, -nzv$nzv]

##########################################################################################

#Better encoding of categorical variables

#imp_feature <- read_csv("D:/kaggle/Springleaf/xgb_featureImp.csv")

imp_feature <- read_csv("D:/kaggle/Springleaf/imp_1000.csv")

#names <- (imp_feature$Feature)

names <- names(imp_feature)

#imp_names <- sapply(names, function(x) names(tmp)[x])

#tmp_imp <- tmp[ 1:100 , (names(tmp) %in% imp_names)]

tmp_imp <- tmp[  , (names(tmp) %in% names)]

#write_csv(tmp_imp, "imp_1000.csv")

for(i in 1:43){
  
  tmp_imp[, i] <- as.factor(tmp_imp[, i])
}

tmp_imp$VAR_0543 <- NULL; tmp_imp$VAR_0704 <- NULL; tmp_imp$VAR_0920 <- NULL

tmp_imp$VAR_1087 <- NULL; tmp_imp$VAR_0891 <- NULL; tmp_imp$VAR_0908 <- NULL

tmp_imp$VAR_0887 <- NULL; tmp_imp$VAR_0298 <- NULL; tmp_imp$VAR_0318 <- NULL

tmp_imp$VAR_1494 <- NULL; tmp_imp$VAR_0608 <- NULL; tmp_imp$VAR_0330 <- NULL

tmp_imp$VAR_1247 <- NULL; tmp_imp$VAR_0073 <- NULL; tmp_imp$VAR_0241 <- NULL
############################################################################################

#Group by function;select columns to apply function and also the interaction degrees

##group_Data function

groupData <- function(xmat, Degree){
  
  require(foreach, quietly = T)
  
  #indices of combinations
  
  xind <- combn(1:ncol(xmat), Degree)
  
  #storage structure for the result
  
  agx <- foreach(ii = 1:ncol(xind), .combine = cbind) %do%
    
  {
    x <- xmat[ , xind[1, ii]]
    
    for(jj in 2:nrow(xind))
      
    {
      x <- paste(x, xmat[ , xind[jj, ii]], sep = "_")
      
    }
    x
  }
  colnames(agx) <- paste(paste("f", Degree, sep = ""), 1:ncol(agx), sep = "_")
  
  return(agx)
}

############################################################################################

tmp_imp_2 <- groupData(tmp_imp, 1)

#dim(tmp_imp_2)

dummies <- dummyVars(~ ., data = tmp_imp)

tmp_imp <- predict(dummies, newdata = tmp_imp)

tmp_imp_1 <- cbind.data.frame(tmp_imp_cpy$VAR_0241, tmp_imp_cpy$VAR_1934, tmp_imp_cpy$VAR_0001) 


tmp_imp_1 <- model.matrix( ~ ., data = tmp_imp_1)

gc()

rm(nzv); rm(test); rm(test_imp); rm(tmp); rm(tmp_date); rm(tmp_pre_pred)

rm(train); rm(datecolumns); rm(train_target); rm(train_ID); rm(test_ID);rm(names)

rm(tmp_Imp)

test_imp_2d <- predict(dummies_test, newdata = test_imp_2)



##############################################################################

tmp_new <- cbind(tmp, tmp_imp)

rm(tmp); rm(tmp_date); rm(tmp_imp); rm(tmp_imp_2); rm(tmp_imp_2d)

#testing step to check fr fixing cat. vars

#seperate numeric and character columns

tmp_numr <- tmp_new[ , sapply(tmp_new, is.numeric)]

tmp_char <- tmp_new[ , sapply(tmp_new, is.character)]

cat("Numerical Column Count", length(names(tmp_numr)),"\n", 
    
    "Character column count", length(names(tmp_char)))

#check for number of unique elements per column_character

#str(lapply(tmp_char, unique ), vec.len = 3)


#check for number of unique elements per column_numeric

#str(lapply(tmp, unique ), vec.len = 3, list.len = 900)


tmp_new[is.na(tmp_new)] <- -9999999


feature.names <- names(tmp_new)

for (f in feature.names) {
  
  if (class(tmp_new[[f]])=="character") {
    
    levels <- unique((tmp_new[[f]]))
    
    tmp_new[[f]] <- as.integer(factor(tmp_new[[f]], levels=levels))
    
  }
  
}


##############################################################################

train <-  tmp_new[c(1:nrow(train)), ]

test <- tmp_new[((nrow(train) +1) : nrow(tmp_new)), ]

dim(train); dim(test)

gc()


split <- createDataPartition(train_target, p = .75, list = FALSE)

response <- train_target

training <- train[ split,]

testing  <- train[-split,]

response_testing <-  response[-split]

response_training <- response[split]

dtrain <- xgb.DMatrix(data.matrix(training[,feature.names]), label= response_training)

dval <- xgb.DMatrix(data.matrix(testing[,feature.names]), label= response_testing)

watchlist <- list(train = dtrain, test = dval)

param <- list(  objective   = "binary:logistic", 
                
                eta                 = 0.005,
                
                max_depth           = 11,
                
                subsample           = 0.7,
                
                colsample_bytree    = 0.35,
                
                eval_metric         = "auc"
)



clf_first <- xgb.train( params = param, 
                        
                        data                = dtrain, 
                        
                        nrounds             = 1000,
                        
                        verbose             = 2, 
                        
                        watchlist = watchlist,
                        
                        nthread = 2,
                        
                        maximize = TRUE)

submission <- data.frame(ID=test_ID)

submission$target <- NA 

submission[,"target"] <- predict(clf_first, data.matrix(test[,feature.names]))

write_csv(submission, "10072015.csv")

#############################################################################
ptrain = predict(clf_first, dtrain, outputmargin = T)

setinfo(dtrain, "base_margin", ptrain)


clf_first_1 <- xgboost( params = param, 
                          
                          data                = dtrain, 
                          
                          nrounds             = 1000,
                          
                          verbose             = 2,
                        
                          nthread = 2,
                          
                          maximize = TRUE)

submission <- data.frame(ID=test_ID)

submission$target <- NA 

submission[,"target"] <- predict(clf_first_1, data.matrix(test[,feature.names]))

write_csv(submission, "10072015_1.csv")
