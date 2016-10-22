require(data.table); require(lubridate); require(caret); require(sqldf); require(xgboost); require(xlsx); require(dplyr); require(readr); require(doParallel); require(bit64)

Spanish2English <- fread("D:\\kaggle\\SANTANDER\\DATA\\Spanish2English.csv", data.table = F)

train_raw <- fread("D:\\kaggle\\SANTANDER\\DATA\\train.csv", data.table = F)

extra <- cbind.data.frame( ID = train_raw$ID, 
                           
                           var3 = train_raw$var3, 
                           
                           var15 = train_raw$var15, 
                           
                           var38 = train_raw$var38, 
                           
                           TARGET = train_raw$TARGET )

train_raw <- train_raw[, !(names(train_raw) %in% names(extra))]

names(train_raw) <- Spanish2English$English

train_raw <- cbind(train_raw, extra)


test_raw <- fread("D:\\kaggle\\SANTANDER\\DATA\\test.csv", data.table = F)

extra <- cbind.data.frame( ID = test_raw$ID, 
                           
                           var3 = test_raw$var3, 
                           
                           var15 = test_raw$var15, 
                           
                           var38 = test_raw$var38 )

test_raw <- test_raw[, !(names(test_raw) %in% names(extra))]

names(test_raw) <- Spanish2English$English

test_raw <- cbind(test_raw, extra)


##############################################################################################################


response <- train_raw$TARGET

train_raw$TARGET <- NULL

train_raw$ID <- NULL


id <- test_raw$ID

test_raw$ID <- NULL

tmp <- rbind(train_raw, test_raw)


##############################################################################################################

a <- data.frame(sapply(tmp, function(x) length(unique(x))))

devtools::source_gist('4a0a5ab9fe7e1cf3be0e')

b <- strtable(tmp)

c <- lapply(tmp, function(x) (unique(x)))

unique_df <- cbind.data.frame(unique = a$sapply.tmp..function.x..length.unique.x..., b)

write_csv(unique_df, "D:\\kaggle\\SANTANDER\\DATA\\unique_df.csv")



# checking for categorical and continous vars-----------------------------------------------------------------

len_unique <- rep(0, ncol(tmp))

for(i in 1:nrow(unique_df))

  {
  
  if(unique_df$unique[i] < 10) 
    
    {
    
    len_unique[i] <- unique_df$variable[i]
  }
  
}

len_unique <- len_unique[len_unique != 0]


#tmp_unique <- tmp[, len_unique]


for(f in names(tmp)) {
  
  if (length(unique(tmp[[f]])) == 1) {
    
    cat(f, "is constant in train and test combine we delete it.\n")
    
    tmp[[f]] <- NULL
    
  }
}



# seperate dummy variables ( #indicator0# ) from tmp

dummy <- names(tmp)[grep("indicator 0", names(tmp), value = F)]

tmp_dummy <- tmp[, !(names(tmp) %in% dummy)]



# a function for checking number of unique elements and obtaining a freq. output------------------

# unique_no = less than or equal to unique_no----------------------------------------------------

unique_freq <- function(df, unique_no)

  {
  
  a <- (sapply(df, function(x) length(unique(x))))
  
  b <- names(df)
  
  unique_df <- data.frame(names = b, len = a, row.names = 1:length(b), stringsAsFactors = F)
  
  uniq_nam <- c()
  
  for(i in 1:nrow(unique_df)){
    
    if(unique_df$len[i] <= unique_no){
      
      uniq_nam <- c(uniq_nam, unique_df$names[i])
      
    }
  }
  
  uniq_tmp <- df[, uniq_nam]
  
  o_p <- lapply(uniq_tmp, function(x)table(x))
  
  return(o_p)
}


# function to calculate percentage of zeros in every column--------------------------------------

zero_percent <- c()

for(i in 1:ncol(tmp)){
  
  x <- (sum(tmp[i] == 0) / nrow(tmp)) * 100
  
  zero_percent <- c(zero_percent, x)
  
}

df_zero <- data.frame(name = names(tmp), Z_P = zero_percent )

write_csv(df_zero, "D:\\kaggle\\SANTANDER\\DATA\\df_zero.csv")


################################################################################################

#modelling

train <- tmp[c(1:nrow(train_raw)), ]

test <- tmp[c((nrow(train_raw) + 1) : nrow(tmp)), ]


#h <- sample(nrow(train),2000)

#dval <- xgb.DMatrix(data = data.matrix(train[h,]),label = response[h])

#dtrain <- xgb.DMatrix(data = data.matrix(train[-h,]),label = response[-h], missing = NaN)

dtrain <- xgb.DMatrix(data = data.matrix(train),label = response, missing = NaN)


watchlist<-list(val = dval, train = dtrain)

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
                    
                    nrounds             = 877,
                    
                    #early.stop.round = 1000,
                    
                    verbose             = 2,
                    
                    #watchlist           = watchlist,
                    
                    maximize            = T,
                    
                    nthread = 4
                    
                    )


cv <- xgb.cv(params = param, 
       
       data = dtrain, 
       
       nrounds = 877, 
       
       nfold = 10, 
       
       prediction = T, 
       
       showsd = T, 
       
       maximize = T,
       
       nthread = 4)


preds <- predict(clf, data.matrix(test), missing = NaN)

submission <- data.frame(ID=id, TARGET=preds)

cat("saving the submission file\n")

write.csv(submission, "D:\\kaggle\\SANTANDER\\SUBMISSION\\03262016_1.csv", row.names = F)

# importance matrix--------------------------------------------------------------------------

importance_matrix <- xgb.importance(feature_names = names(train), model = clf)

jpeg(filename = "D:\\kaggle\\SANTANDER\\Plots\\imp_03262016.jpeg")

xgb.plot.importance(importance_matrix = importance_matrix[1:20, ])

dev.off()
