
#11252015

require(data.table); require(lubridate); require(caret); require(sqldf); require(xgboost); require(sqldf); require(xlsx); require(Matrix)

train_raw <- fread(input = "D:\\kaggle\\HOMESITE\\Data\\train.csv", data.table = F)

response <- train_raw$QuoteConversion_Flag

train_raw$QuoteConversion_Flag <- NULL

train_raw$QuoteNumber <- NULL



test_raw <- fread(input = "D:\\kaggle\\HOMESITE\\Data\\test.csv", data.table = F)

id <- test_raw$QuoteNumber

test_raw$QuoteNumber <- NULL


tmp <- rbind(train_raw, test_raw)

tmp$Original_Quote_Date <- as.Date(tmp$Original_Quote_Date)

tmp$month <- as.integer(format(tmp$Original_Quote_Date, "%m"))

tmp$year <- as.integer(format(tmp$Original_Quote_Date, "%y"))

tmp$day <- weekdays(as.Date(tmp$Original_Quote_Date))


tmp[is.na(tmp)] <- -1


match("SalesField8", names(tmp))


# include salesfield8 field in the sparse matrix


dummy_var = names(tmp)[c(-34, -4)]  


tmp_dummy <- tmp[ , dummy_var]



len = length(names(tmp_dummy))


for(i in 1:len){
  
  print(paste0(( i / len) * 100, "%"))
  
  levels <- unique(tmp_dummy[[i]])
  
  tmp_dummy[, i] <- factor(tmp_dummy[, i], levels = levels)
  
}


gc()


# tmp_dummy$SalesField8 <- tmp$SalesField8


# sparse.model.matrix(~ . -1, data = rbind(train[,-1],test[,-1]))

# error : this can be applied to factors with 2 or more levels

sum(is.na(tmp_dummy))

sum(is.na(tmp))

a <- lapply(tmp_dummy, function(x) sum(is.na(x)))


for(i in 1:length(a)){
  
  if( a[[i]] > 1 ) print(paste(names(a)[i], i))
  
}


tmp_dummy$PropertyField6 <- NULL

tmp_dummy$Original_Quote_Date <- NULL

tmp_dummy$GeographicField10A <- NULL

a <- lapply(tmp_dummy, function(x) length(unique(x)))

for(i in 1:length(a)){
  
  if( a[[i]] == 1 ) print(paste(a[i], i))

  }

dummies <- sparse.model.matrix(~ . -1 , data = tmp_dummy, 
                               
                               contrasts.arg = lapply(tmp_dummy, contrasts, contrasts=FALSE))





train = dummies[1:nrow(train_raw), ]


test <- dummies[(nrow(train_raw)+1):nrow(dummies), ]



dtrain <- xgb.DMatrix(data = train, label=response)


param <- list(objective           = "binary:logistic",
              
              booster = "gbtree",
              
              eval_metric = "auc",
              
              eta = 0.02, # 0.06, #0.01,
              
              max_depth = 7, #changed from default of 8
              
              subsample = 0.86, # 0.7
              
              colsample_bytree = 0.68, # 0.7
              
              num_parallel_tree = 2
              
              # alpha = 0.0001,
              
              # lambda = 1
              
)


cl <- makeCluster(2); registerDoParallel(cl)


set.seed(11*25*15)


cv <- xgb.cv(params = param, data = dtrain, 
             
             nrounds = 1900, 
             
             nfold = 4, 
             
             showsd = T, 
             
             maximize = F)

start <- Sys.time()

clf <- xgb.train(   params              = param,
                    
                    data                = dtrain,
                    
                    nrounds             = 1900,
                    
                    verbose             = 1,  #1
                    
                    #early.stop.round    = 150,
                    
                    #watchlist           = watchlist,
                    
                    maximize            = FALSE,
                    
                    nthread = 2)



pred <- predict(clf, data.matrix(test))


submission <- data.frame(QuoteNumber = id, QuoteConversion_Flag = pred)


write_csv(submission, "D:\\kaggle\\HOMESITE\\submission\\11252015.csv")

total_part <- Sys.time() - start