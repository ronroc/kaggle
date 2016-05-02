# seperate tmp into seperate df's for char, num, binary, dates -- manipulate -- combine

require(data.table); require(lubridate); require(caret); require(sqldf); require(xgboost); require(sqldf); require(xlsx); require(readr)


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


####################################################################################################


# seperating top 25 predictors from feature importance

imp <- read.xlsx("feature_importance.xlsx", sheetIndex = 1, stringsAsFactors=FALSE)

top_25 <- imp$Feature[1:25]

tmp_25 <- tmp[, top_25]

lapply(tmp_25, function(x) unique(x))

lapply(tmp_25_result , function(x) sum(is.na(x)))



# first convert all character to numeric

for (f in top_25) {
  
  if (class(tmp_25[[f]])=="character") {
    
    levels <- unique(tmp_25[[f]])
    
    tmp_25[[f]] <- as.integer(factor(tmp_25[[f]], levels=levels))
    
  }
}

# Bag Impute---------------------------------------------------------------------------------------

tmp_25_test = preProcess(tmp_25, method = "bagImpute")

set.seed(11212015)

tmp_25_result <- predict(tmp_25_test, tmp_25)


# dummfying columns--------------------------------------------------------------------------------

dummy_var = names(tmp_25_result)[-4] # dummfying all top 25 columns but change during running

tmp_dummy <- tmp[ , dummy_var]

len = length(names(tmp_dummy))

for(i in 1:len){
  
  print(paste0(( i / len) * 100, "%"))
  
  levels <- unique(tmp_dummy[[i]])
  
  tmp_dummy[, i] <- factor(tmp_dummy[, i], levels = levels)
  
}


gc()


dummies <- dummyVars( ~., data = tmp_dummy)

gc()


tmp_dummy <- predict(dummies, newdata = tmp_dummy)

tmp_dummy <- data.frame(tmp_dummy)

dim(tmp_dummy)



####################################################################################################

tmp_raw <- tmp[ , ! (names(tmp) %in% top_25)]

tmp_new <- cbind(tmp_raw, tmp_25_result, tmp_dummy)


# first convert all character to numeric

for (f in names(tmp_new)) {
  
  if (class(tmp_new[[f]])=="character") {
    
    levels <- unique(tmp_new[[f]])
    
    tmp_new[[f]] <- as.integer(factor(tmp_new[[f]], levels=levels))
    
  }
}


####################################################################################################

train <- tmp_new[c(1:260753), ]

test <- tmp_new[c(260754:434589), ]

gc()


train[is.na(train)] <- -1

test[is.na(test)] <- -1

gc()


###################################################################################################

feature.names <- names(train)

h<-sample(nrow(train),2000)

dval<-xgb.DMatrix(data=data.matrix(train[h,]),label=response[h])
#dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,]),label=train$QuoteConversion_Flag[-h])
dtrain<-xgb.DMatrix(data=data.matrix(train),label=response)

watchlist<-list(val=dval,train=dtrain)

param <- list(objective           = "binary:logistic", 
                booster = "gbtree",
                eval_metric = "auc",
                eta                 = 0.02, # 0.06, #0.01,
                max_depth           = 7, #changed from default of 8
                subsample           = 0.82, # 0.7
                colsample_bytree    = 0.66, # 0.7
                num_parallel_tree   = 2
                # alpha = 0.0001, 
                # lambda = 1
              )

cl <- makeCluster(2); registerDoParallel(cl) 

set.seed(11*21*15)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 1800, 
                    verbose             = 1,  #1
                    #early.stop.round    = 150,
                    watchlist           = watchlist,
                    maximize            = FALSE,
                    
                    nthread = 2)


pred <- predict(clf, data.matrix(test[,feature.names]))

submission <- data.frame(QuoteNumber = id, QuoteConversion_Flag = pred)

write_csv(submission, "D:\\kaggle\\HOMESITE\\submission\\11212015_2.csv")

