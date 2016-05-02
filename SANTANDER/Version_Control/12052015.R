
# seperate tmp into seperate df's for char, num, binary, dates -- manipulate -- combine

require(data.table); require(lubridate); require(caret); require(sqldf); require(xgboost); require(xlsx); require(dplyr)


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

tmp$week <- week((as.Date(tmp$Original_Quote_Date)))

tmp$date <- (((tmp$year * 52 ) + tmp$week) %% 4)


####################################################################################################


# seperating top 25 predictors from feature importance

imp <- read.xlsx("D:\\kaggle\\HOMESITE\\feature_importance.xlsx", sheetIndex = 1, stringsAsFactors=FALSE)

top_25 <- imp$Feature[1:25]

tmp_25 <- tmp[, top_25]

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

str(tmp_25_test)

str(tmp_25_result)

table(tmp_25_result$PropertyField29)

names(tmp_25_result)

dummy_var = names(tmp_25_result)[-4] # dummfying all top 25 columns but change during running

#tmp_dummy <- tmp_char[ , dummy_var]

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


lapply(tmp_25_result, function(x) unique(x))

tmp_raw <- tmp[ , ! (names(tmp) %in% top_25)]

tmp_new <- cbind(tmp_raw, tmp_25_result, tmp_dummy)

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
              
              eta = 0.023, # 0.06, #0.01,
              
              max_depth = 6, #changed from default of 8
              
              subsample = 0.83, # 0.7
              
              colsample_bytree = 0.77, # 0.7
              
              num_parallel_tree = 2
              
              # alpha = 0.0001,
              
              # lambda = 1
              
)

start <- Sys.time()

cl <- makeCluster(2); registerDoParallel(cl)

set.seed(11*21*15)

#cv <- xgb.cv(params = param, data = dtrain, 

#            nrounds = 1800, 

#           nfold = 4, 

#          showsd = T, 

#         maximize = F)

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

write_csv(submission, "D:\\kaggle\\HOMESITE\\submission\\12052015.csv")

time_taken <- Sys.time() - start




##################################################################################################################################

# retraining xgboost

# model.xgb = xgboost(params = param, data = xgtrain, nround = x) 

#ptrain = predict(model.xgb, xgtrain, outputmargin = TRUE) 

ptrain = predict(clf, dtrain, outputmargin = TRUE) 

setinfo(dtrain, "base_margin", ptrain) 

clf_ext = xgboost(params = param, data = dtrain, nround = 1000, early.stop.round = 50) 


pred <- predict(clf_ext, data.matrix(test[,feature.names]))

submission <- data.frame(QuoteNumber = id, QuoteConversion_Flag = pred)

write_csv(submission, "D:\\kaggle\\HOMESITE\\submission\\12052015_1.csv")

#imp_mat <- xgb.importance(feature_names = feature.names, model = clf_ext)

write_csv(imp_mat, "D:\\kaggle\\HOMESITE\\fimp_12052015.csv")


##################################################################################################################################


require(data.table); require(lubridate); require(caret); require(sqldf); require(xgboost); require(xlsx); require(dplyr)

rm(list = ls())

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

tmp$week <- week((as.Date(tmp$Original_Quote_Date)))

tmp$date <- (((tmp$year * 52 ) + tmp$week) %% 4)



for (f in names(tmp)) {
  
  #if (class(tmp[[f]])=="character") {
    
    levels <- unique(tmp[[f]])
    
    tmp[[f]] <- as.integer(factor(tmp[[f]], levels=levels))
    
  }
  
#}

str(tmp, list.len =999)

tmp[is.na(tmp)] <- -1

## add interaction features

# Take interaction features from feature importance from various different algos (Eg : 20 from each)

# Interaction features



imp <- read_csv("D:\\kaggle\\HOMESITE\\fimp_12052015.csv")

top_50 <- imp$Feature[1:20]

tmp_int <- tmp[, top_50]

for (f in top_50) {
  
  if (class(tmp_int[[f]])=="character") {
    
    levels <- unique(tmp_int[[f]])
    
    tmp_int[[f]] <- as.integer(factor(tmp_int[[f]], levels=levels))
  }
  
}

#int_col <- c()

#tmp_int <- tmp_new[ , int_col]



# create + interaction features


for (i in 1:ncol(tmp_int)) {
  
  for (j in (i + 1) : (ncol(tmp_int) + 1)) {
    
#    a = i; b= j
    
    var.x <- colnames(tmp_int)[i]
    
    var.y <- colnames(tmp_int)[j]
    
    var.new <- paste0(var.x, '_plus_', var.y)
    
    tmp_int[ , paste0(var.new)] <- tmp_int[, i] + tmp_int[, j]
    
  }
}


gc()

# create - interaction features


for (i in 1:ncol(tmp_int)) {
  
  for (j in (i + 1) : (ncol(tmp_int) + 1)) {
    
    var.x <- colnames(tmp_int)[i]
    
    var.y <- colnames(tmp_int)[j]
    
    var.new <- paste0(var.x, '_minus_', var.y)
    
    tmp_int[ , paste0(var.new)] <- tmp_int[, i] - tmp_int[, j]
    
  }
}


gc()


# create * interaction features


for (i in 1:ncol(tmp_int)) {
  
  for (j in (i + 1) : (ncol(tmp_int) + 1)) {
    
    var.x <- colnames(tmp_int)[i]
    
    var.y <- colnames(tmp_int)[j]
    
    var.new <- paste0(var.x, '_mult_', var.y)
    
    tmp_int[ , paste0(var.new)] <- tmp_int[, i] * tmp_int[, j]
    
  }
}





# create / interaction features


for (i in 1:ncol(tmp_int)) {
  
  for (j in (i + 1) : (ncol(tmp_int) + 1)) {
    
    var.x <- colnames(tmp_int)[i]
    
    var.y <- colnames(tmp_int)[j]
    
    var.new <- paste0(var.x, '_divide_', var.y)
    
    tmp_int[, paste0(var.new)] <- tmp_int[, i] / tmp_int[, j]
    
  }
}



# tmp_int <- tmp_int[, -int_col] # why are the columns removed



# count sum of features by row
tmp$feat_sum <- as.integer(rowSums(tmp[, 1:ncol(tmp)]))
       
       # variance of features by row
tmp$feat_var <- as.integer(apply(tmp[, 1:ncol(tmp)], 1, var))  
       
       # count no. of non-empty features
tmp$feat_filled <- as.integer(rowSums(tmp[, 1:ncol(tmp)] != 0))
       
       # count of NA in rows
tmp$feat_NA <- apply(X = tmp, MARGIN = 1, FUN = function(x) sum(is.na(x)))  
       
       # optional features -- statistical feature addition
       
tmp$feat_mean <- as.integer(apply(tmp[, 1:ncol(tmp)], 1, mean))
       
tmp$feat_median <- as.integer(apply(tmp[, 1:ncol(tmp)], 1, median))
       
tmp$feat_min <- as.integer(apply(tmp[, 1:ncol(tmp)], 1, min))
       
tmp$feat_max <- as.integer(apply(tmp[, 1:ncol(tmp)], 1, max))
       
tmp$feat_LENunique <- as.integer(apply(tmp[, 1:ncol(tmp)], 1, function(x) length(unique(x))))

                                    

tmp_new <- (tmp)#, tmp_int)

rm(tmp)

train <- tmp_new[c(1:260753), ]

test <- tmp_new[c(260754:434589), ]

gc()

#train[is.na(train)] <- -1

#test[is.na(test)] <- -1

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
              
              eta = 0.023, # 0.06, #0.01,
              
              max_depth = 6, #changed from default of 8
              
              subsample = 0.83, # 0.7
              
              colsample_bytree = 0.77, # 0.7
              
              num_parallel_tree = 2
              
              # alpha = 0.0001,
              
              # lambda = 1
              
)

start <- Sys.time()

cl <- makeCluster(2); registerDoParallel(cl)

set.seed(12*06*15)

#cv <- xgb.cv(params = param, data = dtrain, 

#            nrounds = 1800, 

#           nfold = 4, 

#          showsd = T, 

#         maximize = F)

clf <- xgb.train(   params              = param,
                    
                    data                = dtrain,
                    
                    nrounds             = 1900,
                    
                    verbose             = 1,  #1
                    
                    #early.stop.round    = 150,
                    
                    watchlist           = watchlist,
                    
                    maximize            = FALSE,
                    
                    nthread = 2)


pred <- predict(clf, data.matrix(test[,feature.names]))

submission <- data.frame(QuoteNumber = id, QuoteConversion_Flag = pred)

write_csv(submission, "D:\\kaggle\\HOMESITE\\submission\\12062015\\12062015_1.csv")

time_taken <- Sys.time() - start


pred <- predict(clf, data.matrix(test[,feature.names]), ntreelimit = 1800)

submission <- data.frame(QuoteNumber = id, QuoteConversion_Flag = pred)

write_csv(submission, "D:\\kaggle\\HOMESITE\\submission\\12062015\\12062015_1.csv")



imp_mat <- xgb.importance(feature_names = feature.names, model = clf)

write_csv(imp_mat, "D:\\kaggle\\HOMESITE\\FEATURE_IMP\\12062015_1.csv")
