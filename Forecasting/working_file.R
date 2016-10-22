#10222015--------------------------------------------------------------------------------

library(readr)

library(xgboost)

set.seed(28071993)

cat("reading the train and test data\n")

train <- read_csv("D:/kaggle/Forecasting/DATA/train.csv")

test  <- read_csv("D:/kaggle/Forecasting/DATA/test.csv")

store <- read_csv("D:/kaggle/Forecasting/DATA/store.csv")

# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)

train <- merge(train,store)

test <- merge(test,store)

# There are some NAs in the integer columns so conversion to zero

train[is.na(train)]   <- -1

test[is.na(test)]   <- -1

names(train)

str(train)

summary(train)

names(test)

str(test)

summary(test)

# looking at only stores that were open in the train set

# may change this later

train <- train[ which(train$Open=='1'),]

train <- train[ which(train$Sales!='0'),]

# seperating out the elements of the date column for the train set

train$month <- as.integer(format(train$Date, "%m"))

train$year <- as.integer(format(train$Date, "%y"))

train$day <- as.integer(format(train$Date, "%d"))

# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)

train <- train[,-c(3,8)]

# seperating out the elements of the date column for the test set

test$month <- as.integer(format(test$Date, "%m"))

test$year <- as.integer(format(test$Date, "%y"))

test$day <- as.integer(format(test$Date, "%d"))

# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)

test <- test[,-c(4,7)]

feature.names <- names(train)[c(1,2,5:19)]

feature.names

cat("assuming text variables are categorical & replacing them with numeric ids\n")

for (f in feature.names) {
  
  if (class(train[[f]])=="character") {
    
    levels <- unique(c(train[[f]], test[[f]]))
    
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

response <- train$Sales

train$Sales <- NULL

split = createDataPartition(train_target, p = 0.9, list = F)

response_val <- response[-split]

response_train <- response[split]

tmp <- train[,feature.names]

dval <- xgb.DMatrix(data=data.matrix(tmp[-split, ]), label = log(response_val+1))

dtrain <- xgb.DMatrix(data=data.matrix(tmp[split, ]), label = log(response_train+1))

watchlist<-list(val=dval,train=dtrain)

RMPSE<- function(preds, dtrain) {
  
  labels <- getinfo(dtrain, "label")
  
  elab <- exp(as.numeric(labels))-1
  
  epreds <- exp(as.numeric(preds))-1
  
  err <- sqrt(mean((epreds/elab-1)^2))
  
  return(list(metric = "RMPSE", value = err))
}

param <- list(  objective           = "reg:linear", 
                
                booster = "gbtree",
                
                eta                 = 0.01, 
                
                max_depth           = 20, 
                
                subsample           = 0.7, 
                
                colsample_bytree    = 0.7 
                
                # alpha = 0.0001, 
                
                # lambda = 1
)

cl <- makeCluster(2); registerDoParallel(cl)

clf <- xgb.train(   params              = param, 
                    
                    data                = dtrain, 
                    
                    nrounds             = 1000, #300, #280, #125, #250, # changed from 300
                    
                    verbose             = 1,
                    
                    nthread             = 2,
                    
                    
                    
                    watchlist           = watchlist,
                    
                    maximize            = FALSE,
                    
                    feval=RMPSE
)


pred <- exp(predict(clf, data.matrix(test[,feature.names]))) -1

submission <- data.frame(Id=test$Id, Sales=pred)

write_csv(submission, "D:/kaggle/Forecasting/submission/1022015.csv")

######################################################################################




#10232015------------------------------------------------------------------------------

library(data.table)  

library(h2o)

train <- fread("../input/train.csv",stringsAsFactors = T)

test  <- fread("../input/test.csv",stringsAsFactors = T)

store <- fread("../input/store.csv",stringsAsFactors = T)

train <- train[Sales > 0,]  ## We are not judged on 0 sales records in test set

train <- merge(train,store,by="Store")

test <- merge(test,store,by="Store")

train[,Date:=as.Date(Date)]

test[,Date:=as.Date(Date)]

# seperating out the elements of the date column for the train set

train[,month:=as.integer(format(Date, "%m"))]

train[,year:=as.integer(format(Date, "%y"))]

train[,Store:=as.factor(as.numeric(Store))]

test[,month:=as.integer(format(Date, "%m"))]

test[,year:=as.integer(format(Date, "%y"))]

test[,Store:=as.factor(as.numeric(Store))]

train[,logSales:=log1p(Sales)]

h2o.init(nthreads=-1,max_mem_size='6G')

trainHex<-as.h2o(train)

features<-colnames(train)[!(colnames(train) %in% c("Id","Date","Sales","logSales","Customers"))]

rfHex <- h2o.randomForest(x=features,
                          y="logSales", 
                          ntrees = 100,
                          max_depth = 30,
                          nbins_cats = 1115, ## allow it to fit store ID
                          training_frame=trainHex)

testHex<-as.h2o(test)

predictions<-as.data.frame(h2o.predict(rfHex,testHex))

pred <- expm1(predictions[,1])

submission <- data.frame(Id=test$Id, Sales=pred)

write_csv(submission, "D:/kaggle/Forecasting/submission/1023015.csv")

metric=function(y,yhat)
  
{
  
  y=exp(y)-1
  
  yhat=exp(yhat)-1
  
  sum_squared=0
  
  for( i in c(1:length(yhat)))
    
  {
    
    prop_squared=((y[i]-yhat[i])/y[i])^2
    
    sum_squared=sum_squared+prop_squared
    
  }
  
  err=sqrt((1/length(yhat))*sum_squared)
  
  return(err)
  
}

#######################################################################################


#10232015_1-----------------------------------------------------------------------------

library(readr); library(xgboost); library(data.table); require(sqldf)

set.seed(28071993)

train <- fread("D:/kaggle/Forecasting/DATA/train.csv")

store <- fread("D:/kaggle/Forecasting/DATA/store.csv")

test <- fread("D:/kaggle/Forecasting/DATA/test.csv")

train <- merge(train,store, by = "Store")

test <- merge(test,store, by = "Store")

setdiff(names(train), names(test))

test[, `:=`(Sales = "NA", Customers = "NA" )]

train[, `:=`(Id = 1:nrow(train) )]

tmp <- rbind(train, test)

tmp[, Date := as.Date(Date)]

tmp[, `:=`( month = as.integer(format(Date, "%m")), 
            
            year = as.integer(format(Date, "%y")),
            
            day = as.integer(format(Date, "%d"))
            
)]

factors <- names(tmp)[!(names(tmp) %in% c("Date", "Sales", "Customers", 
                                          "CompetitionDistance", "Id"))]
tmp <- data.frame(tmp)

tmp_original <- tmp

for( i in factors){
  
  tmp[, i] <- as.factor(tmp[, i])
  
  print(paste(i, ":", length(table(tmp[i]))))
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

#####################################################################################################

#2 way count--------------------------------------------------------------------

nms <- combn(factors, 2)

dim(nms)

nms_df <- data.frame(nms) 

len = length(names(nms_df))

for (i in 1:len) {
  
  nms_df[, i] <- as.character(nms_df[, i])
  
}

tmp_count <- data.frame(id = 1:dim(tmp)[1])

for(i in 1:dim(nms_df)[2]){
  
  print(((i / dim(nms_df)[2]) * 100 ))
  
  tmp_count[, paste(i, "_two", sep="")] <- my.f2cnt(th2 = tmp, 
                                                    
                                                    vn1 = nms_df[1,i], 
                                                    
                                                    vn2 = nms_df[2,i] )
  
}

#3 way count--------------------------------------------------------------------

start <- Sys.time()

nms <- combn(factors, 3)

dim(nms)

nms_df <- data.frame(nms)

len = length(names(nms_df))

for (i in 1:len) {
  
  print(paste0(( i / len) *100, "%"))
  
  nms_df[, i] <- as.character(nms_df[, i])
  
}

for(i in 1:dim(nms_df)[2]){
  
  print((i / dim(nms_df)[2]) * 100)
  
  tmp_count[, paste(i, "_three", sep="")] <- my.f3cnt(th2 = tmp, 
                                                      
                                                      vn1 = nms_df[1,i], 
                                                      
                                                      vn2 = nms_df[2,i], 
                                                      
                                                      vn3 = nms_df[3,i])
  
}

time_taken <- Sys.time() - start 

tmp_new = cbind.data.frame(tmp_original, tmp_count)



###############################################################################

train <-  tmp_new[1:1017209,  ]

test <- tmp_new[(1017209+1) : nrow(tmp_new), ]

rm(nms); rm(nms_df); rm(store); rm(tmp); rm(factor_col)
rm(factors); rm(i); rm(len); rm(removecols); rm(tmp_count); rm(tmp_original)
rm(tmp_new)

gc()

removecols <- c("Id","Date","Sales","Customers")

feature.names <- colnames(train)[!(colnames(train) %in% removecols)]

train = setDT(train)

train <- train[Open == 1]

train <- train[Sales != 0]

train <- as.data.frame(train)

#train <- train[ which(train$Open=='1'),]

#train <- train[ which(train$Sales!='0'),]

train[is.na(train)] <- 0

test[is.na(test)] <- 1

for (f in names(train)) {
  
  if (class(train[[f]])=="character") {
    
    levels <- unique(c(train[[f]], test[[f]]))
    
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
    
  }
}

response <- train$Sales

split = createDataPartition(response, p = 0.9, list = F)

response_val <- response[-split]

response_train <- response[split]

tmp <- train[,feature.names]

dval <- xgb.DMatrix(data=data.matrix(tmp[-split, ]), label = (response_val))

dtrain <- xgb.DMatrix(data=data.matrix(tmp[split, ]), label = (response_train))

watchlist<-list(val=dval,train=dtrain)

param <- list(  objective           = "reg:linear", 
                booster = "gbtree",
                eta                 = 0.02, # 0.06, #0.01,
                max_depth           = 10, #changed from default of 8
                subsample           = 0.9, # 0.7
                colsample_bytree    = 0.7 # 0.7
                #num_parallel_tree   = 2
                # alpha = 0.0001, 
                # lambda = 1
)

RMPSE<- function(preds, dtrain) {
  
  labels <- getinfo(dtrain, "label")
  
  elab <- exp(as.numeric(labels))-1
  
  epreds <- exp(as.numeric(preds))-1
  
  err <- sqrt(mean((epreds/elab-1)^2))
  
  return(list(metric = "RMPSE", value = err))
}


gc()
library(doParallel)
cl <- makeCluster(2); registerDoParallel(cl)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 3000, #300, #280, #125, #250, # changed from 300
                    verbose             = 2,
                    early.stop.round    = 600,
                    watchlist           = watchlist,
                    maximize            = T,
                    feval=RMPSE
)

pred1 <- exp(predict(clf, data.matrix(test[,feature.names]))) -1

submission <- data.frame(Id=test$Id, Sales=pred1)

write_csv(submission, "10252015.csv")


#10252015_1-------------------------------------------------------------------------------

# changed n_rounds to 15000, LB score decreased 