
# seperate tmp into seperate df's for char, num, binary, dates -- manipulate -- combine

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


####################################################################################################

# remove non zero variance predictors

nzv <- nearZeroVar(tmp)

tmp_raw <- tmp[, -nzv]

str(tmp_raw, list.len = 250)


for (f in names(tmp_raw)) {
  
  if (class(tmp_raw[[f]])=="character") {
    
    levels <- unique(tmp_raw[[f]])
    
    tmp_raw[[f]] <- as.integer(factor(tmp_raw[[f]], levels=levels))
    
  }
  
}


############################################################################################


# one way count 


dummy_var = names(tmp_raw)[c(-34)]  # dummfying all top 25 columns but change during running

tmp_factors <- tmp_raw[ , dummy_var]


len = length(names(tmp_factors))

for (i in 1:len) {
  
  print(paste0( i / (len) *100, "%"))
  
  levels <- unique(tmp_factors[[i]])
  
  tmp_factors[ , i] <- factor(tmp_factors[ , i], levels = levels)
  
}


tmp_count <- data.frame(id = 1:dim(tmp_raw)[1])


#one way count


len = dim(tmp_factors)[2]

for(i in 1:len){
  
  
  print((i / len) * 100 )
  
  tmp_factors$x <- tmp_factors[, i]
  
  sum1 <- sqldf("select x, count(1) as cnt
                
                from tmp_factors  group by 1 ")
  
  tmp1 <- sqldf("select cnt from tmp_factors a left join sum1 b on a.x=b.x")
  
  tmp_count[, paste(names(tmp_factors)[i], "_one", sep="")] <- tmp1$cnt
  
}  

tmp_count <- tmp_count[ , -1]

###########################################################################################
###########################################################################################



# NOT USING FOR THIS SUBMISSION  (11222015) - CHECK LINE 170 

# converting all of them to a sparse dummy matrix 

# check for continous columns to exclude from dummy vars and counts

 match("SalesField8", names(tmp_raw))



dummy_var = names(tmp_raw)[c(-34)]  # dummfying all top 25 columns but change during running

tmp_dummy <- tmp_raw[ , dummy_var]



len = length(names(tmp_dummy))

for(i in 1:len){
  
  print(paste0(( i / len) * 100, "%"))
  
  levels <- unique(tmp_dummy[[i]])
  
  tmp_dummy[, i] <- factor(tmp_dummy[, i], levels = levels)
  
}

gc()


len = length(names(train))

for(i in 1:len){
  
  print(paste0(( i / len) * 100, "%"))
  
  levels <- unique(train[[i]])
  
  train[, i] <- factor(train[, i], levels = levels)
  
}


matrix_train <- (model.matrix(~  . , data = train, 
                              
                              contrasts.arg = lapply(train, contrasts, contrasts=FALSE)))




sparse_test <- Matrix(model.matrix(~  . , data = test, 
                                   
                                   contrasts.arg = lapply(test, contrasts, contrasts=FALSE)), sparse=TRUE)



dummy_var = names(tmp_raw)[c(-34)]  # dummfying all top 25 columns but change during running

tmp_dummy <- tmp_raw[ , dummy_var]


###########################################################################################
###########################################################################################



tmp_new <- cbind(tmp_raw, tmp_count)

train <- tmp_new[c(1:260753), ]

test <- tmp_new[c(260754:434589), ]

gc()

train[is.na(train)] <- -1

test[is.na(test)] <- -1

gc()

#train <- train[sample(1:nrow(train), nrow(train)), ]

#test <- test[sample(1:nrow(test), nrow(test)), ]

feature.names <- names(train)

h <- sample(nrow(train),2000)

dval <- xgb.DMatrix(data=data.matrix(train[h,]),label=response[h])

#dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,]),label=train$QuoteConversion_Flag[-h])

dtrain <- xgb.DMatrix(data=data.matrix(train),label=response)

watchlist <- list(val=dval,train=dtrain)

param <- list(objective           = "binary:logistic",
              
              booster = "gbtree",
              
              eval_metric = "auc",
              
              eta = 0.02, # 0.06, #0.01,
              
              max_depth = 7, #changed from default of 8
              
              subsample = 0.82, # 0.7
              
              colsample_bytree = 0.66, # 0.7
              
              num_parallel_tree = 2
              
              # alpha = 0.0001,
              
              # lambda = 1
              
              )

cl <- makeCluster(2); registerDoParallel(cl)

set.seed(11*22*15)

start <- Sys.time()

clf <- xgb.train( params              = param,
                    
                    data                = dtrain, 
                    
                    label               = response,
                    
                    nrounds             = 1800,
                    
                    verbose             = 1,  #1
                    
                    #early.stop.round    = 150,
                    
                    watchlist           = watchlist,
                    
                    maximize            = FALSE,
                    
                    nthread = 2)

pred <- predict(clf, data.matrix(test[,feature.names]))

submission <- data.frame(QuoteNumber = id, QuoteConversion_Flag = pred)

write_csv(submission, "D:\\kaggle\\HOMESITE\\submission\\11222015_n.csv")

time_taken <- Sys.time() - start

xgb.save(clf, "D:\\kaggle\\HOMESITE\\models\\11212015.R")

importance_matrix <- xgb.importance(feature_names = feature.names, model = clf)

jpeg(filename = "D:\\kaggle\\HOMESITE\\Plots\\imp_11212015_20.jpeg")

xgb.plot.importance(importance_matrix = importance_matrix[1:20, ])

dev.off()

