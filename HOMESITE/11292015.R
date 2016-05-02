require(data.table); require(lubridate); require(caret); require(sqldf); require(xgboost); require(sqldf); require(xlsx); require(Matrix)

train_raw <- fread(input = "D:\\kaggle\\HOMESITE\\Data\\train.csv", data.table = F)

response <- train_raw$QuoteConversion_Flag

response <- as.matrix(response)

response_test = numeric()

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

continous_field <- tmp$SalesField8

tmp$SalesField8 <- NULL

tmp$Original_Quote_Date <- NULL

tmp[is.na(tmp)] <- -1

# counts , two way and three way counts

# some extra features from otto comp

source("utilis.R")

tmp_factors = tmp

len = length(names(tmp_factors))

for (i in 1:len) {
  
  print(paste0( i / (len) *100, "%"))
  
  levels <- unique(tmp_factors[[i]])
  
  tmp_factors[ , i] <- factor(tmp_factors[ , i], levels = levels)
  
}


#########################################################################################################


nms <- combn(names(tmp_factors), 2)

dim(nms)

nms_df <- data.frame(nms) 

len = length(names(nms_df))


for (i in 1:len) {
  
  nms_df[, i] <- as.character(nms_df[, i])
  
}


tmp_count <- data.frame(id = 1:dim(tmp)[1])


for(i in 1:dim(nms_df)[2]){
  
    #new df 
  
  print(((i / dim(nms_df)[2]) * 100 ))
  
  tmp_count[, paste(i, "_two", sep="")] <- my.f2cnt(th2 = tmp, 
                                                    
                                                    vn1 = nms_df[1,i], 
                                                    
                                                    vn2 = nms_df[2,i] )
  
}


######## one way count--------------------------------------------------------------------------------


len = dim(tmp_factors)[2]

for(i in 1:len){
  
  
  print((i / len) * 100 )
  
  tmp_factors$x <- tmp_factors[, i]
  
  sum1 <- sqldf("select x, count(1) as cnt
                
                from tmp_factors  group by 1 ")
  
  tmp1 <- sqldf("select cnt from tmp_factors a left join sum1 b on a.x=b.x")
  
  tmp_count[, paste(names(tmp_factors)[i], "_one", sep="")] <- tmp1$cnt
  
}  


tmp <- cbind(tmp, tmp_count)

#######################################################################################################
#######################################################################################################


dummies <- sparse.model.matrix( ~. - 1, data = tmp )

# doubt for creation of sparse matrix do cols have to be factors # check

sparse.model.matrix( ~. -1, data = mtcars)

# no change has to be factors

mtcars$mpg <- as.factor(mtcars$mpg)

sparse.model.matrix( ~. -1, data = mtcars)

#######################################################################################################
#######################################################################################################


len = length(names(tmp))

for (i in 1:len) {
  
  print(paste0( i / (len) *100, "%"))
  
  levels <- unique(tmp[[i]])
  
  tmp[ , i] <- factor(tmp[ , i], levels = levels)
  
}




# AFTER THIS STEP COMBINE "sales field 8" _continous field by first converting it into  a matrix

# and then cbind to the final tmp



dummies <- sparse.model.matrix(~ . -1 , data = tmp, 
                               
                               contrasts.arg = lapply(tmp, contrasts, contrasts=FALSE))





train = dummies[c(1:260753), ]


test <- dummies[c(260754:434589), ]

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


set.seed(11*28*15)


#cv <- xgb.cv(params = param, data = dtrain, 

#             nrounds = 1900, 

#             nfold = 4, 

#             showsd = T, 

#             maximize = F)


start <- Sys.time()


clf <- xgb.train(   params              = param,
                    
                    data                = dtrain,
                    
                    nrounds             = 1900,
                    
                    verbose             = 1,  #1
                    
                    #early.stop.round    = 150,
                    
                    #watchlist           = watchlist,
                    
                    maximize            = FALSE,
                    
                    nthread = 2)



pred <- predict(clf, (test))


submission <- data.frame(QuoteNumber = id, QuoteConversion_Flag = pred)


write_csv(submission, "D:\\kaggle\\HOMESITE\\submission\\11292015.csv")

total_time <- Sys.time() - start


# try different objective functions then combine (bagging) for better results

