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

tmp$week <- week((as.Date(tmp$Original_Quote_Date)))

tmp$date <- (((tmp$year * 52 ) + tmp$week) %% 4)

continous_field <- tmp$SalesField8

tmp$SalesField8 <- NULL

tmp$Original_Quote_Date <- NULL

tmp[is.na(tmp)] <- -1


feature.names <- names(tmp)


for (f in feature.names) {
  
  if (class(tmp[[f]]) == "character") {
    
    levels <- unique(c(tmp[[f]]))
    
    tmp[[f]] <- as.integer(factor(tmp[[f]], levels=levels))
    
    
  }
}


#########################################################################################################


tmp_factors = tmp

len = length(names(tmp_factors))

for (i in 1:len) {
  
  print(paste0( i / (len) *100, "%"))
  
  levels <- unique(tmp_factors[[i]])
  
  tmp_factors[ , i] <- factor(tmp_factors[ , i], levels = levels)
  
}


## one way count 

len = dim(tmp_factors)[2]

tmp_count <- data.frame(id = 1:dim(tmp)[1])

for(i in 1:len){
  
  
  print((i / len) * 100 )
  
  tmp_factors$x <- tmp_factors[, i]
  
  sum1 <- sqldf("select x, count(1) as cnt
                
                from tmp_factors  group by 1 ")
  
  tmp1 <- sqldf("select cnt from tmp_factors a left join sum1 b on a.x=b.x")
  
  tmp_count[, paste(names(tmp_factors)[i], "_one", sep="")] <- tmp1$cnt
  
}  


tmp <- cbind(tmp, tmp_count)



len = length(names(tmp))

for (i in 1:len) {
  
  print(paste0( i / (len) *100, "%"))
  
  levels <- unique(tmp[[i]])
  
  tmp[ , i] <- factor(tmp[ , i], levels = levels)
  
}


tmp$sales8 <- continous_field



# ERROR : contrasts can only be applied to factors with 2 or more levels

del = c(rep(0, length(names(tmp))))

len = length(names(tmp))

for (i in 1:len) {
  
  print(paste0( i / (len) *100, "%"))
  
  levels <- length(unique(tmp[[i]]))
  
  if(levels == 1) del <- c(del, names(tmp)[i])

}

del = del[del!= 0]

tmp$GeographicField10A_one <- NULL

tmp <- tmp[, ! (names(tmp) %in% del) ]

dummies <- sparse.model.matrix(~ . -1 , data = tmp)

train = dummies[c(1:260753), ]


test <- dummies[c(260754:434589), ]


dtrain <- xgb.DMatrix(data = train, label=response)


cl <- makeCluster(2); registerDoParallel(cl)


set.seed(12*02*15)


#cv <- xgb.cv(params = param, data = dtrain, 

#             nrounds = 1900, 

#             nfold = 4, 

#             showsd = T, 

#             maximize = F)


param <- list(  objective           = "binary:logistic", 
                
                booster = "gbtree",
                
                eval_metric = "auc",
                
                eta                 =  0.015, 
                
                max_depth           = 22, 
                
                subsample           = 0.7, 
                
                colsample_bytree    = 0.7 ,
                
                num_parallel_tree   = 2,
                
                min_child_weight =  3
                
)


start <- Sys.time()


clf <- xgb.train(   params              = param, 
                    
                    data                = dtrain, 
                    
                    nrounds             = 2125, 
                    
                    verbose             = 1,  #1
                    
                    #early.stop.round    = 150,
                    
                    #watchlist           = watchlist,
                    
                    maximize            = FALSE
)


pred <- predict(clf, (test))


submission <- data.frame(QuoteNumber = id, QuoteConversion_Flag = pred)


write_csv(submission, "D:\\kaggle\\HOMESITE\\submission\\12022015.csv")

total_time <- Sys.time() - start

