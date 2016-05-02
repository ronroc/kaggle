
require(data.table); require(lubridate); require(caret); require(sqldf); require(xgboost); require(xlsx); require(dplyr); require(readr); require(doParallel)

#rm(list = ls())

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


tmp[is.na(tmp)] <- -1

row_NA <- apply(tmp, 1, function(x) sum(x == -1))

# feat_filled = as.integer(rowSums(tmp[, 1:ncol(tmp)] != 0)) 

# seperate character columns

char <- rep(0, length(names(tmp)))

for(i in names(tmp))
  
{
  if(class(tmp[, i]) == "character"){
    
    char <- c(char, i)
  }
  
  char <- char[char != 0 ]
}

# convert char columns to factors to dummify them

tmp_char <- tmp[, char]


for(f in names(tmp_char)){
  
  levels <- unique(tmp_char[, f])
  
  tmp_char[,f] <- factor(tmp_char[,f], levels = levels)
  
}


dummies <- dummyVars( ~., data = tmp_char)

tmp_char <- predict(dummies, newdata = tmp_char)

tmp_char <- data.frame(tmp_char)

rm(dummies)

gc()


for (f in names(tmp)) {
  
  if (class(tmp[[f]])=="character") {
    
    levels <- unique(tmp[[f]])
    
    tmp[[f]] <- as.integer(factor(tmp[[f]], levels=levels))
    
  }
  
}




tmp_new <- cbind(tmp, tmp_char)


rm(test_raw); rm(train_raw); rm(tmp_char)


##############################################################################################

# to create a tsne of unique elements with length less than or equal to 20

require(Rtsne)

# tmpI = apply(train_raw,2,function(x) length(unique(x)))

tmpI = lapply(train_raw, function(x) length(unique(x)))

tmpI_n <- rep(0, length(tmpI))


for(i in 1:length(tmpI))
{
  for(j in 2:20)
{
  if(tmpI[[i]] == j) tmpI_n[i] <- names(tmpI)[i]
     
 }

  }
  

tmpI_n <- tmpI_n[tmpI_n != 0] 

train = train_raw[ ,tmpI_n]

test = test_raw[ ,tmpI_n]


gc()


both = rbind(train,test)

rm(train); rm(test)

gc()


for (j in 1:ncol(both)) {
  
  both[,j] = as.numeric(as.factor(both[,j]))
  
}


both[is.na(both)]=2

gc()

both = as.matrix(both)

gc()

both = both-1

tsne <- Rtsne(both, check_duplicates = FALSE, pca = FALSE, verbose=TRUE,
              
              perplexity=30, theta=0.5, dims=2, max_iter = 500)


tmp_new <- cbind(tmp_new, tsne$Y)

# add interaction terms


imp <- read_csv("D:\\kaggle\\HOMESITE\\FEATURE_IMP\\12062015_1.csv")

top_50 <- imp$Feature[1:5]

tmp_int <- tmp[, top_50]

for (f in top_50) {
  
  if (class(tmp_int[[f]])=="character") {
    
    levels <- unique(tmp_int[[f]])
    
    tmp_int[[f]] <- as.integer(factor(tmp_int[[f]], levels=levels))
  }
  
}


gc()

rm(imp); 

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

tmp_new <- cbind(tmp_new, tmp_int)
rm(tmp_int)
gc()

# create - interaction features

# add interaction terms


imp <- read_csv("D:\\kaggle\\HOMESITE\\FEATURE_IMP\\12062015_1.csv")

top_50 <- imp$Feature[1:5]

tmp_int <- tmp[, top_50]

for (f in top_50) {
  
  if (class(tmp_int[[f]])=="character") {
    
    levels <- unique(tmp_int[[f]])
    
    tmp_int[[f]] <- as.integer(factor(tmp_int[[f]], levels=levels))
  }
  
}


gc()

rm(imp); 



for (i in 1:ncol(tmp_int)) {
  
  for (j in (i + 1) : (ncol(tmp_int) + 1)) {
    
    var.x <- colnames(tmp_int)[i]
    
    var.y <- colnames(tmp_int)[j]
    
    var.new <- paste0(var.x, '_minus_', var.y)
    
    tmp_int[ , paste0(var.new)] <- tmp_int[, i] - tmp_int[, j]
    
  }
}


gc()

tmp_new <- cbind(tmp_new, tmp_int)
rm(tmp_int)
gc()




# create * interaction features


# add interaction terms


imp <- read_csv("D:\\kaggle\\HOMESITE\\FEATURE_IMP\\12062015_1.csv")

top_50 <- imp$Feature[1:5]

tmp_int <- tmp[, top_50]

for (f in top_50) {
  
  if (class(tmp_int[[f]])=="character") {
    
    levels <- unique(tmp_int[[f]])
    
    tmp_int[[f]] <- as.integer(factor(tmp_int[[f]], levels=levels))
  }
  
}


gc()

rm(imp); 


for (i in 1:ncol(tmp_int)) {
  
  for (j in (i + 1) : (ncol(tmp_int) + 1)) {
    
    var.x <- colnames(tmp_int)[i]
    
    var.y <- colnames(tmp_int)[j]
    
    var.new <- paste0(var.x, '_mult_', var.y)
    
    tmp_int[ , paste0(var.new)] <- tmp_int[, i] * tmp_int[, j]
    
  }
}

tmp_new <- cbind(tmp_new, tmp_int)
rm(tmp_int)
gc()




tmp_int <- tmp_int[, !(names(tmp_int) %in% top_50)]

gc()


tmp_new <- cbind(tmp_new, tmp_int)


##################################################################################

rm(tmp); rm(test_raw); rm(train_raw); rm(tmp_char); rm(tmp_int); rm(imp)

tmp_new$row_NA <- row_NA

tmp_new$feat <- feat_filled


train <- tmp_new[c(1:260753), ]

test <- tmp_new[c(260754:434589), ]

rm(tmp_new)

gc()

#train[is.na(train)] <- -1

#test[is.na(test)] <- -1

gc()

###################################################################################################

feature.names <- names(train)

h<-sample(nrow(train),2000)

dval<-xgb.DMatrix(data=data.matrix(train[h,]),label=response[h])

#dtrain<-xgb.DMatrix(data=data.matrix(train[-h,]),label=response[-h])

dtrain<-xgb.DMatrix(data=data.matrix(train),label=response)

watchlist<-list(val=dval,train=dtrain)

param <- list(  objective           = "binary:logistic", 
                
                booster = "gbtree",
                
                eval_metric = "auc",
                
                eta                 = 0.023, # 0.06, #0.01,
                
                max_depth           = 6, #changed from default of 8
                
                subsample           = 0.83, # 0.7
                
                colsample_bytree    = 0.77, # 0.7
                
                num_parallel_tree = 2
                
)

start <- Sys.time()

require(doParallel)

cl <- makeCluster(2); registerDoParallel(cl)

set.seed(12*12*15)

#cv <- xgb.cv(params = param, data = dtrain, 

#            nrounds = 1800, 

#           nfold = 4, 

#          showsd = T, 

#         maximize = F)

clf <- xgb.train(   params              = param,
                    
                    data                = dtrain,
                    
                    nrounds             = 3000,
                    
                    verbose             = 1,  #1
                    
                    #early.stop.round    = 400,
                    
                    watchlist           = watchlist,
                    
                    maximize            = FALSE,
                    
                    nthread = 2)


pred <- predict(clf, data.matrix(test[,feature.names]))

submission <- data.frame(QuoteNumber = id, QuoteConversion_Flag = pred)

write_csv(submission, "D:\\kaggle\\HOMESITE\\submission\\12092015\\12092015_5.csv")

time_taken <- Sys.time() - start


pred <- predict(clf, data.matrix(test[,feature.names]), ntreelimit = 1000)

submission <- data.frame(QuoteNumber = id, QuoteConversion_Flag = pred)

write_csv(submission, "D:\\kaggle\\HOMESITE\\submission\\12092015\\12092015_3.csv")
