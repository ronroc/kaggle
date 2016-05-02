set.seed(01*14*2016)

require(data.table); require(lubridate); require(caret); require(sqldf); require(xgboost); require(xlsx); require(dplyr); require(readr); require(doParallel)

#rm(list = ls())

train_raw <- fread(input = "D:\\kaggle\\HOMESITE\\Data\\train.csv", data.table = F)

response <- train_raw$QuoteConversion_Flag

train_raw$QuoteConversion_Flag <- NULL

train_raw$QuoteNumber <- NULL


test_raw <- fread(input = "D:\\kaggle\\HOMESITE\\Data\\test.csv", data.table = F)

id <- test_raw$QuoteNumber

test_raw$QuoteNumber <- NULL

continous_vars <- c("Field8", "Field9", "Field11", "SalesField8", "Original_Quote_Date", "date")


# categorical and discrete are grouped into a single group

categorical_vars <- c()

remove_vars <- c("PropertyField6", "GeographicField10A")

tmp <- rbind(train_raw, test_raw)

tmp <- tmp[, !(names(tmp) %in% remove_vars)]


tmp$Original_Quote_Date <- as.Date(tmp$Original_Quote_Date)

tmp$month <- as.integer(format(tmp$Original_Quote_Date, "%m"))

tmp$year <- as.integer(format(tmp$Original_Quote_Date, "%y"))

tmp$day <- weekdays(as.Date(tmp$Original_Quote_Date))

tmp$week <- week((as.Date(tmp$Original_Quote_Date)))

tmp$date <- (((tmp$year * 52 ) + tmp$week) %% 4)

#########################################################################################


a <- lapply(tmp, function(x) length(unique(x)))

len_unique <- rep(0, ncol(tmp))

for(i in 1:length(a))
{
  if(a[[i]] > 25 & a[[i]] < 30) {
    
    len_unique[i] <- (names(a[i]))
  }
  
}

len_unique <- len_unique[len_unique != 0]

len_unique <- len_unique[!(len_unique %in% continous_vars)]

tmp_unique <- tmp[, len_unique]



tmp[is.na(tmp)] <- -1

row_NA <- apply(tmp, 1, function(x) sum(x == -1))

tmp$row_NA <- row_NA


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


#tmp_char <- tmp[, names(tmp_unique)]

tmp_char <- tmp_unique

rm(tmp_unique)

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


#################################################################################################


#a <- lapply(tmp, function(x) length(unique(x)))


high_card <- rep(0, ncol(tmp))

for(i in 1:length(a))

  {
  
  if(a[[i]] > 25) {
    
    high_card[i] <- (names(a[i]))
  }
  
}

high_card <- high_card[high_card != 0]

high_card <- high_card[!(high_card %in% continous_vars)]


#tmp_high_card <- tmp[, high_card]

high_card <- c("PersonalField16", "PersonalField17", "PersonalField14", "PersonalField18", 
               
               "PersonalField19" )


tmp_high_card <- tmp[, high_card]


str(tmp_high_card, list.len = 999)


cat("assuming text variables are categorical & replacing them with numeric ids\n")

for (f in names(tmp_high_card)) {
  
  if (class(tmp_high_card[[f]])=="character") {
    
    levels <- unique(c(tmp_high_card[[f]]))
    
    tmp_high_card[[f]] <- as.integer(factor(tmp_high_card[[f]], levels=levels))
    
  }
}




# converting to factors

len = length(names(tmp_high_card))

for (i in 1:len) {
  
  print(paste0( i / (len) *100, "%"))
  
  tmp_high_card[ , i] <- as.factor(tmp_high_card[ , i])
  
}


str(tmp_high_card, list.len = 999)


# counts ;


tmp_factors <- tmp_high_card

rm(tmp_high_card)

# 2 way count

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
  
  print(paste0(((i / dim(nms_df)[2]) * 100), "%"))
  
  tmp_count[, paste(i, "_two", sep="")] <- my.f2cnt(th2 = tmp_factors,
                                                                   
                                                                   vn1 = nms_df[1,i],
                                                                   
                                                                   vn2 = nms_df[2,i] )
  
}


#3 way count

nms <- combn(names(tmp_factors), 3)

dim(nms)

nms_df <- data.frame(nms); #nms_df <- nms_df[ c(1:3), c(1:100)]


len = length(names(nms_df))

for (i in 1:len) {
  
  print(paste0(((i / len) * 100), "%"))
  
  nms_df[, i] <- as.character(nms_df[, i])
  
}

for(i in 1:dim(nms_df)[2]){
  
  #new df
  
  print(paste0(((i / dim(nms_df)[2]) * 100), "%"))
  
  tmp_count[, paste(i, "_three", sep="")] <- my.f3cnt(th2 = tmp_factors,
                                                                     
                                                                     vn1 = nms_df[1,i],
                                                                     
                                                                     vn2 = nms_df[2,i],
                                                                     
                                                                     vn3 = nms_df[3,i])
  
}


#one way count

len = length(names(tmp_factors))

for(i in 1:len){
  
  
  print(paste0(((i / len) * 100), "%") )
  
  tmp_factors$x <- tmp_factors[, i]
  
  sum1 <- sqldf("select x, count(1) as cnt
                
                from tmp_factors  group by 1 ")
  
  tmp1 <- sqldf("select cnt from tmp_factors a left join sum1 b on a.x=b.x")
  
  tmp_count[, paste(names(tmp_factors)[i], "_cnt", sep="")] <- tmp1$cnt
  
}

tmp_count$id <- NULL

##################################################################################################

tmp_cont <- tmp[, continous_vars]

for (f in names(tmp_cont)) {
  
  if (class(tmp_cont[[f]])=="character") {
    
    levels <- unique(c(tmp_cont[[f]]))
    
    tmp_cont[[f]] <- as.integer(factor(tmp_cont[[f]], levels=levels))
    
  }
}

str(tmp_cont)
tmp_cont$Original_Quote_Date <- NULL

tmp_pre <-  preProcess(tmp_cont, method = ("BoxCox"))

tmp_cont_new <- predict(tmp_pre, tmp_cont)

gc()

###################################################################################################

tmp <- tmp[, !(names(tmp) %in% c(continous_vars))]

tmp_new <- cbind(tmp, tmp_char, tmp_cont_new, tmp_count)


rm(test_raw); rm(train_raw); rm(tmp_char); rm(nms); rm(tmp_count); rm(tmp_factors); rm(tmp_cont_new)

rm(tmp_cont); rm(tmp1); rm(tmp)


#############################################################################################


# add interaction terms


imp <- read_csv("D:\\kaggle\\HOMESITE\\FEATURE_IMP\\12062015_1.csv")

top_50 <- imp$Feature[1:5]

tmp_int <- tmp_new[, top_50]

for (f in top_50) {
  
  if (class(tmp_int[[f]])=="character") {
    
    levels <- unique(tmp_int[[f]])
    
    tmp_int[[f]] <- as.integer(factor(tmp_int[[f]], levels=levels))
  }
  
}


gc()

rm(imp);


#############################################################################################

# plus interaction


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


############################################################################################


# create - interaction features


# add interaction terms



imp <- read_csv("D:\\kaggle\\HOMESITE\\FEATURE_IMP\\12062015_1.csv")

top_50 <- imp$Feature[1:5]

tmp_int <- tmp_new[, top_50]

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


#############################################################################################


# create * interaction features


# add interaction terms


imp <- read_csv("D:\\kaggle\\HOMESITE\\FEATURE_IMP\\12062015_1.csv")

top_50 <- imp$Feature[1:5]

tmp_int <- tmp_new[, top_50]

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


#############################################################################################

# create ^ interaction features

# not using division interaction features - NA's


imp <- read_csv("D:\\kaggle\\HOMESITE\\FEATURE_IMP\\12062015_1.csv")

top_50 <- imp$Feature[1:5]

tmp_int <- tmp_new[, top_50]

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
    
    var.new <- paste0(var.x, '_order_', var.y)
    
    tmp_int[, paste0(var.new)] <- (tmp_int[, i] * tmp_int[, j]) ^ 2
    
  }
}


#############################################################################################

# NA terms test

a <- lapply(tmp_int, function(x) sum(is.na(x)))


len_unique <- rep(0, ncol(tmp_int))

for(i in 1:length(a))
{
  if(a[[i]]  != 0) {
    
    len_unique[i] <- (names(a[i]))
  }
  
}

len_unique <- len_unique[len_unique != 0]



tmp_new <- cbind(tmp_new, tmp_int)

rm(tmp_int)

gc()


##################################################################################################


# create 3^ interaction features

# not using division interaction features - NA's


imp <- read_csv("D:\\kaggle\\HOMESITE\\FEATURE_IMP\\12062015_1.csv")

top_50 <- imp$Feature[1:5]

tmp_int <- tmp_new[, top_50]

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
    
    var.new <- paste0(var.x, '_orderss_', var.y)
    
    tmp_int[, paste0(var.new)] <- (tmp_int[, i] * tmp_int[, j]) ^ 3
    
  }
}



tmp_new <- cbind(tmp_new, tmp_int)

rm(tmp_int)

gc()


##################################################################################################

# getting NA's with the below code

# create 4^ interaction features

# not using division interaction features - NA's


imp <- read_csv("D:\\kaggle\\HOMESITE\\FEATURE_IMP\\12062015_1.csv")

top_50 <- imp$Feature[1:5]

tmp_int <- tmp_new[, top_50]

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
    
    var.new <- paste0(var.x, 'four_orderss_', var.y)
    
    tmp_int[, paste0(var.new)] <- (tmp_int[, i] * tmp_int[, j]) ^ 4
    
  }
}

a <- lapply(tmp_int, function(x) sum(is.na(x)))


len_unique <- rep(0, ncol(tmp_int))

for(i in 1:length(a))
{
  if(a[[i]]  != 0) {
    
    len_unique[i] <- (names(a[i]))
  }
  
}

len_unique <- len_unique[len_unique != 0]

# remove NA cols and append back to tmp_new

tmp_int <- tmp_int[ , !(names(tmp_int) %in% len_unique)]

tmp_new <- cbind(tmp_new, tmp_int)

rm(tmp_int)

gc()


##############################################################################################

tmp_new <- tmp_new[, !(names(tmp_new) %in% top_50)]

imp <- read_csv("D:\\kaggle\\HOMESITE\\FEATURE_IMP\\12062015_1.csv")

top_50 <- imp$Feature[1:5]

tmp_int <- tmp[, top_50]

for (f in top_50) {
  
  if (class(tmp_int[[f]])=="character") {
    
    levels <- unique(tmp_int[[f]])
    
    tmp_int[[f]] <- as.integer(factor(tmp_int[[f]], levels=levels))
  }
  
}


tmp_new <- cbind(tmp_new, tmp_int)

rm(tmp_int); rm(tmp)


##################################################################################

rm(tmp); rm(test_raw); rm(train_raw); rm(tmp_char); rm(tmp_int); rm(imp)


train <- tmp_new[c(1:260753), ]

test <- tmp_new[c(260754:434589), ]

rm(tmp_new)

gc()

#train[is.na(train)] <- -1

#test[is.na(test)] <- -1


###################################################################################################

feature.names <- names(train)

h<-sample(nrow(train),10000)

dval<-xgb.DMatrix(data=data.matrix(train[h,]),label=response[h])

dtrain<-xgb.DMatrix(data=data.matrix(train[-h,]),label=response[-h])

#dtrain<-xgb.DMatrix(data=data.matrix(train),label=response, )

watchlist<-list(val=dval,train=dtrain)

param <- list(  objective           = "binary:logistic",
                
                booster = "gbtree",
                
                eval_metric = "auc",
                
                eta                 = 0.023, # 0.06, #0.01,
                
                max_depth           = 6, #changed from default of 8
                
                subsample           = 0.83, # 0.7
                
                colsample_bytree    = 0.77, # 0.7
                
                num_parallel_tree = 2,
                
                min_child_weight = 5
                
)

start <- Sys.time()

require(doParallel)

cl <- makeCluster(2); registerDoParallel(cl)

set.seed(1*14*16)

#cv <- xgb.cv(params = param, data = dtrain,

#            nrounds = 1800,

#           nfold = 4,

#          showsd = T,

#         maximize = F)

clf <- xgb.train(   params              = param,
                    
                    data                = dtrain,
                    
                    nrounds             = 3500,
                    
                    verbose             = 1,  #1
                    
                    #early.stop.round    = 150,
                    
                    watchlist           = watchlist,
                    
                    maximize            = T,
                    
                    nthread = 2)

xgb.save(clf, "D:\\kaggle\\HOMESITE\\models\\01032016_1.R")

rm(submission)

pred <- predict(clf, data.matrix(test[,feature.names]))

submission <- data.frame(QuoteNumber = id, QuoteConversion_Flag = pred)

write_csv(submission, "D:\\kaggle\\HOMESITE\\submission\\12072015\\01032016_2.csv")

time_taken <- Sys.time() - start


pred <- predict(clf, data.matrix(test[,feature.names]), ntreelimit = 1500)

submission <- data.frame(QuoteNumber = id, QuoteConversion_Flag = pred)

write_csv(submission, "D:\\kaggle\\HOMESITE\\submission\\12072015\\01032016_3.csv")



pred <- predict(clf, data.matrix(test[,feature.names]), ntreelimit = 1000)

submission <- data.frame(QuoteNumber = id, QuoteConversion_Flag = pred)

write_csv(submission, "D:\\kaggle\\HOMESITE\\submission\\12072015\\01142016.csv")
