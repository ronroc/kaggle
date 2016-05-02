require(data.table); require(zoo); require(forecast); require(readr)

#train <- fread("D:/kaggle/Forecasting/DATA/train.csv")

train <- read_csv("D:/kaggle/Forecasting/DATA/train.csv")

#store <- fread("D:/kaggle/Forecasting/DATA/store.csv")

store <- read_csv("D:/kaggle/Forecasting/DATA/store.csv")

#test <- fread("D:/kaggle/Forecasting/DATA/test.csv")

test <- read_csv("D:/kaggle/Forecasting/DATA/test.csv")

str(train)

str(test)

str(store)

train[, Date := as.Date(Date)]

test[, Date := as.Date(Date)]

train <- train[order(Date)]

test <- test[order(Date)]

summary(train)

summary(test)

test[is.na(test$Open)]

#test[is.na(test)] <- 1

#Unique values per column----------------------------------------------------

train[, lapply(.SD, function(x) length(unique(x)))]

test[, lapply(.SD, function(x) length(unique(x)))]

#All test scores are also in train data--------------------------------------

sum(unique(test$Store) %in% unique(train$Store))

sum(!(unique(train$Store) %in% unique(test$Store)))

#percent open in train------------------------------------------------------

table(train$Open) / nrow(train) 

table(test$Open) / nrow(test) 

#percent of time promo in train---------------------------------------------

table(train$Promo) / nrow(train)

table(test$Promo) / nrow(test)

#rmng percentage breakup

for(i in names(train)){

  print(i)
    
  print(table(train[i]) / nrow(train))
  
  print(paste("The length of", i, "is :", length(table(train[i]))))
  
  print("---------------------------------------------------------")
 }

