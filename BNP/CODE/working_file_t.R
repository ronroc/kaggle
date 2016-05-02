# load libraries------------------------------------------------------------------------------------------

require(data.table); require(xgboost)

train_raw <- fread("D:\\kaggle\\BNP\\DATA\\train.csv", data.table = F)

test_raw <- fread("D:\\kaggle\\BNP\\DATA\\test.csv", data.table = F)

train_ID <- train_raw$ID 

test_ID <- test_raw$ID

response <- train_raw$target

train_raw$ID <- NULL; test_raw$ID <- NULL; train_raw$target <- NULL

tmp <- rbind(train_raw, test_raw)

# transform data step------------------------------------------------------------------------------------

train <- tmp[1:nrow(train_raw), ]

test <- tmp[(nrow(train_raw) + 1) : nrow(tmp), ]

# optimise hyper params for model------------------------------------------------------------------------

# seperate 50% of data for train

# 25% for tuning

# 25% for validation

tune <-  sample(x = nrow(train), size = 0.25 * nrow(train))

index <- [1:nrow(train) %in% tune]

