rm(list = ls())

library(doParallel)

cl <- makeCluster(2)

registerDoParallel(cl)

library(xgboost)

library(readr)

param <- list(
  "objective"  = "binary:logistic"
  
  , "eval_metric" = "auc"
  
  , "eta" = 0.01
  
  , "subsample" = 0.7
  
  , "colsample_bytree" = 0.5
  
  , "min_child_weight" =6
  
  , "max_depth" = 9
  
  , "alpha" = 4
  
  , "nthreads" = 2
  
)

train <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

y <- train$target

train$target <- NULL

train_ID <- train$ID

test <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")

test_ID <- test$ID

train <- train[,-1]

test <- test[,-1]

tmp <- rbind(train, test)

tmp[tmp == -1] = NA

tmp[tmp == ""] = NA

tmp[tmp == "[]"] = NA

tmp_date <- tmp[, grep("JAN1|FEB1|MAR1", tmp)]

tmp_date <- sapply(tmp_date, function(x) strptime(x, "%d%B%y :%H:%M:%S"))

tmp_date = do.call(cbind.data.frame, tmp_date)

ids <- c("VAR_0227", "VAR_0228")

remove_col <- c(names(tmp_date), ids)

tmp <- tmp[, !(names(tmp) %in% (remove_col))]

tmp <- cbind(tmp, tmp_date)

tmp_year <- data.frame(sapply(tmp_date, function(x) year(x)))

names(tmp_year) <- paste("YEAR_", names(tmp_year), sep = "") 

tmp_mday <- data.frame(sapply(tmp_date, function(x) mday(x)))

names(tmp_mday) <- paste("MDAY_", names(tmp_mday), sep = "") 

train <- tmp[1:145231,]

test <- tmp[(nrow(train)+1): nrow(tmp), ]

levels_TR <- unique(train$VAR_0241)

levels_tt <- unique(test$VAR_0241)

train$VAR_0241 <- as.integer(factor(train$VAR_0241, levels=levels_TR))

test$VAR_0241  <- as.integer(factor(test$VAR_0241,  levels=levels_tt))

rm(tmp)
gc()

feature.names <- names(train)

for (f in feature.names) {
  
  if (class(train[[f]])=="character") {
    
    levels <- unique(c(train[[f]], test[[f]]))
    
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
    
  }
}


train[is.na(train)] <- -1

test[is.na(test)] <- -1

benchmark <- read_csv("D:/kaggle/Springleaf/SUBMISSION/second.csv")

first <- read_csv("D:/kaggle/Springleaf/SUBMISSION/third.csv")

second <- read_csv("D:/kaggle/Springleaf/SUBMISSION/third_first.csv")

third <- read_csv("D:/kaggle/Springleaf/SUBMISSION/fourth.csv")

fourth <- read_csv("D:/kaggle/Springleaf/sixth.csv")

fifth <- read_csv("D:/kaggle/Springleaf/eight.csv")

feature_1 <- benchmark$target[1:145231] 

train$feature1 <- feature_1

test$feature1 <- benchmark$target

feature_2 <- first$target[1:145231] 

train$feature2 <- feature_2

test$feature2 <- first$target

feature_3 <- second$target[1:145231] 

train$feature3 <- feature_3

test$feature3 <- second$target

feature_4 <- third$target[1:145231] 

train$feature4 <- feature_4

test$feature4 <- third$target

feature_5 <- fourth$target[1:145231] 

train$feature5 <- feature_5

test$feature5 <- fourth$target



xgtrain = xgb.DMatrix(as.matrix(train), label = y, missing = NA)

gc()

model = xgb.train(
  
  nrounds = 600   # increase for more results at home
  
  , params = param
  
  , data = xgtrain
  ,verbose = 2
  
  
)


rm("train")

gc()

train_val <-predict(model, newdata=xgtrain)

rm("xgtrain")

xgtest <- xgb.DMatrix(as.matrix(test), missing = NA)

preds_out <- predict(model, xgtest)


submission_second <- data.frame(ID=test_ID)

submission_second$target <- preds_out 

write_csv(submission_second, "twelve.csv")
