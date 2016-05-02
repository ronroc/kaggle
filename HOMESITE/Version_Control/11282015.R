# this script tries one hot encoding and forward feature selection on the original feature set

# not blowing up the feature space


require(data.table); require(lubridate); require(caret); require(sqldf); require(xgboost); require(sqldf); require(xlsx); require(Matrix)

train_raw <- fread(input = "D:\\kaggle\\HOMESITE\\Data\\train.csv", data.table = F)

response <- train_raw$QuoteConversion_Flag

response <- as.matrix(response)

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

feature.names <- names(tmp)


for (f in feature.names) {
  
  if (class(tmp[[f]]) == "character") {
    
    levels <- unique(c(tmp[[f]]))
    
    tmp[[f]] <- as.integer(factor(tmp[[f]], levels=levels))
    
  
  }
}


len = length(names(tmp))

for (i in 1:len) {
  
  print(paste0( i / (len) *100, "%"))
  
  levels <- unique(tmp[[i]])
  
  tmp[ , i] <- factor(tmp[ , i], levels = levels)
  
}



tmp_mat <- as.matrix(tmp)



# running into memory issues

# double <- groupData(tmp, 2)

# tmp_mat <- cbind(tmp)#, double)

tmp_mat_train <- tmp_mat[1:260753 , ]

tmp_mat_test = tmp_mat[(260753+1):434589, ]

response_test = numeric()

# for sparse matrix factors  required



# need tmp_mat_train, tmp_mat_test, response, response_test, salesfield8 : remove everything

keep <- c("tmp_mat_train", "tmp_mat_test", "response", "response_test", "continous_field", "id")

toremove <- setdiff(ls(), keep)

rm(list = c(toremove, "toremove"))



# need tmp_mat_train, tmp_mat_test, response, response_test : remove everything

#function for forward stepwise logistic regression fitting to determine optimal features to encode


set.seed(11*29*2015)

require(glmnet, quietly = T)

require(Matrix, quietly = T)

#initialize optimization metrics-----------------------------------------------------------------------------------------------

cv_max_auc = 0.5 #target minimum.... represents random guess

cv_fold_auc = numeric()

cv_train_auc = numeric()

#initialize feature holding-----------------------------------------------------------------------------------------------------

best_col = numeric()

num_features = numeric()

# big inefficient for loop that does everything -------------------------------------------------------------------------------

for(i in 1:ncol(tmp_mat_train)) 
  
{
  
  # add columns selected + iteration column
  
  colName = colnames(tmp_mat_train)[c(best_col, i)] 
  
  vars = as.data.frame(tmp_mat_train[,c(best_col, i)])
  
  colnames(vars) = colName
  
  #encode into sparse model               
  
  vars = sparse.model.matrix(~ . - 1, data = vars)                     
  
  #10 fold logistic regression w/ lasso reg ran to obtain max mean AUC on validation set
  
  for(j in 1:10) 
    
  {
    
    cv_train = cv.glmnet(x = vars, y = response[,1], family = "binomial", type.measure = "auc")
    
    cv_fold_auc[j] = max(cv_train$cvm)
    
  }
  
  cv_train_auc[i] = mean(cv_fold_auc)
  
  #reset cv fold auc
  
  cv_fold_auc = numeric()
  
  #determining if new column is useful.  if so, adding to the model and raising auc bar
  
  if(cv_train_auc[i] > cv_max_auc)
    
  {
    
    #for next iteration: know indecies of the columns to keep
    
    best_col = c(best_col, i)
    
    #store how many important features from the current set to plot against auc
    
    best_features = which(coef(cv_train, s = cv_train$lambda.min) > 0)
    
    num_features[i] = length(best_features)
    
    #raise auc bar
    
    cv_max_auc = cv_train_auc[i]
    
  }
  
  #live update
  
  for(k in 1)
    
  {
    
    print(cat('Feature Loop', i, 'complete.  Max validation AUC:', cv_max_auc, 'Number of features:', num_features[i]))
    
  }
  
}

print(best_col) 

tmp_mat_train = tmp_mat_train[,best_col]


# AFTER THIS STEP COMBINE "sales field 8" _continous field by first converting it into  a matrix

# and then cbind to the final tmp_mat_train


# add model training step


train = tmp_mat_train[1:260753 ,  ]


test <- tmp_mat_train[(260753+1):434589, ]


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


write_csv(submission, "D:\\kaggle\\HOMESITE\\submission\\11282015.csv")

total_time <- Sys.time() - start

##############################################################################################################################

# new approach---------------------------------------------------------------------------------------------------------------

# two way and three way counts

# sparse matrix

# run lasso, same step as listed above

# run xgboost on the selected features

#############################################################################################################################

# new appraoch ------------------------------------------------------------------------------------------------------------

# factor to integer

### Creates a set of new columns for your data set that summarize factor levels by descriptive statistics such as mean,

### sd, skewness etc.

### Variables : train = training set

###             test = test set

###             response = response variable (Hazard in this competition)

###             variables = vector of column names you wise to summarize (T1_V4 for example). Must be strings.

###             metrics = vector of desctriptive statistics you wish to compute for each factor level. Must Be Strings.

### ex: factorToNumeric(train, test, "Hazard", "T1_V4", "mean") will return a column of hazard means by factor level

###     in column T1_V4

### You can specify the test parameter as train to get a column to add to your training set

###### REQUIRES package qdapTools  #####

library(qdapTools)



factorToNumeric <- function(train, test, response, variables, metrics){
  
  temp <- data.frame(c(rep(0,nrow(test))), row.names = NULL)
  
  for (variable in variables){
    
    for (metric in metrics) {
      
      x <- tapply(train[, response], train[,variable], metric)
      
      x <- data.frame(row.names(x),x, row.names = NULL)
      
      temp <- data.frame(temp,round(lookup(test[,variable], x),2))
      
      colnames(temp)[ncol(temp)] <- paste(metric,variable, sep = "_")
      
    }
    
  }
  
  return (temp[,-1])
  
}



#### Sample Usage #### 

train <- read.csv("../input/train.csv")

test <- read.csv("../input/test.csv")

#### Returns mean, median, and sd for factor T1_V4 by factor level to be added to the training set.

data.frame(head(train$T1_V4))

head(factorToNumeric(train, train, "Hazard", "T1_V4", c("mean","median","sd")))

#### Returns mean, median, and sd for factor T1_V4 by factor level to be added to the test set.

data.frame(head(test$T1_V4))

head(factorToNumeric(train, test, "Hazard", "T1_V4", c("mean","median","sd")))

##############################################################################################################################

# new approach--------------------------------------------------------------------------------------------------------------
