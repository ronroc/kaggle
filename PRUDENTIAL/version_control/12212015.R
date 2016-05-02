library(readr); library(xgboost); require(sqldf); require(data.table)

# Set a random seed for reproducibility

rm(list = ls())

cat("reading the train and test data\n")

set.seed(12222015)

train_raw <- fread("D:\\kaggle\\PRUDENTIAL\\Data\\train.csv", data.table = F)

test_raw  <- read_csv("D:\\kaggle\\PRUDENTIAL\\Data\\test.csv")

#feature.names <- names(train)[2:ncol(train)-1]

response <- train_raw$Response

train_raw$Response <- NULL

tmp <- rbind(train_raw, test_raw)

# seperate categorical and continous vars

categorical_var <- c(paste("Product_Info_", c(1:3,5:7), sep=""), 
                     
                     paste("Employment_Info_", c(2,3,5), sep=""),
                     
                     paste("InsuredInfo_", 1:7, sep=""), 
                     
                     paste("Insurance_History_", c(1:4,7:9), sep=""), 
                     
                     "Family_Hist_1", 
                     
                     paste("Medical_History_", c(2:14, 16:23, 25:31, 33:41), sep="")
                     
)

continous_var <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", 
                   
                   "Employment_Info_4","Employment_Info_6", "Insurance_History_5", 
                   
                   "Family_Hist_2", "Family_Hist_3", "Family_Hist_4","Family_Hist_5"
                   
)


discrete_var <- c("Medical_History_1", "Medical_History_15", "Medical_History_24", "Medical_History_32", 
                  
                  paste("Medical_Keyword_", 1:48, sep="")
                  
)


factor_names <- c(categorical_var, "Medical_History_1", "Medical_History_15", "Medical_History_24", "Medical_History_32")

tmp_factors = tmp[ , factor_names] 

tmp_factors[is.na(tmp_factors)] <- -1


rm(train_raw); rm(test_raw)

tmp_dummy <- tmp[, categorical_var]


for(f in names(tmp_dummy)){
  
  levels <- unique(tmp_dummy[, f])
  
  tmp_dummy[,f] <- factor(tmp_dummy[,f], levels = levels)
  
}

require(caret)

dummies <- dummyVars( ~., data = tmp_dummy)

tmp_dummy <- predict(dummies, newdata = tmp_dummy)

tmp_dummy <- data.frame(tmp_dummy)

rm(dummies)

tmp_new <- cbind(tmp, tmp_dummy)


rm(tmp); rm(tmp_dummy); rm(tmp_factors)  

gc()

tmp_new[is.na(tmp_new)] <- -1

train <- tmp_new[c(1:59381),]

test <- tmp_new[c(59382:79146),]

rm(tmp_new)

cat("assuming text variables are categorical & replacing them with numeric ids\n")

feature.names <- names(train)[-1]

for (f in feature.names) {
  
  if (class(train[[f]])=="character") {
    
    levels <- unique(c(train[[f]], test[[f]]))
    
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}


train[is.na(train)] <- -1

test[is.na(test)] <- -1


cat("training a XGBoost classifier\n")

xgbtrain <- xgb.DMatrix(data.matrix(train[, -c(1)]) , label = response )


param <- xgboost(data        = xgbtrain,
  
              eta         = 0.02,
              
              nrounds = 2000,
            
              depth       = 21,
            
              objective   = "count:poisson",
            
              colsample_bytree=0.65,
            
              min_child_weight=3,
            
              subsample=0.7,
            
              verbose = 1, 
            
              silent = 0,
              
              eval.metric = "rmse"
              
              )



ScoreQuadraticWeightedKappa = function (rater.a , rater.b, 
                                        min.rating,
                                        max.rating) {
  
  if (missing(min.rating)) {
    min.rating = min(min(rater.a),min(rater.b))
  }
  if (missing(max.rating)) {
    max.rating = max(max(rater.a),max(rater.b))
  }
  
  rater.a = factor(rater.a, levels=min.rating:max.rating)
  rater.b = factor(rater.b, levels=min.rating:max.rating)
  
  #pairwise frequencies
  confusion.mat = table(data.frame(rater.a, rater.b))
  confusion.mat = confusion.mat / sum(confusion.mat)
  
  #get expected pairwise frequencies under independence
  histogram.a = table(rater.a) / length(table(rater.a))
  histogram.b = table(rater.b) / length(table(rater.b))
  expected.mat = histogram.a %*% t(histogram.b)
  expected.mat = expected.mat / sum(expected.mat)
  
  #get weights
  labels = as.numeric( as.vector (names(table(rater.a))))
  weights = outer(labels, labels, FUN = function(x,y) (x-y)^2 )
  
  #calculate kappa
  kappa = 1 - sum(weights*confusion.mat)/sum(weights*expected.mat)
  kappa
}



evalerror <- function(preds, dtrain) {
  
  labels <- getinfo(dtrain, "label")
  
  err <- ScoreQuadraticWeightedKappa(labels,round(preds))
  
  return(list(metric = "kappa", value = err))
  
}



cv <- xgb.cv(data        = xgbtrain,
              
              eta         = 0.025,
              
              depth       = 21,
              
              nrounds     = 4000,
              
              objective   = "count:poisson",
              
              feval=evalerror,
              
              colsample_bytree=0.7,
              
              min_child_weight=3,
              
              subsample=0.7, 
              
              nfold=5
              
)


cat("making predictions\n")

submission <- data.frame(Id=test$Id)

# change

submission$Response <- as.integer(round(predict(param, data.matrix(test[,feature.names]))))


# I pretended this was a regression problem and some predictions may be outside the range

submission[submission$Response<1, "Response"] <- 1

submission[submission$Response>8, "Response"] <- 8

submission[submission$Response==3,"Response"] <- 2

cat("saving the submission file\n")

write_csv(submission, "D:\\kaggle\\PRUDENTIAL\\submission\\12222015_4.csv")


######################################################################################################

dim(tmp_factors)


for(f in names(tmp_factors)){
  
  levels <- unique(tmp_factors[, f])
  
  tmp_factors[,f] <- factor(tmp_factors[,f], levels = levels)
  
}

str(tmp_factors, list.len = 999)


#Important step ^^^^

#############################################################################################################


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
  
  print(((i / dim(nms_df)[2]) * 100 ))
  
  tmp_count[, paste(i, "_two", sep="")] <- my.f2cnt(th2 = tmp_factors, 
                                                    
                                                    vn1 = nms_df[1,i], 
                                                    
                                                    vn2 = nms_df[2,i] )
  
}

###############################################################################################################

#3 way count


nms <- combn(names(tmp_factors), 3)

dim(nms)

nms_df <- data.frame(nms);

len = length(names(nms_df))

for (i in 1:len) {
  
  print(paste0(( i / len) *100, "%"))
  
  nms_df[, i] <- as.character(nms_df[, i])
  
}

for(i in 1:dim(nms_df)[2]){
  
  #new df 
  
  print((i / dim(nms_df)[2]) * 100)
  
  tmp_count[, paste(i, "_three", sep="")] <- my.f3cnt(th2 = tmp, 
                                                      
                                                      vn1 = nms_df[1,i], 
                                                      
                                                      vn2 = nms_df[2,i], 
                                                      
                                                      vn3 = nms_df[3,i])
  
}


##############################################################################################################



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


##########################################################################################################

tmp_new <- cbind(tmp, tmp_count)

rm(tmp1); rm(tmp_factors); rm(test); rm(train); rm(tmp_count) 

gc()

tmp_new[is.na(tmp_new)] <- -1

train <- tmp_new[c(1:59381),]

test <- tmp_new[c(59382:79146),]

rm(tmp_new)

cat("assuming text variables are categorical & replacing them with numeric ids\n")

feature.names <- names(train)[-1]

for (f in feature.names) {
  
  if (class(train[[f]])=="character") {
    
    levels <- unique(c(train[[f]], test[[f]]))
    
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

cat("training a XGBoost classifier\n")

feature.names = names(train)[-1]

clf <- xgboost(data        = data.matrix(train[,feature.names]),
               
               label       = response,
               
               eta         = 0.02,
               
               depth       = 21,
               
               nrounds     = 4000,
               
               objective   = "count:poisson",
               
               eval_metric = "rmse",
               
               colsample_bytree=0.65,
               
               min_child_weight=3,
               
               subsample=0.7)

cat("making predictions\n")

submission <- data.frame(Id=test$Id)

submission$Response <- as.integer(round(predict(clf, data.matrix(test[,feature.names]))))


# I pretended this was a regression problem and some predictions may be outside the range

submission[submission$Response<1, "Response"] <- 1

submission[submission$Response>8, "Response"] <- 8

submission[submission$Response==3,"Response"] <- 2

cat("saving the submission file\n")

write_csv(submission, "C:\\Users\\amulya\\Documents\\Kaggle\\PRUDENTIAL\\submission\\12222015_1.csv")


#######################################################################################################
