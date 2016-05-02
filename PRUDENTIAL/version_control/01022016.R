
library(caret); library(mlr); library(data.table); require(doParallel)

cl <- makeCluster(detectCores()); registerDoParallel(cl)

train <- fread("D:\\kaggle\\PRUDENTIAL\\Data\\train.csv", data.table = F)

test  <- fread("D:\\kaggle\\PRUDENTIAL\\Data\\test.csv", data.table = F)

test$Response = 0

## store Id column and remove it from the train and test data


testId = test$Id

train$Id = test$Id = NULL


tmp = rbind(train, test)


# row wise count of NA's

row_NA <- apply(tmp, 1, function(x) sum(is.na(x)))

tmp[is.na(tmp)] <- -1



cat_var <- c(paste("Product_Info_", c(1:3,5:7), sep=""), paste("Employment_Info_", c(2,3,5), sep=""),
             
             paste("InsuredInfo_", 1:7, sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""), 
             
             "Family_Hist_1", paste("Medical_History_", c(2:9, 11:14, 16:23, 25:31, 33:41), sep=""))



cont_var <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", 
              
              "Employment_Info_6", "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", 
              
              "Family_Hist_5")




disc_var <- c("Medical_History_1", "Medical_History_15", "Medical_History_24", "Medical_History_32", 
              
              paste("Medical_Keyword_", 1:48, sep=""), "Medical_History_10")



tmp_cat <- tmp[, cat_var]

tmp_cont <- tmp[, cont_var]

tmp_disc <- tmp[, disc_var]



## consider tmp_cat; tmp_disc

# length of unique values

len_cat <- lapply(tmp_cat, function(x) length(unique(x)))

len_disc <- lapply(tmp_disc, function(x) length(unique(x)))


# features with number of unique elements more than 10


high_card <- c()


for(i in 1:length(len_cat)){
  
  if(len_cat[[i]] > 10) high_card <- c(names(len_cat)[i], high_card)
}



for(i in 1:length(len_disc)){
  
  if(len_disc[[i]] > 10) high_card <- c(names(len_disc)[i], high_card)
}



tmp_high_card <- tmp[, high_card]

tmp_cat <- tmp_cat[ , !(names(tmp_cat) %in% high_card )]

tmp_disc <- tmp_disc[ , !(names(tmp_disc) %in% high_card )]

# removing dummified medical keyword cols

tmp_Med_Kwrd <- tmp_disc[ ,(names(tmp_disc) %in% paste("Medical_Keyword_", 1:48, sep=""))]


tmp_Med_Kwrd$row_NA <- row_NA



# removed tmp_disc

rm(tmp_disc)


# one var missing 

tmp_missing <- names(tmp)[!(names(tmp) %in%  c(names(tmp_cat), names(tmp_cont), names(tmp_high_card), 
                                               
                                               names(tmp_Med_Kwrd)))]


##################################################################################################


tmp_dummy <- tmp_cat


len = length(names(tmp_dummy))


for (i in 1:len) {
  
  print(paste0( i / (len) *100, "%"))
  
  tmp_dummy[ , i] <- as.factor(tmp_dummy[ , i])
  
}



str(tmp_dummy, list.len = 999)


dummies <- dummyVars( ~ ., data = tmp_dummy)


gc()


tmp_dummy <- predict(dummies, newdata = tmp_dummy)

tmp_dummy <- data.frame(tmp_dummy)
  
dim(tmp_dummy)


## now considering high card vars


str(tmp_high_card, list.len = 999)


cat("assuming text variables are categorical & replacing them with numeric ids\n")

for (f in names(tmp_high_card)) {
  
  if (class(tmp_high_card[[f]])=="character") {
    
    levels <- unique(c(tmp[[f]]))
    
    tmp_high_card[[f]] <- as.integer(factor(tmp_high_card[[f]], levels=levels))
    
  }
}


str(tmp_high_card, list.len = 999)


# converting to factors

len = length(names(tmp_high_card))

for (i in 1:len) {
  
  print(paste0( i / (len) *100, "%"))
  
  tmp_high_card[ , i] <- as.factor(tmp_high_card[ , i])
  
}


# counts ;


tmp_factors <- tmp_high_card


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
  
  tmp_count[, paste(names(nms_df)[i], "_two", sep="")] <- my.f2cnt(th2 = tmp_high_card, 
                                                                   
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
  
  tmp_count[, paste(names(nms_df)[i], "_three", sep="")] <- my.f3cnt(th2 = tmp_high_card, 
                                                                     
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



## treatment of continous vars

# pre-process using box cox

require(caret)
tmp_pre <-  preProcess(tmp_cont, method = ("BoxCox"))

tmp_cont_new <- predict(tmp_pre, tmp_cont)


############################################################################################################

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


#############################################################################################


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


#############################################################################################

# create ^ interaction features

# not using division interaction features - NA's


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



tmp_new <- cbind(tmp_new, tmp_int)

rm(tmp_int)

gc()



#tmp <- tmp[, !( names(tmp) %in% names(tmp_factors))]


tmp_Med_Kwrd$Response <- tmp$Response

tmp_new <- cbind(tmp_Med_Kwrd, tmp_dummy, tmp_count, tmp_cat, tmp_high_card, tmp_cont_new)

dim(tmp_new)

# check row numbers

train <- tmp_new[c(1:59381),]

test <- tmp_new[c(59382:79146),]

gc()

## create mlr task and convert factors to dummy features


trainTask = makeRegrTask(data = train, target = "Response")

#trainTask = createDummyFeatures(trainTask)

testTask = makeRegrTask(data = test, target = "Response")

#testTask = createDummyFeatures(testTask)


## create mlr learner


set.seed(12302015)

lrn = makeLearner("regr.xgboost")

lrn$par.vals = list(
  
  #nthread             = 30,
  
  nrounds             = 500,
  
  print.every.n       = 20,
  
  objective           = "reg:linear",
  
  depth = 9,
  
  min_child_weight = 50,
  
  subsample = 0.5,
  
  eta = 0.05,
  
  colsample_bytree = 0.3
)


# missing values will be imputed by their median


lrn = makeImputeWrapper(lrn, classes = list(numeric = imputeMedian(), integer = imputeMedian()))

## Create Evaluation Function


SQWKfun = function(x = seq(1.5, 7.5, by = 1), pred) {
  
  preds = pred$data$response
  
  true = pred$data$truth
  
  cuts = c(min(preds), x[1], x[2], x[3], x[4], x[5], x[6], x[7], max(preds))
  
  preds = as.numeric(Hmisc::cut2(preds, cuts))
  
  err = Metrics::ScoreQuadraticWeightedKappa(preds, true, 1, 8)
  
  return(-err)
  
}


SQWK = makeMeasure(id = "SQWK", minimize = FALSE, properties = c("regr"), best = 1, worst = 0,
                   
                   fun = function(task, model, pred, feats, extra.args) {
                     
                     return(-SQWKfun(x = seq(1.5, 7.5, by = 1), pred))
                     
                   })



## This is how you could do hyperparameter tuning

# # 1) Define the set of parameters you want to tune (here 'eta')



ps = makeParamSet(
  
  makeNumericParam("eta", lower = 0.01, upper = 0.03),
  makeNumericParam("colsample_bytree", lower = 0.5, upper = 0.7)
)


# # 2) Use 3-fold Cross-Validation to measure improvements


rdesc = makeResampleDesc("CV", iters = 3L)


# # 3) Here we use Random Search (with 10 Iterations) to find the optimal hyperparameter


ctrl =  makeTuneControlRandom(maxit = 20)


# # 4) now use the learner on the training Task with the 3-fold CV to optimize your set of parameters and evaluate it with SQWK


res = tuneParams(lrn, task = trainTask, resampling = rdesc, par.set = ps, control = ctrl, measures = SQWK)


res


# # 5) set the optimal hyperparameter


lrn = setHyperPars(lrn, par.vals = res$x)


## now try to find the optimal cutpoints that maximises the SQWK measure based on the Cross-Validated predictions


cv = crossval(lrn, trainTask, iter = 4, measures = SQWK, show.info = TRUE)


optCuts = optim(seq(1.5, 7.5, by = 1), SQWKfun, pred = cv$pred)


optCuts
  


optCuts = optim(seq(1.5, 7.5, by = 1), SQWKfun, pred = pred)

print(optCuts)

preds = as.numeric(Hmisc::cut2(test$cvResponse1, c(-Inf, optCuts$par, Inf)))


## now train the model on all training data


tr = train(lrn, trainTask)



## predict using the optimal cut-points


pred = predict(tr, testTask, missing = NaN)


preds = as.numeric(Hmisc::cut2(pred$data$response, c(-Inf, optCuts$par, Inf)))


table(preds)


## create submission file


submission = data.frame(Id = testId[1:19765])


submission$Response = as.integer(preds)

write.csv(submission, "01022016_1.csv", row.names = FALSE)


