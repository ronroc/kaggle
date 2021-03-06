
x <-  c("start  of the first step", "Reading in the data"); cat(x, sep = "\n")

library(readr); library(xgboost); library(caret); library(lubridate)

library(doParallel); library(caret); library(sqldf)

set.seed(867530099)

train <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

# save ID and response

train_target <- train$target

train$target <- NULL

train_ID <- train$ID

train$ID <- NULL

test <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")

# save ID and response

test_ID <- test$ID

test$ID <- NULL


print(dim(train)); print(dim(test))


#bind train and test for better and easier pre-processing

tmp <- rbind(train, test)

#seperate out date columns in train and test (mostly by using a grep function)

datecolumns = c("VAR_0073", "VAR_0075", "VAR_0156", "VAR_0157", "VAR_0158",
  
  "VAR_0159", "VAR_0166", "VAR_0167", "VAR_0169","VAR_0168",
  
  "VAR_0176", "VAR_0177", "VAR_0178", "VAR_0179", "VAR_0204", "VAR_0217")

tmp_date <- tmp[datecolumns]

#convert into

tmp_date <- data.frame(apply(tmp_date, 2,function(x)(
                                 
                      strptime(x, format = "%d%b%y: %H:%M:%S",tz = "UTC"))))



tmp_date[,c(17:32)] <- data.frame(apply(tmp_date[, c(1:16)], 2, function(x) wday(x)))

names(tmp_date)[(17:32)] <- paste0("wday_", names(tmp_date)[(1:16)])



tmp_date[,c(33:48)] <- data.frame(apply(tmp_date[, c(1:16)], 2, function(x) mday(x)))

names(tmp_date)[(33:48)] <- paste0("mday_", names(tmp_date)[(1:16)])



tmp_date[, c(49:50)] <- data.frame(apply(tmp_date[, c(15:16)], 2, function(x) hour(x)))

names(tmp_date)[(49:50)] <- paste0("hour_", names(tmp_date)[c(15:16)])



tmp_date[, c(51:52)] <- data.frame(apply(tmp_date[, c(49:50)], 2, function(x)
    
                                   ifelse(x <= 6, 1,
                              
                                   ifelse(x <= 12, 2,
                               
                                   ifelse(x <= 18, 3,
                                          
                                   ifelse(x <= 24, 4))))))


names(tmp_date)[(51:52)] <- paste0("Dzones_", names(tmp_date)[(15:16)])

#plot histogram of dates

par(mar = c(2,2,2,2),mfrow = c(4,4))

for (i in 1:16) {
  
  hist( tmp_date[,i], "weeks" ,format = "%d %b %y", main = names(tmp_date[,i]),
    
        xlab = "", ylab = "")
  }

for (i in 1:16) {
  
  tmp_date[, i] <- as.numeric(tmp_date[, i])

  }


tmp_pre <-  preProcess(tmp_date, method = ("BoxCox"))

tmp_pre_pred <- predict(tmp_pre, tmp_date)

tmp_date <- tmp_pre_pred




dim(tmp)

tmp <- tmp[,!(names(tmp) %in% (datecolumns))]

tmp <- cbind(tmp, tmp_date)

dim(tmp); dim(tmp_date)

gc()

nzv <- read_csv("nzv.csv")

tmp <- tmp[,-nzv$nzv]

##########################################################################################
#using owen`s Amazon code approach

my.f2cnt <- function(th2, vn1, vn2, filter=TRUE) {
  
  df <- data.frame(f1=th2[,vn1], f2=th2[,vn2], filter=filter)
  
  sum1 <- sqldf("select f1, f2, count(*) as cnt 
                
                from df 
                
                where filter=1 
                
                group by 1,2")
  
  tmp <- sqldf("select b.cnt 
               
               from df a left join sum1 b 
               
               on a.f1=b.f1 and a.f2=b.f2")
  
  tmp$cnt[is.na(tmp$cnt)] <- 0
  
  return(tmp$cnt)

  }

#3 way count

my.f3cnt<-function(th2, vn1, vn2, vn3, filter=TRUE) {
  
  df<-data.frame(f1=th2[,vn1], f2=th2[,vn2], f3=th2[, vn3], filter=filter)
  
  sum1<-sqldf("select f1, f2, f3, count(*) as cnt 
              
              from df 
              
              where filter=1 
              
              group by 1,2, 3")
  
  tmp<-sqldf("select b.cnt 
             
             from df a left join sum1 b 
             
             on a.f1=b.f1 and a.f2=b.f2 and a.f3=b.f3")
  
  tmp$cnt[is.na(tmp$cnt)]<-0
  
  return(tmp$cnt)

}


# group data => create combinations of a given order

groupData <- function(xmat, degree)

  {
  
  # indices of combinations
  
  xind <- combn(1:ncol(xmat), degree)
  
  # storage structure for the result
  
  agx <- foreach(ii = 1:ncol(xind), .combine = cbind ) %do%
  
    {
    x <- xmat[,xind[1,ii]]
    
    for (jj in 2:nrow(xind))
    
      {
      
      x <- paste(x, xmat[,xind[jj,ii]], sep = "_")
    
      }
    
    x
  }
  
  colnames(agx) <- paste(paste("f", degree, sep = ""), 1:ncol(agx), sep = "_")
  
  return(agx)

  }

######################################################################################

len = length(names(tmp))
  
  names = rep(0, len)
  
  
  for (i in 1:len) {
    
    print(paste0(( i / len) *100, "%"))
    
    if (length(table(tmp[[i]])) < 100) {
      
      names[i] = names(tmp)[i]
      
    }
    
  }
  
  names = names[names != 0]
  
  length(names)
  
  #names <- c(names, "VAR_0200", "VAR_0241","VAR_0404", "VAR_0493")

  tmp_factors = tmp[,(names(tmp) %in% names)]; dim(tmp_factors)
  
  len = length(names(tmp_factors))
  
  for (i in 1:len) {
    
    print(paste0(( i / len) *100, "%"))
    
    tmp_factors[, i] <- as.factor(tmp_factors[, i])
    
  }
  
##########################################################################################
  
imp_feature <- read_csv("D:/kaggle/Springleaf/xgb_featureImp.csv")
  
imp_names <- (imp_feature$Feature)
  
imp_names <- sapply(imp_names, function(x) names(tmp)[x])
  
imp_names <- imp_names[1:200]

tmp_factors <- tmp_factors[, (names(tmp_factors) %in% imp_names)]

####################################################################################

#2 way count

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


#3 way count

nms <- combn(names(tmp_factors), 3)

dim(nms)

nms_df <- data.frame(nms); nms_df <- nms_df[ c(1:3), c(1:200)]


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


#one way count

len = dim(tmp_factors)[2]

for(i in 1:len){
  
  
    print((i / len) * 100 )
    
    tmp_factors$x <- tmp_factors[, i]
    
    sum1 <- sqldf("select x, count(1) as cnt
       
                from tmp_factors  group by 1 ")

    tmp1 <- sqldf("select cnt from tmp_factors a left join sum1 b on a.x=b.x")

    tmp_count[, paste(names(tmp_factors)[i], "_cnt", sep="")] <- tmp1$cnt
  
    }  

#need to convert all of tmp_factors to numeric

len = length(names(tmp_factors))

for (i in 1:len) {
  
  tmp_factors[, i] <- as.numeric(tmp_factors[, i])
  
}

#############################################################################

tmp_new = cbind.data.frame(tmp, tmp_factors, tmp_count)

#leave one out average left out

################################################################################                
#need to rm , #large memory footprint

dummies <- dummyVars( ~ ., data = tmp_factors)
                
tmp_factors <- predict(dummies, newdata = tmp_factors)
                
tmp_new = cbind.data.frame(tmp, tmp_factors)
                
                
#tmp_imp$VAR_0543 <- NULL; tmp_imp$VAR_0704 <- NULL; tmp_imp$VAR_0920 <- NULL
                
#tmp_imp$VAR_1087 <- NULL; tmp_imp$VAR_0891 <- NULL; tmp_imp$VAR_0908 <- NULL
                
#tmp_imp$VAR_0887 <- NULL; tmp_imp$VAR_0298 <- NULL; tmp_imp$VAR_0318 <- NULL
                
#tmp_imp$VAR_1494 <- NULL; tmp_imp$VAR_0608 <- NULL; tmp_imp$VAR_0330 <- NULL
                
#tmp_imp$VAR_1247 <- NULL; tmp_imp$VAR_0073 <- NULL; tmp_imp$VAR_0241 <- NULL
                
############################################################################################
                
rm(nzv); rm(test_imp); rm(tmp); rm(tmp_date); rm(tmp_pre_pred)
                
rm(train); rm(datecolumns);

rm(names); rm(tmp_Imp); rm(tmp_factors); rm(imp_feature); rm(nms_df);rm(nms)
                
######################################################################################
                
rm(tmp); rm(tmp_date); rm(tmp_imp); rm(tmp_imp_2); rm(tmp_imp_2d); rm(sum1)
rm(tmp1); rm(tmp_count); rm(tmp_pre)

gc()                
                
tmp_new[is.na(tmp_new)] <- -9999999
                
                
feature.names <- names(tmp_new)
                
for (f in feature.names) {

if (class(tmp_new[[f]]) == "character") {

levels <- unique((tmp_new[[f]]))
                    
tmp_new[[f]] <-as.integer(factor(tmp_new[[f]], levels = levels))
                    
}
                  
}
                
                
##############################################################################
                
train <- tmp_new[c(1:145231),]
                
test <- tmp_new[c(145232:290463),]
                
dim(train); dim(test)
                
gc()
                
                
split <- createDataPartition(train_target, p = .9, list = FALSE)
                
response <- train_target
                
training <- train[split,]
                
testing  <- train[-split,]
                
response_testing <- response[-split]
                
response_training <- response[split]

feature.names <- names(train)

dtrain <- xgb.DMatrix(data.matrix(training[,feature.names]), label = response_training)
                
dval <- xgb.DMatrix(data.matrix(testing[,feature.names]), label = response_testing)
                
watchlist <- list(train = dtrain, test = dval)

param <- list(  objective           = "binary:logistic", 
                
                # booster = "gblinear",
                
                eta                 = 0.014, 
                
                max_depth           = 10,  
                
                subsample           = 0.7,
                
                colsample_bytree    = 0.7,
                
                eval_metric         = "auc",
                
                nthreads = -1
                
)                
cl <- makeCluster(2); registerDoParallel(cl)                
                
clf_first <- xgb.train( params = param,
                    
                        data = dtrain,
                    
                        nrounds = 1000,
                    
                        verbose = 2,
                    
                        watchlist = watchlist,
                    
                        
                    
                        maximize = TRUE
                  )

xgb.save(model = clf_first, "1012015xgb")

submission <- data.frame(ID = test_ID)
                
submission$target <- NA
                
submission[,"target"] <- predict(clf_first, 
                                 
                                 data.matrix(test[,feature.names]))
                
write_csv(submission, "10122015.csv")
                
#############################################################################
#If restarted
                
xgb.load("10072015xgb")
                
ptrain = predict(clf_first, dtrain, outputmargin = T)
                
setinfo(dtrain, "base_margin", ptrain)
                
                
clf_first_1 <- xgboost( params = param,
                        
                        data = dtrain,
                    
                        nrounds = 1000,
                    
                        verbose = 2,
                    
                        nthread = 2,
                    
                        maximize = TRUE
                  )
                
submission <- data.frame(ID = test_ID)
                
submission$target <- NA
                
submission[,"target"] <- predict(clf_first_1, data.matrix(test[,feature.names]))
                
write_csv(submission, "10082015.csv")
                