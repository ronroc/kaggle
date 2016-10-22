
set.seed(02*14*2016)

# read in the data file--------------------------------------------------------------------------------

require(data.table); require(xgboost); require(h2o); require(caret)


train <- fread("D:\\kaggle\\PRUDENTIAL\\Data\\train.csv", data.table = F)

test <- fread("D:\\kaggle\\PRUDENTIAL\\Data\\test.csv", data.table = F)


train_id <- train$Id

train$Id <- NULL;

id <- test$Id; test$Id <- NULL

response <- train$Response; train$Response <- NULL


tmp <- rbind(train, test)


row_NA <- apply(tmp, 1, function(x) sum(is.na(x)))

tmp$row_NA <- row_NA


# # new feature engg found in high performing script----------------------------------------------------------
# 
# first <- c()
# 
# char <- as.character(tmp$Product_Info_2)
# 
# for(i in 1:length(tmp$Product_Info_2))
#   
# {
#   
#   first[i] <- substr(char[i], 1, 1) 
#   
# }
# 
# 
# tmp$first <- first
# 
# 
# second <- c()
# 
# for(i in 1:length(tmp$Product_Info_2))
#   
# {
#   
#   second[i] <- substr(char[i], 2, 2) 
#   
# }
# 
# 
# tmp$second <- second
# 


# dummify varible------------------------------------------------------------------------------

dummy <- c("Product_Info_2")

tmp_dummy <- data.frame(tmp[,"Product_Info_2"])

tmp_dummy[ , 1] <- as.factor(tmp_dummy[ , 1])


dummies <- dummyVars( ~ ., data = tmp_dummy)

gc()

tmp_dummy <- predict(dummies, newdata = tmp_dummy)

tmp_dummy <- data.frame(tmp_dummy)

dim(tmp_dummy)


# count the number of keywords row wise--------------------------------------------------------

keywords <- paste("Medical_Keyword_", 1:48, sep="")

tmp_count <- tmp[, keywords]


count <- apply(tmp_count, 1, function(x) sum(x))

tmp$count <- count


##############################################################################################



tmp[is.na(tmp)] <- -1

tmp_new <- cbind(tmp, tmp_dummy)

# interaction features-------------------------------------------------------------------------

# require(readr)
# 
# imp_mat <- read_csv("D:\\kaggle\\PRUDENTIAL\\Data\\imp_mat_raw.csv")
# 
# top5 <- imp_mat$Feature[1:5]

tmp_int <- tmp[ , c("Ins_Age", "BMI")]

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

#####################################################################################################

tmp_int <- tmp[ , c("Ins_Age", "BMI")]


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

#####################################################################################################

tmp_int <- tmp[ , c("Ins_Age", "BMI")]

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



tmp_new <- tmp_new[, !(names(tmp_new) %in%  c("Ins_Age", "BMI"))]

tmp_int <- tmp[ , c("Ins_Age", "BMI")]

tmp_new <- cbind(tmp_new, tmp_int)

#####################################################################################################

tmp_new$Medical_History_10 <- NULL

tmp_new$Medical_History_24 <- NULL


############################################################################################################



feature.names <- names(tmp_new)

for (f in feature.names) {
  
  if (class(tmp_new[[f]])=="character") {
    
    levels <- unique(c(tmp_new[[f]]))
    
    tmp_new[[f]] <- as.integer(factor(tmp_new[[f]], levels=levels))
    
  }
}


# tmp_new <- cbind(tmp_new, tmp_dummy)

# tmp_new <- tmp_new[ , !(names(tmp_new) %in% c("Product_Info_2"))]



# trying a Rtsne to help with------------------------------------------------------------------


require(Rtsne)

both = as.matrix(tmp_new)

gc()

both = both-1

tsne <- Rtsne(both, check_duplicates = FALSE, pca = FALSE, verbose=TRUE,
              perplexity=30, theta=0.5, dims=2)

tmp_new <- cbind(tmp_new, tsne$Y)

train <- tmp_new[c(1:59381),]
 
test <- tmp_new[c(59382:79146),]

