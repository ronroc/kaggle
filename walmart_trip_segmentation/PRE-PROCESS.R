
# explicitly mention data.table usage at the beginning 

#load packages

require(xgboost); require(readr); require(caret); require(doParallel); require(data.table); require(FeatureHashing); require(sqldf); require(dplyr)

set.seed(11102015)

################################################################################################################

#read in data 

#train_raw <- read_csv("C:/Users/amulya/Documents/Kaggle/Walmart/train.csv")

#test_raw <- read_csv("C:/Users/amulya/Documents/Kaggle/Walmart/test.csv")

train_raw <- read_csv("D:/kaggle/walmart_seg/Data/train.csv")

test_raw <- read_csv("D:/kaggle/walmart_seg/Data/test.csv")

visit_num <- test_raw$VisitNumber

################################################################################################################

feature.names <- names(train_raw)[!names(train_raw) %in% c("TripType")]

response <- names(train_raw)[1]

response <- train_raw[ , response] 

class_old <- sort(unique(response$TripType))

class_new <- seq(0, 37)

#replace elements of class_old with elements of class_new in response

for( i in 1:38 ){
  
  train_raw$TripType[train_raw$TripType == class_old[i]] <- class_new[i]
  
}

table(train_raw$TripType)

################################################################################################################


#very basic stuff----------------------------------------------------------------------------------------------

# options : 1. find better ways of extracting info from text variables

#           2. one way is stuff such as count, tf idf etc...

#           3. there are other wayss which as of 11072015 I dnt know but I will refer @Kaggle

#           4. Below mentioned is a very basic way of using character information.


cat("assuming text variables are categorical & replacing them with numeric ids\n")

for (f in feature.names) {
  
  if (class(train_raw[[f]]) == "character") {
    
    levels <- unique(c(train_raw[[f]], test_raw[[f]]))
    
    train_raw[[f]] <- as.integer(factor(train_raw[[f]], levels=levels))
    
    test_raw[[f]]  <- as.integer(factor(test_raw[[f]],  levels=levels))
  }
}


###############################################################################################################

# combine train and test

# easier way to manipulate data for both train and test

# columns in both test and train should be the same for training , testing , validation and prediction



tmp <- rbind(train_raw[ , feature.names], test_raw)

# IMP : FACED ISSUES [ READ : CODE BREAKS] WHEN tbl_df WAS PRESENT CLASS

#     : as.factor CONVERSION COULDN`T GO THROUGH 

#     : REMOVING THE CLASS PROVED BENEFICIAL ; RUN THE BELOW CODE FOR CONVERSION

tmp <- data.frame(tmp)


# for counts (1 - way, 2 - way, 3 - way)

# selected columns should contain factors

# so first select columns and then convert them into factors  

# Also maintain name used in below code for easy reproducibilty ( use the same names for ur present df)



tmp_factors = tmp[ , feature.names]; dim(tmp_factors)

len = length(names(tmp_factors))

for (i in 1:len) {
  
  print(paste0( i / (len) *100, "%"))
  
  tmp_factors[ , i] <- as.factor(tmp_factors[ , i])
  
}

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
  
  tmp_count[, paste(i, "_two", sep="")] <- my.f2cnt(th2 = tmp, 
                                                    
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

###############################################################################################################


# IMP : This step is not required unless the method of creating tmp_factors is similar to 

# that used in springleaf Marketing response challenge

# for this contest skipping this step as it is not required



#need to convert all of tmp_factors to numeric


len = length(names(tmp_factors))

for (i in 1:len) {
  
  tmp_factors[, i] <- as.numeric(tmp_factors[, i])
  
}



###############################################################################################################



#use this code before running dummy variables conversion 

#Note IMP:  1. Selected columns should be of factors


#dummify few columns


name <- c("Weekday", "DepartmentDescription", "ScanCount")

tmp_dummy <- (tmp[ , name ])

len = length(names(tmp_dummy))

for (i in 1:len) {
  
  print(paste0(( i / len) * 100, "%"))
  
  tmp_dummy[, i] <- as.factor(tmp_dummy[, i])
  
}



dummies <- dummyVars( ~ ., data = tmp_dummy)

gc()

tmp_dummy <- predict(dummies, newdata = tmp_dummy)

tmp_dummy <- data.frame(tmp_dummy)

dim(tmp_dummy)

##############################################################################################################


#test if the below code works before running 


tmp_features <- data.frame(id = 1:dim(tmp)[1])



# features for difference from mean

for(i in 1:ncol(tmp_new)){
  
  tmp_features[, paste(names(tmp_new)[i], "_mean", sep="")] <- tmp_new[, i] - mean(tmp_new[, i])
  
  
}




# features for standardization

for(i in 1:ncol(tmp_new)){
  
  tmp_features[, paste0(names(tmp_new)[i], "_zscore")] <- ((tmp_new[, i] - mean(tmp_new[, i])) / sd(tmp_new[, i]))
  
  
}

###########################################################################################################


# Take interaction features from feature importance from various different algos (Eg : 20 from each)

# Interaction features


int_col <- c()

tmp_int <- tmp_new[ , int_col]



# create + interaction features


for (i in 1:ncol(tmp_int)) {
  
  for (j in (i + 1) : (ncol(tmp_int) + 1)) {
    
    var.x <- colnames(tmp_int)[i]
    
    var.y <- colnames(tmp_int)[j]
    
    var.new <- paste0(var.x, '_plus_', var.y)
    
    tmp_int[, paste0(var.new)] <- tmp_int[, i] + tmp_int[, j]
    
  }
}




# create - interaction features


for (i in 1:ncol(tmp_int)) {
  
  for (j in (i + 1) : (ncol(tmp_int) + 1)) {
    
    var.x <- colnames(tmp_int)[i]
    
    var.y <- colnames(tmp_int)[j]
    
    var.new <- paste0(var.x, '_minus_', var.y)
    
    tmp_int[, paste0(var.new)] <- tmp_int[, i] - tmp_int[, j]
    
  }
}





# create * interaction features


for (i in 1:ncol(tmp_int)) {
  
  for (j in (i + 1) : (ncol(tmp_int) + 1)) {
    
    var.x <- colnames(tmp_int)[i]
    
    var.y <- colnames(tmp_int)[j]
    
    var.new <- paste0(var.x, '_mult_', var.y)
    
    tmp_int[, paste0(var.new)] <- tmp_int[, i] * tmp_int[, j]
    
  }
}





# create / interaction features


for (i in 1:ncol(tmp_int)) {
  
  for (j in (i + 1) : (ncol(tmp_int) + 1)) {
    
    var.x <- colnames(tmp_int)[i]
    
    var.y <- colnames(tmp_int)[j]
    
    var.new <- paste0(var.x, '_divide_', var.y)
    
    tmp_int[, paste0(var.new)] <- tmp_int[, i] / tmp_int[, j]
    
  }
}



tmp_int <- tmp_int[, -int_col]

################################################################################################


# Dealing with text data

# Tried methods : 1. Length of string (nchar)

#               : 2. number of words

#               : 3. N-Grams


# create a data frame with columns being of strings (character type)

# create a new tmp df with raw features

# these changes introduced NA`s in dval N dtrain 

# solution : remove NA`s before combining 

tmp_1 <- read_csv("D:/kaggle/walmart_seg/Data/train.csv")[-1]

tmp_2 <- read_csv("D:/kaggle/walmart_seg/Data/test.csv")

tmp <- rbind(tmp_1, tmp_2)

tmp_str <- data.frame((tmp[ , "DepartmentDescription"]))


tmp_str[is.na(tmp_str)] <- 0


names(tmp_str) = ("Dept_Desc")




#convert columns from character to factors in a data frame

# code sourced from Stack Overflow


i <- sapply(tmp_str, is.factor)


tmp_str[i] <- lapply(tmp_str[i], as.character)


tmp_str[, paste0("str_len")] <-  nchar(x = tmp_str$Dept_Desc) 



# calculate number of words


tmp_str$Dept_Desc <- gsub(' {2,}',' ', tmp_str$Dept_Desc)

lnth <- rep(0, nrow(tmp_str))

for(i in 1:nrow(tmp_str)) {
  
  lnth[i] <-  length(strsplit(tmp_str$Dept_Desc[i],' ')[[1]])
  
}

tmp_str$num_wrd <- lnth



# calculate N-Grams of words


rm(tmp); rm(tmp_1); rm(tmp_2)

############################################################################################################


# combine all temporarily created df`s into tmp_new in a single piece of code  

# tmp_count

# tmp_dummy

# tmp_features

# tmp_int

# tmp_str


#tmp_new = cbind.data.frame(tmp_new, tmp_dummy)


tmp_new = cbind.data.frame(tmp, tmp_count, tmp_dummy)

tmp_new = cbind.data.frame(tmp_new, tmp_features)

tmp_new = cbind.data.frame(tmp_new, tmp_str)


gc()

#remove unwanted df

rm(nms); rm(nms_df); rm(sum1); rm(tmp_count); rm(tmp_factors); rm(tmp1); rm(tmp_features) 

rm(tmp); rm(tmp_str); rm(train); rm(test)

#rm(tmp_new_cpy); rm(tmp_cpy)

gc()

#seperate train from test

train <- tmp_new[c(1:647054),]

test <- tmp_new[c(647055:1300700),]

dim(train); dim(test)

gc()

#rm(tmp_new); rm(tmp_str)

#gc()

##############################################################################################################

#check for new ways to impute NA

# options : 1. PLot the distributions of each column

#           2. Plot the distribution of NA

#           3. Impute by mean, KNN ( both are found in caret ) **** should try for this competition *** 

train[is.na(train)] <- 0

gc()

test[is.na(test)] <- 0

gc()

#############################################################################################################################

# Impute method : KnnImpute

# columns upc and fineline number has 419 NA's 

pp_1_test = preProcess(iris_miss_1, method = "knnImpute")

set.seed(1)

test_1_result <- predict(pp_1_test, iris_miss_1)

#bag Impute

preProc <- preProcess(method="bagImpute", training[, 1:4])

training[, 1:4] <- predict(preProc, training[, 1:4])
