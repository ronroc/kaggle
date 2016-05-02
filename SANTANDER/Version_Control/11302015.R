

require(data.table); require(lubridate); require(caret); require(sqldf); require(xgboost); require(sqldf); require(xlsx); require(Matrix)

train_raw <- fread(input = "D:\\kaggle\\HOMESITE\\Data\\train.csv", data.table = F)

response <- train_raw$QuoteConversion_Flag

response <- as.matrix(response)

response_test = numeric()

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



#############################################################################################################################


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

###########################################################################################################


train = tmp[c(1:260753), ]


test <- tmp[c(260754:434589), ]

a <- factorToNumeric(train = train_raw, test = test_raw, response = train_raw$QuoteConversion_Flag, 
                
                variables =  "GeographicField64", metrics = c("mean","median","sd")  
        )

########################################################################################################
########################################################################################################


len = length(mtcars)

for (i in 1:len) {
  
  print(paste0( i / (len) *100, "%"))
  
  levels <- unique(mtcars[[i]])
  
  mtcars[ , i] <- factor(mtcars[ , i], levels = levels)
  
}

train <- mtcars[1:10, ]

test <- mtcars[11:32, ]

factorToNumeric(train = train, test = test, response = "vs", 
                
                variables =  "carb", metrics = c("mean"))

# error

rm(mtcars)

data("mtcars")

train <- mtcars[1:10, ]

test <- mtcars[11:32, ]

factorToNumeric(train = train, test = test, response = train$vs, 
                
                variables =  "carb", metrics = c("mean","median","sd")  
)
