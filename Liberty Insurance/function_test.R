train <- read_csv("D:/Kaggle/Liberty Insurance/DATA/CSV/train.csv")

test <- read_csv("D:/Kaggle/Liberty Insurance/DATA/CSV/test.csv")



factorToNumeric <- function(train, test, response, variables, metrics){

  library(qdapTools)
  
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

# convert all character to numeric

for (f in names(train)) {
  
  if (class(train[[f]]) == "character") {
    
    levels <- unique(c(train[[f]]))
    
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    
    
  }
}


# convert all numeric to factors


len = length(train)

for (i in 1:len) {
  
  print(paste0( i / (len) *100, "%"))
  
  levels <- unique(train[[i]])
  
  train[ , i] <- factor(train[ , i], levels = levels)
  
}


head(factorToNumeric(train, train, "Hazard", "T1_V4", c("mean","median","sd")))
