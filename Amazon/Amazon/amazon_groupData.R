
require(readr)

train <- read_csv("C:/Users/amulya/Desktop/train_amazon.csv")

test <- read_csv("C:/Users/amulya/Desktop/test_amazon.csv")

train <- train[ , -10]

train_y <- as.matrix(train[ ,1])

train_new <- train[ ,-1]

test_new <- test[ , c(2:9)]

tmp <- rbind(train_new, test_new)

tmp <- as.matrix(tmp)

##group_Data function

groupData <- function(xmat, Degree){
  
  require(foreach, quietly = T)
  
  #indices of combinations
  
  xind <- combn(1:ncol(xmat), Degree)
  
  #storage structure for the result
  
  agx <- foreach(ii = 1:ncol(xind), .combine = cbind) %do%
  
    {
      x <- xmat[ , xind[1, ii]]
      
      for(jj in 2:nrow(xind))
        
      {
        x <- paste(x, xmat[ , xind[jj, ii]], sep = "_")
        
      }
      x
    }
  colnames(agx) <- paste(paste("f", Degree, sep = ""), 1:ncol(agx), sep = "_")
  
  return(agx)
}


amazon_X = cbind(tmp, groupData(tmp, 2), groupData(tmp, 3))
