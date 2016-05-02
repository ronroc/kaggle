require(caret); require(readr); require(ROCR); require(quadprog)  

set.seed(8675309) 

first <- read_csv("D:/kaggle/Springleaf/SUBMISSION/09152015_1.csv")

second <- read_csv("D:/kaggle/Springleaf/SUBMISSION/09152015_2.csv")

third <- read_csv("D:/kaggle/Springleaf/SUBMISSION/ensemble_4.csv")

fourth <- read_csv("D:/kaggle/Springleaf/SUBMISSION/ensemble_3.csv")

fifth <- read_csv("D:/kaggle/Springleaf/SUBMISSION/ensemble_2.csv")

seventh <- read_csv("D:/kaggle/Springleaf/SUBMISSION/ensemble_1.csv")

eigth <- read_csv("D:/kaggle/Springleaf/SUBMISSION/ten.csv")

ninth <- read_csv("D:/kaggle/Springleaf/SUBMISSION/eight_2.csv")

tenth <- read_csv("D:/kaggle/Springleaf/SUBMISSION/eight.csv")



X=cbind(first$target[1:145231], second$target[1:145231], third$target[1:145231], fourth$target[1:145231], 
        
        fifth$target[1:145231], seventh$target[1:145231]
        
        , eigth$target[1:145231], ninth$target[1:145231], tenth$target[1:145231])



train <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

test <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")

y = train$target


optimumWeight <-function(X,y){
  
  varnum=ncol(X)
  
  w <- solve.QP( 
  
      t(X) %*% X, t(y) %*% X, 
    
      cbind(  # One constraint per COLUMN
      
        matrix(1, nr=varnum, nc=1),
      
        diag(varnum),
      
        -diag(varnum)
    ),
    
    c(1, rep(0,varnum), rep(-1,varnum)), 
    
    meq = 1 # Only the first constraint is an equality, the others are >=
  )
  
  return(w$solution)
  
}

target <- optimumWeight(X,y)[1]* first$target +
  optimumWeight(X,y)[2]* second$target +
  optimumWeight(X,y)[3]* fourth$target + 
  optimumWeight(X,y)[4]* fifth$target + 
  optimumWeight(X,y)[5]* seventh$target+
  optimumWeight(X,y)[6]* eigth$target + 
  optimumWeight(X,y)[7]* ninth$target + 
  optimumWeight(X,y)[8]* tenth$target + 
  optimumWeight(X,y)[9]* third$target

sub <- data.frame(ID=test$ID)

sub$target <- target

write_csv(sub, "D:/kaggle/Springleaf/SUBMISSION/09192015_3.csv")
