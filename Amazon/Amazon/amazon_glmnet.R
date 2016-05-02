

##################################################################
library("glmnet")
test <- read.csv("test.csv", header=TRUE,
                 colClasses = c("integer", rep("factor", 9)))
train <- read.csv("train.csv", header=TRUE,
                  colClasses = c("numeric", rep("factor", 9)))

x_all = sparse.model.matrix(~ . -1, data = rbind(train[,-1],test[,-1]))

x_train = x_all[1:nrow(train),]
x_test <- x_all[(nrow(train)+1):nrow(x_all),]

print("parse completed")

model = cv.glmnet(
  x = x_train,
  y = train$ACTION,
  family = "binomial",
  standardize = F,
  alpha = 0.5,
  type.measure = "auc",
  intercept = T,
  nfolds = 10)

cat("Max cv.glmnet VAL AUC:", max(model$cvm, na.rm = T), "\n")

response_gmlnet <- predict(model, x_test, type="response", s = "lambda.min")
response_gmlnet <- data.frame(id = test$id, ACTION = response_gmlnet[,])

write.csv(response_gmlnet,
          file = "response_gmlnet.csv",
          row.names = F, quote = F)

#################################################################






I don't remember exact code but if you use all feature as numeric and use random forest then you can get 0.88

library(randomForest)

library(AUCRF)

#train

#test

#Load train and test please

feature <- AUCRF(ACTION~.,train)

#above command will do feature selection use those features only, most probably it will remove 3rd feature from data

tuneRF(train[,-1],as.factor(train[,1]),ntreeTry=400)

#use mtry from above code and run the randomForest. Don't forget to replace ??? below with mtry parameter u find from above

model <- randomForest(train[,-1],as.factor(train[,1]),mtry= ???, ntree=400)

#Please make the prediction

#!Cheers. If I can't move more on the leader board I will put my full code. I will explain all the feature I am using.