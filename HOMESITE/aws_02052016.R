install.packages("doParallel"); install.packages("h2o"); install.packages("readr")

require(doParallel); require(h2o); require(caret); require(readr)

detectCores()

train <- read_csv("train_01262016.csv")

test <- read_csv("test_01262016.csv")

response_1 <- read_csv("response_01262016.csv")

response <- response_1$response

# create folds------------------------------------------------------------------------------------------

dataset_blend_train = matrix(0, nrow(train), 1)

dataset_blend_test_j = matrix(0, nrow(test), 2) # this hould always be number of folds

dataset_blend_test = matrix(0, nrow(test), 1)

# start iteration loop---------------------------------------------------------------------------------

j = 1

print(paste("starting gbm iteration ; number :", j))

set.seed(2*05*2016*j)

require(caret)

skf = createFolds(response, k = 2)

print(paste(nrow(dataset_blend_test_j),ncol(dataset_blend_test_j)))

# start fold loop------------------------------------------------------------------------------------

### Loop over the folds

i <- 0

for (sk in skf) {
  
  i <- i + 1
  
  print(paste("Fold", i))
  
  ### Extract and fit the train/test section for each fold
  
  tmp_train <- unlist(skf[i])
  
  x_train = train[-tmp_train,]
  
  y_train = response[-tmp_train]
  
  x_test  = train[tmp_train,]
  
  y_test  = response[tmp_train]
  
  
  require(h2o)
  
  localH2O <- h2o.init(nthreads = 36, assertion = F)
  
  x_train$target <- as.factor(y_train)
  
  train.hex <- as.h2o(localH2O, object = x_train)
  
  test.hex <- as.h2o(localH2O, object = x_test)
  
  
  myX <- names(train)
  
  myY <- "target"
  
  print(paste("training gbm for iteration :", j, " Fold ; number :", i))
  
  
  test_gbm <- h2o.gbm(x = myX,
                      
                      y = myY,
                      
                      training_frame = train.hex, 
                      
                      ntrees =  2000, 
                      
                      max_depth = 10, 
                      
                      learn_rate = 0.014,
                      
                      distribution="bernoulli", 
                      
                      balance_classes = T
  )
  
  
  pred_gbm <- h2o.predict(object = test_gbm, newdata = test.hex)
  
  pred_gbm <- as.data.frame(pred_gbm)
  
  dataset_blend_train[tmp_train, j] <- pred_gbm$p1
  
  print(paste("predicting gbm for test set iteration :", j,  "; Fold :", i))
  
  test.hex <- as.h2o(localH2O, object = test)
  
  pred_gbm <- h2o.predict(object = test_gbm, newdata = test.hex)
  
  pred_gbm <- as.data.frame(pred_gbm)
  
  dataset_blend_test_j[, i] <- pred_gbm$p1
  
}

dataset_blend_test[, j] <- rowMeans(dataset_blend_test_j)


require(readr)

write_csv(data.frame(dataset_blend_train), "blend_train_gbm_02062016.csv")

write_csv(data.frame(dataset_blend_test), "blend_test_gbm_02062016.csv")