train <- read.csv("train_combined.csv")

test <- read.csv("test_combined.csv")

num_models <- 24;     #(num_models + 1) is the response from the train column

itertions <- 1000

auc_mat <- matrix(0, num_models, 2)

auc_mat[ , 2] <-  1 : num_models

for(i in 1:num_models) {
  
  auc_mat[i,1] <- auc(train[,i],train[,num_models+1])
  
  print(auc(train[,i],train[,num_models+1]))
  
}

best_model_no <- auc_mat[auc_mat[,1] == min(auc_mat[,1]),2]

auc(train[,best_model_no],train[,num_models+1])

x <- matrix(0,1000,itertions)

prediction_test <- matrix(0,nrow(test),1)

prediction_train <- matrix(0,nrow(train),1)

for (j in 1:itertions){
  
  auc_in <- 1
  
  auc_new <- matrix(0,num_models,2)
  
  auc_new[,2] <- 1:num_models
  
  print(j)
  
  t = 1
  
  set.seed(j*121)
  
  train1 <- train[sample(1:nrow(train), 10000,replace=FALSE),]
  
  for(i in 1:num_models){
    
    auc_mat[i,1] <- auc(train1[,i],train1[,num_models+1])
    
  }
  
  best_model_no <- auc_mat[auc_mat[,1] == min(auc_mat[,1]),2]
  
  prediction <- train1[,best_model_no]
  
  prediction_1 <- test[,best_model_no]
  
  prediction_2 <- train[,best_model_no]
  
  x[t,j] <- best_model_no
  
  while(-1 < 0) {
    
    t <- t + 1
    
    prediction1 <- prediction
    
    for (i in 1:num_models){
      
      prediction1 <- ((t*prediction) + train1[,i])/(t+1)
      
      auc_new[i,1] <-  auc(prediction1,train1[,num_models+1])
      
    }
    
    auc_min <- min(auc_new[,1])
    
    model_no <- auc_new[auc_new[,1] == min(auc_new[,1]),2]
    
    if(auc_in < rmsle_min) {break} else {
      
      auc_in <- auc_min
      
      prediction <- (((t-1)*prediction) + train1[,model_no])/t
      
      prediction_1 <- (((t-1)*prediction_1) + test[,model_no])/t
      
      prediction_2 <- (((t-1)*prediction_2) + train[,model_no])/t
      
      x[t,j] <- model_no
      
      print(auc_in)
      
    }
    
  }
  
  prediction_test <- cbind(prediction_test,prediction_1)
  
  prediction_train <- cbind(prediction_train,prediction_2)
  
}
