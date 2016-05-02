require(caret)

require(Matrix)

data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

TrainData <- sparse.model.matrix(~., data = TrainData)

fit1 <- train(TrainData, TrainClasses,
              
             method = "xgbTree",
              
            trControl = trainControl(method = "cv", number = 5, classProbs = TRUE,
                                   
                                     ),
            metric = "ROC",


              "min_child_weight" = 10,
              
              "subsample" = .8,
              
              "colsample_bytree" = .8,
              
              "scale_pos_weight" = 1.0,
              
              "max_depth" = 5
              
              )

