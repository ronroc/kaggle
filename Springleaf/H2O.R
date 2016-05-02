require(h2o)

localH2o <- h2o.init(nthreads = -1, max_mem_size = "10g")

feature.names <- names(train)[2:(ncol(train)) - 2]

train.hex <- as.h2o(localH2o, train)

mooc.hex.split <- h2o.splitFrame(train.hex, ratios = 0.8)

train.rf <- h2o.randomForest(x = feature.names, y = "target", 
                             
                             training_frame = mooc.hex.split[[1]],
                             
                             validation_frame = mooc.hex.split[[2]],
                             
                             ntrees = 500, mtries = 8, balance_classes = T)


train.gbm <- h2o.gbm(x = feature.names, y = "target", 
                     
                     training_frame = mooc.hex.split[[1]],
                     
                     validation_frame = mooc.hex.split[[2]],
                     
                     ntrees = 500, max_depth = 10, learn_rate = 0.1, 
                     
                     balance_classes = T)

train.gbm$@model$validation_metrics$metrics$AUC

test.hex <- as.h2o(localH2o, test)

preds.rf <- h2o.predict(train.rf, test.hex[, feature.names])

preds.gbm <- h2o.predict(train.gbm, test.hex[, feature.names])

submission_second <- data.frame(ID=test$ID)

submission_second$target <- preds.gbm 

h2o.exportFile(preds.gbm, "new.csv")

###############################################################################