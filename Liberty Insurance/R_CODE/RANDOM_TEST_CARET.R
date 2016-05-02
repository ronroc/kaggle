xgbGrid <- expand.grid(nrounds = c(1, 10),
                       max_depth = c(1, 4),
                       eta = c(.1, .4))

set.seed(2)

training <- twoClassSim(100, linearVars = 2)

testing <- twoClassSim(500, linearVars = 2)

trainX <- training[, -ncol(training)]

trainY <- training$Class

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary)

cctrl2 <- trainControl(method = "LOOCV",
                       classProbs = TRUE, summaryFunction = twoClassSummary)


cctrl3 <- trainControl(method = "oob")

cctrl4 <- trainControl(method = "none",
                       classProbs = TRUE, summaryFunction = twoClassSummary)

cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all" )

test_class_cv_model <- train(trainX, trainY, 
                             method = "xgbTree", 
                             trControl = cctrl1,
                             metric = "ROC", 
                             preProc = c("center", "scale"),
                             tuneGrid = xgbGrid)
