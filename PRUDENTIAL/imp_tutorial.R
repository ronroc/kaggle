start <- Sys.time()

library(mlr); library(readr); require(caret)

## Read Data

train <- fread("D:\\kaggle\\PRUDENTIAL\\Data\\train.csv", data.table = F)

test  <- fread("D:\\kaggle\\PRUDENTIAL\\Data\\test.csv", data.table = F)

test$Response = 0


cat.var.names <- c(paste("Product_Info_", c(1:3,5:7), sep=""), paste("Employment_Info_", c(2,3,5), sep=""),
                   
                   paste("InsuredInfo_", 1:7, sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""), 
                   
                   "Family_Hist_1", paste("Medical_History_", c(2:14, 16:23, 25:31, 33:41), sep=""))


## store Id column and remove it from the train and test data

testId = test$Id

train$Id = test$Id = NULL


## create mlr task and convert factors to dummy features

trainTask = makeRegrTask(data = train, target = "Response")

testTask = makeRegrTask(data = test, target = "Response")


## create mlr learner

set.seed(12302015)

lrn = makeLearner("regr.xgboost")

lrn$par.vals = list(

    #nthread             = 30,
  
  nrounds             = 4000,
  
  print.every.n       = 21,
  
  objective           = "reg:linear",
  
  subsample = 0.7,
  
  colsample_bytree=0.65

  )

# missing values will be imputed by their median

lrn = makeImputeWrapper(lrn, classes = list(numeric = imputeMedian(), integer = imputeMedian()))

## Create Evaluation Function

SQWKfun = function(x = seq(1.5, 7.5, by = 1), pred) {

    preds = pred$data$response
  
    true = pred$data$truth 
  
    cuts = c(min(preds), x[1], x[2], x[3], x[4], x[5], x[6], x[7], max(preds))
  
    preds = as.numeric(Hmisc::cut2(preds, cuts))
  
    err = Metrics::ScoreQuadraticWeightedKappa(preds, true, 1, 8)
  
    return(-err)

    }

SQWK = makeMeasure(id = "SQWK", minimize = FALSE, properties = c("regr"), best = 1, worst = 0,

                                      fun = function(task, model, pred, feats, extra.args) {
                   
                                      return(-SQWKfun(x = seq(1.5, 7.5, by = 1), pred))
                   })

## This is how you could do hyperparameter tuning

# # 1) Define the set of parameters you want to tune (here 'eta')

ps = makeParamSet(

    makeNumericParam("eta", lower = 0.01, upper = 0.04)
    
    )

# # 2) Use 3-fold Cross-Validation to measure improvements

rdesc = makeResampleDesc("CV", iters = 3L)

# # 3) Here we use Random Search (with 10 Iterations) to find the optimal hyperparameter

ctrl =  makeTuneControlRandom(maxit = 30)

#ctrl = makeTuneControlIrace(maxExperiments = 200)

# # 4) now use the learner on the training Task with the 3-fold CV to optimize your set of parameters and evaluate it with SQWK

res = tuneParams(lrn, task = trainTask, resampling = rdesc, par.set = ps, control = ctrl, measures = SQWK)

res

# # 5) set the optimal hyperparameter

lrn = setHyperPars(lrn, par.vals = res$x)

## now try to find the optimal cutpoints that maximises the SQWK measure based on the Cross-Validated predictions

cv = crossval(lrn, trainTask, iter = 4, measures = SQWK, show.info = TRUE)

optCuts = optim(seq(1.5, 7.5, by = 1), SQWKfun, pred = cv$pred)

optCuts

## now train the model on all training data

tr = train(lrn, trainTask)

## predict using the optimal cut-points 

pred = predict(tr, testTask)

preds = as.numeric(Hmisc::cut2(pred$data$response, c(-Inf, optCuts$par, Inf)))

table(preds)

## create submission file

submission = data.frame(Id = testId)

submission$Response = as.integer(preds)

write.csv(submission, "D:\\kaggle\\PRUDENTIAL\\submission\\12302015\\12302015_1.csv", row.names = FALSE)
