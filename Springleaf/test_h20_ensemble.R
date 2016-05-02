train <- read.table("http://www.stat.berkeley.edu/~ledell/data/higgs_10k.csv", sep=",")
test <- read.table("http://www.stat.berkeley.edu/~ledell/data/higgs_test_5k.csv", sep=",")

training_frame <- as.h2o(localH2O, train)
validation_frame <- as.h2o(localH2O, test)
y <- "V1"
x <- setdiff(names(training_frame), y)
family <- "binomial"
training_frame[,c(y)] <- as.factor(training_frame[,c(y)])  #Force Binary classification
validation_frame[,c(y)] <- as.factor(validation_frame[,c(y)])  # check to validate that this guarantees the same 0/1 mapping?



learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")
metalearner <- "SL.glm"


# Train the ensemble using 5-fold CV to generate level-one data
# More CV folds will take longer to train, but should increase performance
fit <- h2o.ensemble(x = x, y = y, 
                    training_frame = training_frame, 
                    family = family, 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5, shuffle = TRUE))

