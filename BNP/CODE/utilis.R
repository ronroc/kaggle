# create function to compute logloss on validation set using ground truth----------------------------------

checkLogLoss <- function(model, data) {
  
  # LogLoss Function

  LogLoss <- function(actual, predicted, eps=0.00001) {

        predicted <- pmin(pmax(predicted, eps), 1-eps)
    
        -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
  
        }
  
  # create dummy predictions and compare with fitted model
  
    pred <- as.matrix(predict(model, newdata = data, type = 'prob'))
  
    LogLoss(truth, pred)

    }