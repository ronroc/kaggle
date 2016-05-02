library("doMC")

bagging.gen = function(formula, data, nbags, fraction, modeltype, ...) {
  ensemble = foreach (i = 1:nbags) %do% {
    modeltype(formula, data[sample(nrow(data), nrow(data)*fraction, replace = 1), ], ...)
  }
  class(ensemble) = 'bagging.gen'
  ensemble
}

predict.bagging.gen = function(ensemble, newData) {
  preds = foreach (i=iter(ensemble), .combine=cbind) %do% {
    predict(i, newData)
  }
  apply(preds, 1, mean)
}
