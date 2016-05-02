library(h2o)

localH2O <- h2o.init()

iris.hex <- as.h2o(iris, localH2O, "iris.hex")

model_id <- h2o.gbm(x = 1:4, y = 5, training_frame = iris.hex)@model_id

model.retrieved <- h2o.getModel(model_id, localH2O)

############################################################################################

Example- 2 :
  
model_id <- h2o.gbm(x = 1:4, y = 5, training_frame = iris.hex, 
                    
                     keep_cross_validation_predictions = T, 
                    
                    ntrees = 40, max_depth = 20, nfolds = 5, model_id = "gbm_test",
                    
                    nbins_cats = 20)

h2o.getModel(model_id = model_id@model$cross_validation_models[[1]]$name)

h2o.performance(model = model_id, data = iris.hex, valid = T)

h2o.mse(object = h2o.performance(model = model_id, data = iris.hex, valid = T), train = T, xval = T, valid = T)

model_id@model$cross_validation_metrics

##############################################################################################

Example- 3:
  
hyper_params = list(ntrees=c(50,100,200),max_depth=c(2,3,4), 
                      
                      learn_rate=c(0.1,0.2))

model_id <- h2o.grid(algorithm = "gbm", x = 1:4, y = 5, training_frame = iris.hex, 
                    
                    keep_cross_validation_predictions = T, hyper_params = hyper_params   
                    
                    )

summary(model_id)

model_id@hyper_names
