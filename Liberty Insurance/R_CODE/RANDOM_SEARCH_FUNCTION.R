random_search <- function(n_set, threads){
  
  #param is a list of parameters
  
  # Set necessary parameter
  param <- list("objective" = "multi:softprob",
                "max_depth"=6,
                "eta"=0.1,
                "subsample"=0.7,
                "colsample_bytree"= 1,
                "gamma"=2,
                "min_child_weight"=4,
                "eval_metric" = "mlogloss",
                "silent"=1,
                "num_class" = 9,
                "nthread" = threads)
  
  param_list <- list()
  
  for (i in seq(n_set)){
    
    ## n_par <- length(param)
    
    
    param$max_depth <- sample(3:7,1, replace=T)
    
    param$eta <- runif(1,0.01,0.6)
    param$subsample <- runif(1,0.1,1)
    param$colsample_bytree <- runif(1,0.1,1)
    param$min_child_weight <- sample(1:17,1, replace=T)
    
    param$gamma <- runif(1,0.1,10)
    param$min_child_weight <- sample(1:15,1, replace=T)
    
    param_list[[i]] <- param
    
  }
  
  return(param_list)
  
}