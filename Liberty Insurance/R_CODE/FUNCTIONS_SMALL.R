#CODE ADJUSTMENTS TO RUN IN PARALLEL ,CARET

NormalizedGini <- function(data, lev = NULL, model = NULL) {
  
  SumModelGini <- function(solution, submission) {
    
    df = data.frame(solution = solution, submission = submission)
    
    df <- df[order(df$submission, decreasing = TRUE),]
    
    df$random = (1:nrow(df))/nrow(df)
    
    totalPos <- sum(df$solution)
    
    df$cumPosFound <- cumsum(df$solution) # this will store the cumulative number of positive examples found (used for computing "Model Lorentz")
    
    df$Lorentz <- df$cumPosFound / totalPos # this will store the cumulative proportion of positive examples found ("Model Lorentz")
    
    df$Gini <- df$Lorentz - df$random # will store Lorentz minus random
    
    return(sum(df$Gini))
  }
  
  solution=data$obs
  
  submission=data$pred
  
  result=SumModelGini(solution, submission) / SumModelGini(solution, solution)
  
  names(result) <- "Gini"
  
  result
}
