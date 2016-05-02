library(earth)

working.dir = '/home/sloth/Documents/Portable/Freelancing/kaggle_onSloth/2015_07_07_liberty_risk_assessment/'
setwd(working.dir)

#'NormalizedGini' is the other half of the metric. This function does most of the work, though
SumModelGini <- function(solution, submission) {
  df = data.frame(solution = solution, submission = submission)
  df <- df[order(df$submission, decreasing = TRUE),]
  df
  df$random = (1:nrow(df))/nrow(df)
  df
  totalPos <- sum(df$solution)
  df$cumPosFound <- cumsum(df$solution) # this will store the cumulative number of positive examples found (used for computing 'Model Lorentz')
  df$Lorentz <- df$cumPosFound / totalPos # this will store the cumulative proportion of positive examples found ('Model Lorentz')
  df$Gini <- df$Lorentz - df$random # will store Lorentz minus random
  return(sum(df$Gini))
}
NormalizedGini <- function(solution, submission) {
  SumModelGini(solution, submission) / SumModelGini(solution, solution)
}

input.column = 3:34

#initialize values
run = 1
gini.score = 0

kfold = 10
nreps = 3

#load train data
train.data = read.csv(paste0(working.dir,'/data/train.csv'))


#CAN START REPEAT TRIALS HERE

for (reps in 1:nreps){
  
  #get data
  random.sample = data.frame(index = 1:nrow(train.data), sample = sample(1:kfold, nrow(train.data), replace = TRUE))
  for (k in 1:kfold){
    
    train.sample = subset(random.sample, sample != k)[,1]
    test.sample = subset(random.sample, sample == k)[,1]
    
    model = earth(Hazard ~ ., data=(train.data[train.sample, c(input.column, 2)]))
    print(paste0('Model generated.'))
    
    pred = as.vector( predict(model, train.data[test.sample, ][c(input.column)]) )
    print(paste0('Predictions generated.'))
    
    true = train.data[test.sample,2]
    
    gini.score[run] = NormalizedGini(true, pred)
    
    print(paste0('Scores generated - rep:',reps,' k:',k,' score:',gini.score[run],' mean score:',mean(gini.score)))
    
    run = run + 1
    
  }#for each k(fold)
}#for each n rep
