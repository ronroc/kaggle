char <- c()

for(i in 1:length(names(train)))
{
  if(class(train[[i]]) != "integer" | class(train[[i]]) != "numeric")
    
    char <- c(char, names(train)[i])
  
}


for(i in 1:length(names(train))) a <- (class(train[[i]]))
