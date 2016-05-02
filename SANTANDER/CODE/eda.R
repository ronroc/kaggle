# change file names

require(sqldf); require(ggplot2); require(data.table);require(bit64); require(gridExtra)

Spanish2English <- fread("D:\\kaggle\\SANTANDER\\DATA\\Spanish2English.csv", data.table = F)

# df <- fread("page_1.csv",  data.table = F )

train <- fread("D:\\kaggle\\SANTANDER\\DATA\\train.csv", data.table = F)

tmp <- cbind.data.frame( ID = train$ID, 
                         
                         var3 = train$var3, 
                         
                         var15 = train$var15, 
                         
                         var38 = train$var38, 
                         
                         TARGET = train$TARGET )


train <- train[, !(names(train) %in% names(tmp))]

names(train) <- Spanish2English$English

train <- cbind(train, tmp)



test <- fread("D:\\kaggle\\SANTANDER\\DATA\\test.csv", data.table = F)

tmp <- cbind.data.frame( ID = test$ID, 
                         
                         var3 = test$var3, 
                         
                         var15 = test$var15, 
                         
                         var38 = test$var38 )

test <- test[, !(names(test) %in% names(tmp))]

names(test) <- Spanish2English$English

test <- cbind(test, tmp)


# train_con <- sqldf('select Variable_Name
#                    
#                    from df
#                    
#                    where Type_of_Variable = "Con"')
# 
# 
# train_Cat <- sqldf('select Variable_Name
#                    
#                    from df
#                    
#                    where Type_of_Variable = "Cat"')



# seperate categorical and continous features using code---------------------------------------


# 
# features <- train_con$Variable_Name
# 
# train.cont <- (train[, features])
# 
# train.cont$Response <- train$TARGET


# find unique values and number of unique in train data------------------------------------------------------------------------------


number_unique <- rep(0, ncol(train))

for(i in 1:ncol(train))

  {

number_unique[i] <- length(unique(train[[i]]))

}

number_unique <- data.frame(Name = names(train), no = number_unique)

  
# histogram for categorical variables-------------------------------------------------------------------------------------------------

plotHist <- function( data.in, i ) 
  
{
  data <- data.frame( x = data.in[,i] )
  
  p <- ggplot( data=data, aes(x=factor(x))) + 
    
       geom_histogram() + 
    
       xlab(colnames(data.in)[i]) + 
    
       theme_light() + 
    
       theme(axis.text.x=element_text(size=8))
  
  return (p)
  
}


doPlots <- function(data.in, fun, ii, ncol=3) 
  
{
  
  pp <- list()
  
  for (i in ii) 
    
  {
    
    p <- fun(data.in=data.in, i=i)
    
    pp <- c(pp, list(p))
    
  }
  
  do.call("grid.arrange", c(pp, ncol=ncol))
  
}


# densities for continous variables-------------------------------------------------------------------------------------------------


plotDensity <- function(data.in, i) 
  
{
  
  data <- data.frame(x=data.in[,i], Response=data.in$Response)
  
  p <- ggplot(data) + #geom_density(aes(x=x, colour=factor(Response))) + 
    
       geom_line(aes(x=x), stat="density", size=1, alpha=1.0) +
    
       xlab(colnames(data.in)[i]) + theme_light()
  
  return (p)
  
}


# Box plots of continous features depending on response-----------------------------------------------------------------------------


plotBox <- function(data.in, i) 
  
{
  data <- data.frame(y=data.in[,i], Response=data.in$Response)
  
  p <- ggplot(data, aes(x=factor(Response), y=y)) + 
    
       geom_boxplot() + 
    
       ylab(colnames(data.in)[i]) + 
    
       theme_light()
  
  return (p)
  
}


png(filename="name.png")

doPlots(data.in=train.cont, fun=plotDensity, ii=1:4, ncol=2)

dev.off()

##############################################################################################
##############################################################################################

library(ggplot2)

library(readr)

library(Rtsne)

features <- train[, c(-367, -371)]

tsne <- Rtsne( as.matrix(features), check_duplicates = FALSE, pca = T, 
               
               perplexity=30, theta=0.5, dims=2 )

embedding <- as.data.frame(tsne$Y)

embedding$Class <- as.factor(train$Response)


ggplot(embedding, aes(x=V1, y=V2, color=Class)) +
  
  geom_point(size=1.25) +
  
  guides(colour = guide_legend(override.aes = list(size=6))) +
  
  xlab("") + ylab("") +
  
  ggtitle("t-SNE 2D Embedding") +
  
  theme_light(base_size=20) +
  
  theme(strip.background = element_blank(),
        
        strip.text.x     = element_blank(),
        
        axis.text.x      = element_blank(),
        
        axis.text.y      = element_blank(),
        
        axis.ticks       = element_blank(),
        
        axis.line        = element_blank(),
        
        panel.border     = element_blank()
        
  )


##############################################################################################
##############################################################################################