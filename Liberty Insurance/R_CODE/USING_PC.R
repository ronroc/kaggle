library(readr)
library(caret)
library(corrplot)

# Custom functions
factor2integer <- function(x)
  return(match(x, unique(x)))


# The competition datafiles are in the directory ../input
# Read competition data files:

train.data <- read.csv("../input/train.csv", stringsAsFactors=T)
test.data <- read.csv("../input/test.csv", stringsAsFactors=T)

# Write to the log:
cat(sprintf("Training set has %d rows and %d columns\n", nrow(train.data), ncol(train.data)))
cat(sprintf("Test set has %d rows and %d columns\n", nrow(test.data), ncol(test.data)))

train.features <- train.data[, -(1:2)]

#--- Cast factors to associated integers
train.cast <- as.data.frame(train.features <- lapply(train.features, function(x) {
  if(is.factor(x)) {
    factor2integer(x)
  } else {
    x
  }
}))

#--- Using caret select the biggest contributors
train.trans <- preProcess(train.cast, method=c("BoxCox", "center", "scale", "pca"))
pc <- as.data.frame(train.trans$rotation)
cor.pc <- cor(pc)
corrplot(cor.pc,
         
         order="hclust", method="color")
