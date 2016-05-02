---
  title: "Explore Dataset - Property Inspection Prediction"
author: "Justfor"
date: "10. Juli 2015, latest update 25 Juli 2015"
output: html_document
---
  
  This documents explores the data of the Kaggle competition: 
  Liberty Mutual Group: Property Inspection Prediction

The Website of the Kaggle competition is <https://www.kaggle.com/c/liberty-mutual-group-property-inspection-prediction>.

```{r, message=FALSE}

library(readr)

library(caret)

time.1 <- Sys.time()

format(time.1, "%d-%m-%Y--%H:%M:%S")

```

Loading the data with read_csv (package readr) with character to factor conversion is faster than read.csv from base package.

```{r}
# load raw data
## read.csv is slower though does not need Factor conversion from Characters
#train = read.csv('../input/train.csv')
#test = read.csv('../input/test.csv')

print("Reading data")
train <- read_csv("../input/train.csv")
test <- read_csv("../input/test.csv")

# We'll convert all the characters to factors so we can train a randomForest model on them
extractFeatures <- function(data) {
  character_cols <- names(Filter(function(x) x=="character", sapply(data, class)))
  for (col in character_cols) {
    data[,col] <- as.factor(data[,col])
    data[,col] <- ordered(data[,col])
    # if needed, conversion to numeric: data[,col] <- as.numeric(data[,col])
  }
  return(data)
}

# We'll convert all the characters to numeric to make a correlation Plot
extractFeatures2 <- function(data) {
  character_cols <- names(Filter(function(x) x=="character", sapply(data, class)))
  for (col in character_cols) {
    data[,col] <- as.factor(data[,col])
    data[,col] <- ordered(data[,col])
    data[,col] <- as.numeric(data[,col])
  }
  return(data)
}

train_numeric <- extractFeatures2(train)
test_numeric  <- extractFeatures2(test)

train <- extractFeatures(train)
test  <- extractFeatures(test)

#No missing values
length(train[is.na(train)])
length(test[is.na(test)])

dim(train)
dim(test)

names(train)
names(test)

str(train)
str(test)


# Id column not necessary in DataFrame
train_ids <- train$Id
test_ids <- test$Id
train$Id <- NULL
test$Id <- NULL

# Id column not necessary in DataFrame
train_numeric$Id <- NULL
test_numeric$Id <- NULL

# Hazard column is separate and not necessary in DataFrame
y <- train$Hazard
train$Hazard <- NULL
```

Summarize the Hazard variable, the train and test set

```{r}
# summarize the Hazard or y variable, note that not every number is used (50 Levels only)
summary(y)
summary(as.factor(y))

length(levels(as.factor(y)))

tbl <- table(as.factor(y))
#cbind(Count=tbl,Perc=round(prop.table(tbl)*100,2))
#addmargins(round(prop.table(tbl)*100,2))
round(prop.table(tbl)*100,2)

summary(train)
summary(test)
```

Finding near zero variance variables and correlated variables.

```{r}
# Find near zero values
nzv <- nearZeroVar(train, saveMetrics= TRUE)
print(paste('Range:',range(nzv$percentUnique)))
nzv
nzv <- nearZeroVar(train, saveMetrics= FALSE)
filteredtrain <- train[, -nzv]
dim(filteredtrain)
names(filteredtrain)

nzv_test <- nearZeroVar(test, saveMetrics= TRUE)
print(paste('Range:',range(nzv_test$percentUnique)))
nzv_test
nzv_test <- nearZeroVar(test, saveMetrics= FALSE)
filteredtest <- test[, -nzv_test]
dim(filteredtest)
names(filteredtest)

# get the categorical columns
fact_cols = c('T1_V4', 'T1_V5', 'T1_V6', 'T1_V7', 'T1_V8', 'T1_V9', 'T1_V11', 
              'T1_V12', 'T1_V15', 'T1_V16', 'T1_V17', 'T2_V3', 'T2_V5', 'T2_V11', 'T2_V12',
              'T2_V13')

fact_train = train[fact_cols]
fact_test = test[fact_cols]

# Find Unique values
apply(fact_train, 2, unique)
apply(fact_test, 2, unique)

# Show numerical columns
nt <- names(train)
num_cols <- nt[!names(train) %in% fact_cols]
num_cols
#put the numerical as matrix
num_train_data <- train[,-match(fact_cols, colnames(train))]
num_test_data <- test[,-match(fact_cols, colnames(test))]

apply(num_train_data, 2, summary)
apply(num_test_data, 2, summary)


# Find correlation of numerical columns
descrCor <-  cor(num_train_data)
descrCor
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
highCorr
# Result: 0

# Use train which was converted from Factor to numeric
# Find correlation of all columns
descrCor2 <-  cor(train_numeric)
descrCor2
highCorr2 <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
highCorr2

# Result still 0
```


Linear Dependencies

```{r}
linearCombos <- findLinearCombos(train_numeric)
length(train_numeric)
LINEAR <- linearCombos$remove
length(LINEAR)
```


Some plots to visualize the data.

```{r}

# Show some plots
plot(y)
hist(y)
hist(log(y))
boxplot(y)

# Takes too long as script
# library(psych)
# pairs.panels(fact_train)
# pairs.panels(num_train_data)

## Show some fancy Correlation plots
# First alphabetic
library(corrplot)
M <- cor(train_numeric[sapply(train_numeric, function(x) !is.character(x))])
corrplot.mixed(M, order = "alphabet", lower = "circle", upper = "number", tl.cex = 0.8)

#Second as cluster
col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white", "cyan", 
                           "#007FFF", "blue", "#00007F"))
corrplot(M, order = "hclust", cl.ratio = 0.2, cl.align = "r", col = col1(100))

#Third with calculation of confidence level
cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(train_numeric, 0.95)
## leave blank on no significant coefficient
corrplot(M, p.mat = res1[[1]], insig = "blank")
corrplot(M, p.mat = res1[[1]], cl.ratio = 0.2, cl.align = "r", col = col1(100))
## Not Run
## add p-values on no significant coefficient
## corrplot(M, p.mat = res1[[1]], insig = "p-value")

```

The corrplot showed some correlation: 
  - T1_V12 vs T1_V17 being the strongest, but also further correlations exist.
- T1_V9 vs. T1_V5
- T1_V16 vs T1_V5 and T1_V9

Especially the cluster plot is interesting.

```{r}
time.2 <- Sys.time()
cat(sprintf("%.1f", as.numeric(difftime(time.2, time.1, units="secs"))), " secs\n")
```