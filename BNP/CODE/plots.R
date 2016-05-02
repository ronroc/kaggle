
require(data.table)

train_raw <- fread("D:\\kaggle\\BNP\\DATA\\train.csv", data.table = F)

test_raw <- fread("D:\\kaggle\\BNP\\DATA\\test.csv", data.table = F)

train_ID <- train_raw$ID 

test_ID <- test_raw$ID

response <- train_raw$target

train_raw$ID <- NULL; test_raw$ID <- NULL; train_raw$target <- NULL

# two stages EDA from now on , first understand distribution of features for the whole dataset,

# next do the same for train and test features

tmp <- rbind(train_raw, test_raw)

# check for categorical variables-----------------------------------------------------------------------

uniq <- c()

for(i in names(tmp)){
  
  tmp_1 <- length(unique(tmp[, i]))
  
  uniq <- c(uniq, tmp_1)
 
}

df_uniq <- data.frame(col1 = names(tmp), col2 = uniq) 


# now seperate categorical variables-------------------------------------------------------------------

char_var <- c()

for(i in names(tmp)){
  
  if(class(tmp[, i]) == "character"){
    
    char_var <- c(char_var, i)
  }
}

# length of character variables-----------------------------------------------------------------------

df_uniq[df_uniq$col1 %in% char_var, ]

table(tmp$v22)


continous_var <- names(tmp)[!(names(tmp) %in% char_var)]

tmp_char <- tmp[, char_var]


#####################################################################################################

require(png)

for( i in 1:19 ) {

  i = 19
  
png(filename = paste0("D:\\kaggle\\BNP\\IMG\\02272016\\", "hist_", names(tmp_char)[i], ".png"))

plotHist(tmp_char, i)

dev.off()

}


###########################################################################################

train_continous <- train_raw[, continous_var]

train_continous$Response <- response

require(png)


for( i in 1:19 ) {
  
  mypath = paste0("D:\\kaggle\\BNP\\IMG\\02272016\\continous\\", "density_", names(train_continous)[i], ".png")
  
  png(filename = mypath )
  
  plotDensity(data.in = train_continous, i = i)
  
  dev.off()
  
}


