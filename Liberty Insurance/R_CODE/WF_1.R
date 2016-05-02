#TO CREATE CORELATION PLOT MATRIX

cor(train)

#CHECK BY REMOVING COLUMNS HAVING CHARACTER AS DATA

new_te <- (train[,!sapply(1:ncol(train), function(i) is.character(train[[i]]))])
#ONE MORE COOL ALTERNATE WY

new_te_1 <- train[,!sapply(train, is.character)]


df <- (cor(new_te))


#check for high corelation( positive)

sapply(df, function(i) i > 0.7)

#substitute for columns where value is true

require(corrplot)

corrplot(df, method = "ellipse",order = "hclust")

img <- corrplot(df, method = "number",order = "hclust",type='lower', diag=F, addCoefasPercent=T)

dev.copy(jpeg,filename="corelation_plot.png")

dev.off ()

#make a cross validation profile


