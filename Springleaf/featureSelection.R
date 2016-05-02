setwd("~/kaggle/ssr/YDM")

set.seed(20150630)

dataset = read.csv("train_900_new_features_markpeng_20150629.csv", stringsAsFactors=FALSE, encoding="UTF-8")

submitTest = read.csv("test_900_new_features_markpeng_20150629.csv", stringsAsFactors=FALSE, encoding="UTF-8")

dim(dataset)

dim(submitTest)


##############################
## Boruta feature selection ##
##############################

maxRuns = 100

library(Boruta)

important = Boruta(as.factor(median_relevance)~., 
                   data=dataset, 
                   maxRuns=maxRuns,
                   doTrace=2)
important

str(dataset[,c(important$finalDecision != "Rejected", TRUE)])

length(names(dataset[,c(important$finalDecision != "Rejected", TRUE)]))

capture.output(print(important), file="boruta_100maxRuns_summary957features_20150703.txt")

# save importance plot

dev.off()

pdf('boruta_100maxRuns_plot_957features_20150703.pdf')

plot(important)

dev.off()

# save chosen features
write.table(names(dataset[,c(important$finalDecision != "Rejected", TRUE)]), 
            "boruta_selected_957features_M+D_20150703.txt",
            append=FALSE, row.names=FALSE, col.names=FALSE, quote=FALSE)

## save new data set ##
selectedFeatures = names(dataset[,c(important$finalDecision != "Rejected", TRUE)])
newDataset = dataset[, names(dataset) %in% selectedFeatures]
newDataset$median_relevance = NULL
newDataset$median_relevance = dataset$median_relevance
str(newDataset)
dim(newDataset)

newsubmitTest = submitTest[, names(submitTest) %in% selectedFeatures]
str(newsubmitTest)
dim(newsubmitTest)

# combine to new csv
write.csv(newDataset, "train_Boruta_91features_M+D_markpeng_20150630.csv", 
          append=FALSE, row.names=FALSE, sep=",", quote=TRUE, fileEncoding="UTF-8")
write.csv(newsubmitTest, "test_Boruta_91features_M+D_markpeng_20150630.csv", 
          append=FALSE, row.names=FALSE, sep=",", quote=TRUE, fileEncoding="UTF-8")



##############################
## RFE feature engineering  ##
##############################

set.seed(20150630)

library(caret)

require(doParallel)

cl <- makeCluster(2)

registerDoParallel(cl)


rfFuncs$summary = wKappaSummary

refcontrol = rfeControl(functions=rfFuncs, 
                        method="repeatedcv", 
                        number=3,
                        repeats=3,
                        p=0.35,
                        verbose=TRUE)

refResult = rfe(dataset[,-ncol(dataset)], 
                as.factor(dataset$median_relevance), 
                sizes=seq(50,950,50), 
                rfeControl=refcontrol,
                metric="wKappa",
                maximize=TRUE,
                verbose=TRUE)
# summarize the results
print(refResult)
# list the chosen features
predictors(refResult)

capture.output(print(refResult), file="summary_rfe_957features_50to500_kappa_20150701.txt")

# save chosen features
write.table(predictors(refResult), "rfe_selected_200features_50to500_kappa_20150701.txt",
            row.names = FALSE,
            col.names = FALSE,
            quote=FALSE)

# Save pdf of plots
dev.off()
pdf('rfe_selected_200features_50to500_kappa_20150701_plots.pdf')
for(stat in c("wKappa")) {
  plot(refResult, metric=stat)
  varImpPlot(refResult$fit)
}
dev.off()

# show confusion matrix
refResult$fit
capture.output(print(refResult$fit), file="confusion_rfe_selected_200features_50to500_kappa_20150701.txt")

## save new data set ##
selectedFeatures = predictors(refResult)
newDataset = dataset[, names(dataset) %in% selectedFeatures]
newDataset$median_relevance = NULL
newDataset$median_relevance = dataset$median_relevance
str(newDataset)
dim(newDataset)

newsubmitTest = submitTest[, names(submitTest) %in% selectedFeatures]
str(newsubmitTest)
dim(newsubmitTest)


# combine to new csv
write.csv(newDataset, "train_RFE_200features_M+D_markpeng_20150701.csv", 
          append=FALSE, row.names=FALSE, sep=",", quote=TRUE, fileEncoding="UTF-8")
write.csv(newsubmitTest, "test_RFE_200features_M+D_markpeng_20150701.csv", 
          append=FALSE, row.names=FALSE, sep=",", quote=TRUE, fileEncoding="UTF-8")



