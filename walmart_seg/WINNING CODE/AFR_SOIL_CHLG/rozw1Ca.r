library(wavelets)
library(kernlab)

MSE = function(p, t) {
  mean((p - t)**2)
}

all_trainingdata <- read.csv("../training.csv")
all_testdata <- read.csv("../sorted_test.csv")

soil_property <- "Ca"
sp = c(1100:1500,1700:1850,2150:3200)

const = 0.536

sq = function(x) {
  x * x
}

fun = function(x) {
  sqrt(x + const)
}

inv_fun = function(x) {
  sq(x) - const
}

foo = function(x) { 
  w=dwt(as.numeric(x), filter="la12") 
  w@V[[4]]
}

X_train = all_trainingdata[,sp]
Z_train = data.frame(t(apply(X_train, 1, foo)))

train = Z_train
expected = all_trainingdata[,soil_property]
train[,soil_property] = fun(all_trainingdata[,soil_property])

model = gausspr(as.formula(paste(soil_property, " ~ .")), train, scaled=FALSE, var=0.01, kpar=list(sigma=0.01))

train_prediction = inv_fun(predict(model, train))
train_error = sqrt(MSE(train_prediction, expected))

X_test = all_testdata[,sp]
Z_test = data.frame(t(apply(X_test, 1, foo)))

res = read.csv("../sample_submission.csv")
res$Ca = inv_fun(predict(model, Z_test))

write.csv(res, "1stsolCa.csv", quote=FALSE, row.names=FALSE)

