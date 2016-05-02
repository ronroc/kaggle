
cols <- c("pred1", "pred2", "pred3", ..., "predn")
library("Metrics")
fn.opt.pred <- function(pars, data) {
pars.m <- matrix(rep(pars,each=nrow(data)),nrow=nrow(data))
rowSums(data*pars.m)
}
fn.opt <- function(pars) {
-auc(train$ACTION, fn.opt.pred(pars, train[,cols]))
}
pars <- rep(1/length(cols),length(cols))
opt.result <- optim(pars, fn.opt, control = list(trace = T))

train.pred <- fn.opt.pred(opt.result$par, train[,cols])

test.pred <- fn.opt.pred(opt.result$par, test[,cols])
