# Title     : TODO
# Objective : TODO
# Created by: coultonf
# Created on: 2020-11-06
#Libraries for...
#Neural Net
library(nnet)
library(stringr)
#Ridge Lasso
library(glmnet)
library(MASS)
#PLS
library(pls)

#Seed default
seed = 1

#Helper functions
get.folds = function(n, K) {
  set.seed(seed)
  n.fold = ceiling(n / K)
  fold.ids.raw = rep(1:K, times = n.fold)
  fold.ids = fold.ids.raw[1:n]
  folds.rand = fold.ids[sample.int(n)]
  return(folds.rand)
}
get.MSPE = function(Y, Y.hat){
  return(mean((Y - Y.hat)^2))
}
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

#Modeling Functions
neural.net = function(X, Y){
  # Neural Net Tuning Model 1
  # Returns tuning params

  #Parameters
  all.n.hidden = c(1, 3, 5, 7, 9)
  all.shrink = c(0.001, 0.1, 0.5, 1, 2)
  refits = 5
  int.cv = 5 #Internal CV Fine tuning folds

  #Variables to iterate
  all.pars = expand.grid(n.hidden = all.n.hidden, shrink = all.shrink)
  n.pars = nrow(all.pars)
  CV.MSPEs.NN = array(0, dim = c(int.cv, n.pars))
  names.pars = paste0(all.pars$n.hidden,",", all.pars$shrink)
  colnames(CV.MSPEs.NN) = names.pars

  #Internal CV Folds
  int.n = nrow(data)
  int.folds = get.folds(int.n, int.cv)

  #Iterate through internal CV folds
  for(c in 1:int.cv){
    print(paste0(c, " of ", int.cv))
    X.train.int = X[folds != c,]
    X.valid.int = X[folds == c,]
    Y.train.int = Y[folds != c,]
    Y.valid.int = Y[folds == c,]
    X.train.scaled = rescale(X.train.int, X.train.int)
    X.valid.scaled = rescale(X.valid.int, X.train.int)

    #Iterate through parameter combinations
    for(j in 1:n.pars){
        size = all.pars[j,1]
        decay = all.pars[j,2]
        all.nnets = list(1:refits)
        all.SSEs = rep(0, times = refits)

        #Iterate through parameter refits of single type
        for(r in 1:refits){
          fit.nnet = nnet(X.train.int, Y.train.int, linout=T, size=size, decay=decay, maxit=500, trace=F)
          SSE.nnet = fit.nnet$value
          all.nnets[[r]] = fit.nnet
          all.SSEs[r] = SSE.nnet
        }
        ind.best = which.min(all.SSEs)
        fit.nnet.best = all.nnets[[ind.best]]
        pred.nnet = predict(fit.nnet.best, X.valid.int)
        MSPE.nnet = get.MSPE(Y.valid.int, pred.nnet)
        CV.MSPEs.NN[c,j] = MSPE.nnet
    }
  }
  NNet.MSPEs = colMeans(CV.MSPEs.NN)
  inds.best = which.min(NNet.MSPEs)
  return(names.pars[inds.best])
}
ls.model = function(X.train, Y.train, X.valid, Y.valid){
  fit.ls = lm(Y.train ~ ., data=cbind(Y.train, X.train))
  pred.ls = predict(fit.ls, newdata = X.valid)
  MSPE.ls = get.MSPE(Y.valid, pred.ls)
  return(MSPE.ls)
}
stepwise.model = function(X.train, Y.train, X.valid, Y.valid){
  fit.start = lm(Y.train ~ 1, data = cbind(Y.train, X.train))
  fit.end = lm(Y.train ~ ., data = cbind(Y.train, X.train))
  step.def = step(fit.start, list(upper = fit.end), trace=0)
  pred.step.def = predict(step.def, X.valid)
  err.step.def = get.MSPE(Y.valid, pred.step.def)
  return(err.step.def)
}
ridge.model = function(X.train, Y.train, X.valid, Y.valid){
  lambda.vals = seq(from = 0, to = 100, by = 0.05)
  fit.ridge = lm.ridge(Y.train ~ ., lambda = lambda.vals, data = cbind(Y.train, X.train))
  ind.min.GCV = which.min(fit.ridge$GCV)
  lambda.min = lambda.vals[ind.min.GCV]
  all.coefs.ridge = coef(fit.ridge)
  coef.min = all.coefs.ridge[ind.min.GCV,]
  matrix.valid.ridge = model.matrix(Y.valid ~ ., data = cbind(Y.valid, X.valid))
  pred.ridge = matrix.valid.ridge %*% coef.min
  MSPE.ridge = get.MSPE(Y.valid, pred.ridge)
  return(MSPE.ridge)
}
lasso.model = function(X.train, Y.train, X.valid, Y.valid){
  matrix.train.raw = model.matrix(Y.train ~ ., data = cbind(Y.train, X.train))
  matrix.train = matrix.train.raw[,-1]
  all.LASSOs = cv.glmnet(x = matrix.train, y = Y.train)
  lambda.min = all.LASSOs$lambda.min
  lambda.1se = all.LASSOs$lambda.1se
  #Think we should apply lasso coef into neural model at some point.
  coef.LASSO.min = predict(all.LASSOs, s = lambda.min, type = "coef")
  coef.LASSO.1se = predict(all.LASSOs, s = lambda.1se, type = "coef")
  included.LASSO.min = predict(all.LASSOs, s = lambda.min, type = "nonzero")
  included.LASSO.1se = predict(all.LASSOs, s = lambda.1se, type = "nonzero")
  matrix.valid.LASSO.raw = model.matrix(Y.valid ~ ., data = cbind(Y.valid, X.valid))
  matrix.valid.LASSO = matrix.valid.LASSO.raw[,-1]
  pred.LASSO.min = predict(all.LASSOs, newx = matrix.valid.LASSO, s = lambda.min, type = "response")
  pred.LASSO.1se = predict(all.LASSOs, newx = matrix.valid.LASSO, s = lambda.1se, type = "response")
  MSPE.LASSO.min = get.MSPE(Y.valid, pred.LASSO.min)
  MSPE.LASSO.1se = get.MSPE(Y.valid, pred.LASSO.1se)
  return(c(MSPE.LASSO.1se, MSPE.LASSO.min))
}
pls.model = function(X.train, Y.train, X.valid, Y.valid){
  fit.pls = plsr(Y.train ~ ., data = cbind(Y.train, X.train), validation="CV", segments=10)
  CV.pls = fit.pls$validation # All the CV information
  PRESS.pls = CV.pls$PRESS    # Sum of squared CV residuals
  CV.MSPE.pls = PRESS.pls / nrow(X.train)  # MSPE for internal CV
  ind.best.pls = which.min(CV.MSPE.pls) # Optimal number of components
  pred.pls = predict(fit.pls, X.valid, ncomp = ind.best.pls)
  MSPE.pls = get.MSPE(Y.valid, pred.pls)
  return(MSPE.pls)
}
#Load Data
data = na.omit(read.csv("Data2020.csv"))
test = na.omit(read.csv("Data2020testX.csv"))

#Get num rows as n
n = nrow(data)

#Split into CV folds
K=10
folds = get.folds(n, K)

#CV Comparison of Diff Models
#As we add more models to our analysis the MSPEs for CVs will be added to all.models as a category
all.models = c("LS", "STEPWISE", "RIDGE", "LASSO-MIN", "LASSO-1SE", "PLS", "NNET")
all.MSPEs = array(0, dim = c(K,length(all.models)))
colnames(all.MSPEs) = all.models

#Get NeuralNet Fine Tuned Params
nn.params = neural.net(data[-1],data[1])
nn.hidden = as.integer(str_split(nn.params,',')[[1]])[1]
nn.shrink = as.integer(str_split(nn.params,',')[[1]])[2]

# CV method
for(i in 1:K){
  data.train = data[folds != i,]
  data.valid = data[folds != i,]
  X.train = data[folds != i,-1]
  X.valid = data[folds == i,-1]
  X.test = test
  Y.train = data[folds != i,1]
  Y.valid = data[folds == i,1]

  #LS
  all.MSPEs[i, "LS"] = ls.model(X.train, Y.train, X.valid, Y.valid)

  #Stepwise
  all.MSPEs[i, "STEPWISE"] = stepwise.model(X.train, Y.train, X.valid, Y.valid)

  #Ridge
  all.MSPEs[i, "RIDGE"] = ridge.model(X.train, Y.train, X.valid, Y.valid)

  #Lasso
  mspes.lasso = lasso.model(X.train, Y.train, X.valid, Y.valid)
  all.MSPEs[i, "LASSO-1SE"] = mspes.lasso[1]
  all.MSPEs[i, "LASSO-MIN"] = mspes.lasso[2]

  #PLS
  all.MSPEs[i, "PLS"] = pls.model(X.train, Y.train, X.valid, Y.valid)

  #NeuralNet
  set.seed(seed)
  fit.nnet = nnet(X.train, Y.train, linout=T, size=nn.hidden, decay=nn.shrink, maxit=500, trace=F)
  pred.nnet = predict(fit.nnet, X.valid)
  MSPE.nnet = get.MSPE(Y.valid, pred.nnet)
  all.MSPEs[i,"NNET"] = MSPE.nnet

  #Add models for analysis here.

}
boxplot(all.MSPEs, main = paste0("CV MSPEs over ", K, " folds"))
