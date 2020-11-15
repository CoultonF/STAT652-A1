# Title     : STAT652-A1
# Objective : Minimize R2
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
#Variable selection
library(dplyr)
library(leaps)
#TREE
library(rpart)
library(rpart.plot)

#Seed default
seed = 10

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
shuffle <- function(x, seed=1){
  set.seed(seed)
  new_order = sample.int(length(x))
  new_x = x[new_order]
  return(new_x)
}
predict.matrix = function(fit.lm, X.mat){
  coeffs = fit.lm$coefficients
  Y.hat = X.mat %*% coeffs
  return(Y.hat)
}

#Variable Selection Functions
leaps.selection = function(data){
  allsub = regsubsets(x=data[,-1], y=data[,1], nbest=1,nvmax=15)
  summ.1 = summary(allsub)
  summ.1$bic
  par(mfrow=c(1,1))
  plot(allsub1, main="All Subsets on half of data data")
  n1 = nrow(data)
  results1 = matrix(data=NA, nrow=16, ncol=4)
  mod0 = lm(Y ~ 1, data=data)
  pred11 = predict(mod0, newdata=data)
  sMSE = mean((pred11-data$Y)^2)
  BIC = extractAIC(mod0, k=log(n1))
  MSPE = mean((pred2-data[data$set==2,]$Y)^2)
  results1[1,] = c(0, sMSE, BIC[2], MSPE)
  colnames(results1) = c("p", "sMSE", "BIC", "MSPE")
  data2 = data[,c(1:15)]
  head(data)
  for(v in 1:15){
    mod1 = lm(Y ~ ., data=data[data$set==1, summ.1$which[v,]])
    BIC = extractAIC(mod1, k=log(n1))
    pred1 = predict(mod1)
    sMSE = mean((pred1-data[data$set==1,]$Y)^2)
    pred2 = predict(mod1, newdata=data[data$set==2,])
    MSPE = mean((pred2-data[data$set==2,]$Y)^2)
    results1[v+1,] = c(v, sMSE, BIC[2], MSPE)
  }
  round(results1, digits=2)
  # Best size according to BIC
  results1[which.min(results1[,3]),1]
  # Best size according to MSPE
  results1[which.min(results1[,4]),1]

  # All 3 plots together
  par(mfrow=c(1,3))
  plot(x=results1[,1], y=results1[,2], xlab="Vars in model", ylab="sample-MSE",
       main="SampleMSE vs Vars: 1st", type="b")
  plot(x=results1[,1], y=results1[,3], xlab="Vars in model", ylab="BIC",
       main="BIC vs Vars: 1st", type="b")
  plot(x=results1[,1], y=results1[,4], xlab="Vars in model", ylab="MSPE",
       main="MSPE vs Vars: 1st", type="b")
#  return best data
  return(data)
}

#Modeling Functions
neural.net = function(X, Y){
  # Neural Net Tuning Model 1
  # Returns tuning params

  #Parameters
  all.n.hidden = c(1, 3, 5, 7)
  all.shrink = c(0.001, 0.1, 0.5, 1, 2, 3)
  refits = 10
  int.cv = 10 #Internal CV Fine tuning folds

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
  fit.pls = plsr(Y.train ~ ., data = cbind(Y.train, X.train), validation="CV", segments=5)
  CV.pls = fit.pls$validation
  PRESS.pls = CV.pls$PRESS
  CV.MSPE.pls = PRESS.pls / nrow(X.train)
  ind.best.pls = which.min(CV.MSPE.pls)
  pred.pls = predict(fit.pls, X.valid, ncomp = ind.best.pls)
  MSPE.pls = get.MSPE(Y.valid, pred.pls)
  return(MSPE.pls)
}
gam.model = function(X.train, Y.train, X.valid , Y.valid){
  fit.gam = gam(data=cbind(Y.train, X.train),
            formula=Y.train ~ s(X2, sp=0.1) + s(X4,sp=0.2) + s(X12, sp=0.2) + s(X15, sp=0.2),
            family=gaussian(link=identity))
  pred.gam = predict(fit.gam ,newdata=X.valid)
  MSPE.gam = get.MSPE(Y.valid, pred.gam)
  return(list(MSPE.gam, pred.gam))
}
ppr.model = function(X.train, Y.train, X.valid, Y.valid){
  #parameters
  data.train = cbind(Y.train, X.train)
  data.valid = cbind(Y.valid, X.valid)
  K.ppr = 5
  max.terms = 5
  folds.ppr = get.folds(nrow(X.train), K.ppr)
  MSPEs.ppr = array(0, dim = c(K.ppr,max.terms))
  for(j in 1:K.ppr){
    train.ppr = data.train[folds.ppr != j,]
    valid.ppr = data.train[folds.ppr == j,]
    Y.valid.ppr = valid.ppr$Y.train
    for(l in 1:max.terms){
      ### Fit model
      fit.ppr = ppr(Y.train ~ ., data = train.ppr,
        max.terms = max.terms, nterms = l, sm.method = "gcvspline")

      pred.ppr = predict(fit.ppr, valid.ppr)

      MSPE.ppr = get.MSPE(Y.valid.ppr, pred.ppr)
      MSPEs.ppr[j,l] = MSPE.ppr

    }
  }
  ave.MSPE.ppr = apply(t(MSPEs.ppr), 1, mean)
  best.terms = which.min(ave.MSPE.ppr)
  fit.ppr.best = ppr(Y.train ~ ., data = data.train, max.terms = max.terms, nterms = best.terms, sm.method = "gcvspline")
  pred.ppr.best = predict(fit.ppr.best, data.valid)
  MSPE.ppr.best = get.MSPE(Y.valid, pred.ppr.best)
  return(MSPE.ppr.best)
}
trees.model = function(X.train, Y.train, X.valid, Y.valid){
  fit.tree = rpart(Y.train ~ X2 + X4 + X12 + X15, data = cbind(Y.train, X.train), cp = 0.05)
  pred.fulltree = predict(fit.tree, newdata = cbind(Y.valid, X.valid))
  MSPE.fulltree = get.MSPE(Y.valid, pred.fulltree)
  full.tree.mspe = MSPE.fulltree
  #MIN-CV TREE
  info.tree = fit.tree$cptable
  ind.min = which.min(info.tree[,"xerror"])
  CP.min.raw = info.tree[ind.min, "CP"]
  relerr.min.raw = info.tree[ind.min, "nsplit"]

  if(ind.min == 1){
    ### If minimum CP is in row 1, store this value
    CP.min = CP.min.raw
  } else{
    ### If minimum CP is not in row 1, average this with the value from the
    ### row above it.

    ### Value from row above
    CP.above = info.tree[ind.min-1, "CP"]

    ### (Geometric) average
    CP.min = sqrt(CP.min.raw * CP.above)
  }
  fit.tree.min = prune(fit.tree, cp = CP.min)
  pred.fit.tree.min = predict(fit.tree.min, newdata = cbind(Y.valid, X.valid))
  MSPE.fit.tree.min = get.MSPE(Y.valid, pred.fit.tree.min)
  min.tree.mspe = MSPE.fit.tree.min

  #1SE TREE
  err.min = info.tree[ind.min, "xerror"]
  se.min = info.tree[ind.min, "xstd"]
  threshold = err.min + se.min
  ind.1se = min(which(info.tree[1:ind.min,"xerror"] < threshold))
  CP.1se.raw = info.tree[ind.1se, "xerror"]
  if(ind.1se == 1){
    ### If best CP is in row 1, store this value
    CP.1se = CP.1se.raw
  } else{
    ### If best CP is not in row 1, average this with the value from the
    ### row above it.

    ### Value from row above
    CP.above = info.tree[ind.1se-1, "CP"]

    ### (Geometric) average
    CP.1se = sqrt(CP.1se.raw * CP.above)
  }

  fit.tree.1se = prune(fit.tree, cp = CP.1se)
  pred.fit.tree.1se = predict(fit.tree.1se, newdata = cbind(Y.valid, X.valid))
  MSPE.fit.tree.1se = get.MSPE(Y.valid, pred.fit.tree.1se)
  Ise.tree.mspe = MSPE.fit.tree.1se
  return(c(full.tree.mspe,min.tree.mspe, Ise.tree.mspe))
}

#Load Data
data = na.omit(read.csv("Data2020.csv"))
#data = data[,c(1,3,4,5,13)]
test = na.omit(read.csv("Data2020testX.csv"))
#test = test[,c(2,4,6,12)]

#Get num rows as n
n = nrow(data)

#Split into CV folds
K=10
folds = get.folds(n, K)

#CV Comparison of Diff Models
all.models = c("LS", "STEPWISE", "RIDGE", "LASSO-MIN", "LASSO-1SE", "PLS", "GAM", "PPR", "FULL TREE", "MIN TREE", "1SE TREE")
all.MSPEs = array(0, dim = c(K,length(all.models)))
colnames(all.MSPEs) = all.models

#Get NeuralNet Fine Tuned Params
#nn.params = neural.net(data[-1],data[1])
#nn.hidden = as.integer(str_split(nn.params,',')[[1]])[1]
#nn.shrink = as.integer(str_split(nn.params,',')[[1]])[2]

# CV method
for(i in 1:K){
  X.train = data[folds != i,-1]
  X.valid = data[folds == i,-1]
  Y.train = data[folds != i,1]
  Y.valid = data[folds == i,1]
  X.test = test

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

  #GAM
  all.MSPEs[i, "GAM"] = gam.model(X.train, Y.train, X.valid, Y.valid)[[1]]

  #PLS
  all.MSPEs[i, "PLS"] = pls.model(X.train, Y.train, X.valid, Y.valid)

  #PPR
  all.MSPEs[i, "PPR"] = ppr.model(X.train, Y.train, X.valid, Y.valid)

  #NNET
  #set.seed(seed)
  #fit.nnet = nnet(X.train, Y.train, linout=T, size=nn.hidden, decay=nn.shrink, maxit=500, trace=F)
  #pred.nnet = predict(fit.nnet, X.valid)
  #MSPE.nnet = get.MSPE(Y.valid, pred.nnet)
  #all.MSPEs[i,"NNET"] = MSPE.nnet

  #TREES
  trees.mspes = trees.model(X.train, Y.train, X.valid, Y.valid)
  all.MSPEs[i,"FULL TREE"] = trees.mspes[1]
  all.MSPEs[i,"MIN TREE"] = trees.mspes[2]
  all.MSPEs[i,"1SE TREE"] = trees.mspes[3]

}
par(mfrow=c(1,1))
boxplot(all.MSPEs, main = paste0("CV MSPEs over ", K, " folds"))
all.RMSPEs = apply(all.MSPEs, 2, function(W) W/min(W))
boxplot(t(all.RMSPEs))
#Testing R2 on self
rsq <- function(x, y) summary(lm(y~x))$r.squared

Y.hat = gam.model(data[,-1], data[,1], test, test[,1])[[2]]
Y = gam.model(data[,-1], data[,1], data[,-1], data[1])[[2]]
summary(Y)
summary(data[,1])
summary(Y.hat)
rsq(data[,1], Y)

write.table(Y.hat, 'output.csv', sep = ",", row.names = F, col.names = F)