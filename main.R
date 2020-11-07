# Title     : TODO
# Objective : TODO
# Created by: coultonf
# Created on: 2020-11-06
#Libraries for...
#Neural Net
library(nnet)
library(stringr)
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
all.models = c("NNET")
all.MSPEs = array(0, dim = c(K,length(all.models)))
colnames(all.MSPEs) = all.models

#Get NeuralNet Fine Tuned Params
nn.params = neural.net(data[-1],data[1])
nn.hidden = as.integer(str_split(nn.params,',')[[1]])[1]
nn.shrink = as.integer(str_split(nn.params,',')[[1]])[2]

# CV method
for(i in 1:K){
  print(paste0(i, " of ", K))
  X.train = data[folds != i,-1]
  X.valid = data[folds == i,-1]
  X.test = test
  Y.train = data[folds != i,1]
  Y.valid = data[folds == i,1]

  #Begin NeuralNet
  fit.nnet = nnet(X.train, Y.train, linout=T, size=nn.hidden, decay=nn.shrink, maxit=500, trace=F)
  pred.nnet = predict(fit.nnet, X.valid)
  MSPE.nnet = get.MSPE(Y.valid, pred.nnet)
  all.MSPEs[i,"NNET"] = MSPE.nnet
  #End of NeuralNet

  #Add models for analysis here.

}
boxplot(all.MSPEs, main = paste0("CV MSPEs over ", K, " folds"))
