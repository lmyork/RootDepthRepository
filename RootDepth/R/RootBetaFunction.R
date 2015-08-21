
#####need function for calculating beta, a fitted shape parameter
# Y = 1 - beta^d
# Y = cumulative percentage of roots at that depth
# beta = calculatec coefficient
# d = depth
# could make a version using fitted models as in DX function, but think most fit formula to raw data

RootBeta=function(lengths,depths) {
  cumsumroot <- cumsum(lengths) / sum(lengths) #calculate cumulative root proportion with depth
  rootdata <- as.data.frame(cbind(depths, cumsumroot)) #bind needed data for fitting beta  
  rootbeta.fit <- nls(cumsumroot ~ 1 - beta^depths, data=rootdata, start = list(beta = .8))
  return(summary(rootbeta.fit)$parameters[1])
} #end RootBeta function, works

