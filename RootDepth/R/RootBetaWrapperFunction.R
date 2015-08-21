###split and apply beta function
##load RootsField2013.mese.csv first!


#                                       basic working wrapper in base R, needs to be made more general

aggregate(as.numeric(row.names(RootsField2013.mese)) ~ RootsField2013.mese$Water + RootsField2013.mese$Geno, FUN=function(r) RootBeta(RootsField2013.mese$Length.cm..mean[r], RootsField2013.mese$Layer2[r])) ##works!!!!!

# http://www.r-bloggers.com/how-to-write-and-debug-an-r-function/

# look above as I look into passing argumetns and such










##               Following is what a wrapper function might look like:

##rootdepth(data, c(factor1, factor2), layers, c(length, volume, mass, whatever))
depthfactors <- with(RootsField2013.mese, list(Water, Geno)) #c(factor1, factor2)
depthlayerslength <- subset(RootsField2013.mese, select= c(Layer2, Length.cm..mean)) #layers, c(length, volume, mass, whatever) for now single is OK


sapply(split(depthlayerslength, depthfactors), function(x) RootBeta(x$Length.cm..mean, x$Layer2)) #nls can throw errors when start values are poor, got one for minfactor too small... user will need to be able to pass arguments to nls to attempt to get around this

as.data.frame(sapply(split(depthlayerslength, depthfactors), function(x) RootBeta(x$Length.cm..mean, x$Layer2)))

###now just need to wrap this up in a nice function that calls on RootBeta



aggregate(cbind(slope=1:nrow(xy))~fac1+fac2,FUN=function(r) coef(lm(y~x,data=xy[r,]))[2])

aggregate(cbind(slope=1:nrow(xy))~fac1+fac2,FUN=function(r) return(r))


aggregate(cbind(slope=row.names(RootsField2013.mese)), with(RootsField2013.mese, list(Water, Geno)), FUN=function(r) RootBeta(RootsField2013.mese$Length.cm..mean[as.numeric(r)], RootsField2013.mese$Layer2[as.numeric(r)])) #giving an error

#beta function that only returns rows has wrong placement, so must not be passing them formatted correctly even though very below that simply returns rows seems fine

barleyrows <- aggregate(cbind(slope=row.names(RootsField2013.mese)) ~ RootsField2013.mese$Water + RootsField2013.mese$Geno, FUN=function(r) return(r))

aggregate(row.names(RootsField2013.mese) ~ RootsField2013.mese$Water + RootsField2013.mese$Geno, FUN=function(r) return(r))



aggregate(row.names(RootsField2013.mese) ~ RootsField2013.mese$Water + RootsField2013.mese$Geno, FUN=function(r) RootBeta(RootsField2013.mese$Length.cm..mean[as.numeric(r)], RootsField2013.mese$Layer2[as.numeric(r)])) ##works!!!!!



aggregate(cbind(slope=row.names(RootsField2013.mese)) ~ RootsField2013.mese$Water + RootsField2013.mese$Geno, FUN=function(r) RootBeta(depthlayerslength$Length.cm..mean[as.numeric(r)], depthlayerslength$Layer2[as.numeric(r)])) #giving an error



RootBeta(depthlayerslength$Length.cm..mean[as.numeric(barleyrows$slope[1,])], depthlayerslength$Layer2[as.numeric(barleyrows$slope[1,])])

RootBeta(depthlayerslength$Length.cm..mean[barleyrows$slope[1,]], depthlayerslength$Layer2[barleyrows$slope[1,]])
dd <- barleyrows$slope[5,]

RootBeta(depthlayerslength$Length.cm..mean[as.numeric(dd)], depthlayerslength$Layer2[as.numeric(dd)]) #does not give error



RootBeta=function(lengths,depths) {
  cumsumroot <- cumsum(lengths) / sum(lengths) #calculate cumulative root proportion with depth
  rootdata <- as.data.frame(cbind(depths, cumsumroot)) #bind needed data for fitting beta  
  #rootbeta.fit <- nls(cumsumroot ~ 1 - beta^depths, data=rootdata, start = list(beta = .9))
  #return(summary(rootbeta.fit)$parameters[1])
  return(depths)
} #end RootBeta function, works

RootBeta=function(lengths,depths) {
  cumsumroot <- cumsum(lengths) / sum(lengths) #calculate cumulative root proportion with depth
  rootdata <- as.data.frame(cbind(depths, cumsumroot)) #bind needed data for fitting beta  
  #rootbeta.fit <- nls(cumsumroot ~ 1 - beta^depths, data=rootdata, start = list(beta = .9))
  #return(summary(rootbeta.fit)$parameters[1])
  return(lengths)
} #end RootBeta function, works




