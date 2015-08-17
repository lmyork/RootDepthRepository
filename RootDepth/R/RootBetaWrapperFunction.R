###split and apply beta function
##load RootsField2013.mese.csv first!
##               Following is what a wrapper function might look like:

##rootdepth(data, c(factor1, factor2), layers, c(length, volume, mass, whatever))
depthfactors <- with(RootsField2013.mese, list(Water, Geno)) #c(factor1, factor2)
depthlayerslength <- subset(RootsField2013.mese, select= c(Layer2, Length.cm..mean)) #layers, c(length, volume, mass, whatever) for now single is OK


sapply(split(depthlayerslength, depthfactors), function(x) RootBeta(x$Length.cm..mean, x$Layer2)) #nls can throw errors when start values are poor, got one for minfactor too small... user will need to be able to pass arguments to nls to attempt to get around this

###now just need to wrap this up in a nice function that calls on RootBeta