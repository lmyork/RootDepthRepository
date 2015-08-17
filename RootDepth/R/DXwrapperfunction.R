#first let's apply the DX function to a test case of a single core
##load RootsField2013.mese.csv first!

rootIrrHar.mese <- RootsField2013.mese[which(RootsField2013.mese$Water=="Irrigated" & RootsField2013.mese$Geno=="Harrington"),]
names(rootIrrHar.mese)

DX(rootIrrHar.mese$Length.cm..mean, rootIrrHar.mese$Layer, type="both")

#nice, works

##               Following is what a wrapper function might look like:

##rootdepth(data, c(factor1, factor2), layers, c(length, volume, mass, whatever))
depthfactors <- with(RootsField2013.mese, list(Water, Geno)) #c(factor1, factor2)
depthlayerslength <- subset(RootsField2013.mese, select= c(Layer2, Length.cm..mean)) #layers, c(length, volume, mass, whatever) for now single is OK


sapply(split(depthlayerslength, depthfactors), function(x) DX(x$Length.cm..mean, x$Layer2, type="both", D=.5)) 
sapply(split(depthlayerslength, depthfactors), function(x) DX(x$Length.cm..mean, x$Layer2, type="both", D=.95)) 
sapply(split(depthlayerslength, depthfactors), function(x) DX(x$Length.cm..mean, x$Layer2, type="linear", D=.95)) 