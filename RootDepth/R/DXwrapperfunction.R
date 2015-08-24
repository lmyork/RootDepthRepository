#first let's apply the DX function to a test case of a single core
##load RootsField2013.mese.csv first!

rootIrrHar.mese <- RootsField2013.mese[which(RootsField2013.mese$Water=="Irrigated" & RootsField2013.mese$Geno=="Harrington"),]
names(rootIrrHar.mese)

#DX for single case

DX(rootIrrHar.mese$Length.cm..mean, rootIrrHar.mese$Layer, type="both")

#wrapper function but only takes defaults now, need to update to include other arguments



rootdx.ag(data=RootsField2013.mese, depths="Layer2", roots="Length.cm..mean", splitby=c("Geno", "Water"))

rootdx.ag(data=RootsField2013.mese, depths="Layer2", roots="Length.cm..mean", splitby="Geno") 


rootdx.ag <- function(data, depths, roots, splitby) {
  if (length(splitby) == 1) {
    byfactors <- data[splitby]
    
  } #if we are only aggregating by 1 factor, aggregate by arg different
  
  if (length(splitby) > 1) {
    byfactors <- as.list(data[, splitby])
    
  } #if we are only aggregating by 2 or more factors, aggregate by arg different
  
  rows <- as.numeric(row.names(data)) #get the row names to index by base on splitting
  
  DXf <- aggregate(rows, by=byfactors, FUN=function(r) DX(data[r, roots], data[r, depths]))
  #betas, apply the RootBeta function based on subsetting data frame based on indexes split above
  
  names(DXf) <- c(splitby)
  
  return(DXf)
  
} #end rootdx.ag functi







#nice, works

##               Following is what a wrapper function might look like:

##rootdepth(data, c(factor1, factor2), layers, c(length, volume, mass, whatever))
depthfactors <- with(RootsField2013.mese, list(Water, Geno)) #c(factor1, factor2)
depthlayerslength <- subset(RootsField2013.mese, select= c(Layer2, Length.cm..mean)) #layers, c(length, volume, mass, whatever) for now single is OK


sapply(split(depthlayerslength, depthfactors), function(x) DX(x$Length.cm..mean, x$Layer2, type="both", D=.5)) 
sapply(split(depthlayerslength, depthfactors), function(x) DX(x$Length.cm..mean, x$Layer2, type="both", D=.95)) 
sapply(split(depthlayerslength, depthfactors), function(x) DX(x$Length.cm..mean, x$Layer2, type="linear", D=.95)) 