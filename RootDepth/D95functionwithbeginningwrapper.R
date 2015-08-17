#from eric nord

DX=function(lengths,depths,type="linear",n=200,D=0.95){
  if(type!="linear"&type!="spline"&type!="both") stop("type must be linear, spline, or both")
  if(length(lengths)!=length(depths)) stop("lengths and depths not the same length")
  if(D<0|D>1.0) stop("D must be between 0 and 1")
  fit.l<-approx(depths,lengths,n=n)
  res.l<-fit.l$x[which(cumsum(fit.l$y/sum(fit.l$y))>=D)[1]]
  fit.s<-spline(depths,lengths,n=n)
  res.s<-fit.s$x[which(cumsum(fit.s$y/sum(fit.s$y))>=D)[1]]
  if (type=="linear"){ # for linear interpolation
    # cat("Linear estimate of D",D*100,)
    return(res.l)
  }
  if (type=="spline"){ # for linear interpolation
    return(res.s)
  }
  if (type=="both"){ # for linear interpolation
    return(c(res.l,res.s))
  }
}

##other options needed for DX function, or its wrapper above one level
#based on averaged or raw data in each depth
#how will it handle missing data?
#default to calculate both D50 and D95, those are the standards
#is there much work on using splines to fit these? can we validate which is better somehow?

#first let's apply the DX function to a test case of a single core

rootIrrHar.mese <- RootsField2013.mese[which(RootsField2013.mese$Water=="Irrigated" & RootsField2013.mese$Geno=="Harrington"),]
names(rootIrrHar.mese)

DX(rootIrrHar.mese$Length.cm..mean, rootIrrHar.mese$Layer, type="both")

#nice, works

#OK, want to use split and apply to aggregate but aggregate() itself isn't a good option since we need to pull in... then again seems expand.grid can fill in missing data...

##rootdepth(data, c(factor1, factor2), layers, c(length, volume, mass, whatever))
RootsField2013.mese$Layer2 <- RootsField2013.mese$Layer*15 #each area is 30 cm, this fixes to midpoint

RootsField2013.mese$Layer2[which(RootsField2013.mese$Layer== 1)] <- 15

RootsField2013.mese$Layer2[which(RootsField2013.mese$Layer== 2)] <- 45

RootsField2013.mese$Layer2[which(RootsField2013.mese$Layer== 3)] <- 75
?identi
write.csv(RootsField2013.mese, "RootsField2013layer2.csv")



depthfactors <- with(RootsField2013.mese, list(Water, Geno)) #c(factor1, factor2)
depthlayerslength <- subset(RootsField2013.mese, select= c(Layer2, Length.cm..mean)) #layers, c(length, volume, mass, whatever) for now single is OK


sapply(split(depthlayerslength, depthfactors), function(x) DX(x$Length.cm..mean, x$Layer2, type="both", D=.5)) 
sapply(split(depthlayerslength, depthfactors), function(x) DX(x$Length.cm..mean, x$Layer2, type="both", D=.95)) 
sapply(split(depthlayerslength, depthfactors), function(x) DX(x$Length.cm..mean, x$Layer2, type="linear", D=.95)) 




#above I give the final solution, below is where I figured that out - now it just needs to be a bit more generic in the master call

##

barleyD95 <- sapply(split(depthlayerslength, depthfactors), function(x) DX(x$Length.cm..mean, x$Layer2, type="linear", D=.95)) 
barleyD50 <- sapply(split(depthlayerslength, depthfactors), function(x) DX(x$Length.cm..mean, x$Layer2, type="linear", D=.5)) 
plot(barleyD95 ~ barleyD50)
summary(lm(as.matrix(barleyD95)[,1] ~ as.matrix(barleyD50)[,1]))
abline(lm(as.matrix(barleyD95)[,1] ~ as.matrix(barleyD50)[,1]))

is.vector(barleyD95)

D95barleyboth <- sapply(split(depthlayerslength, depthfactors), function(x) DX(x$Length.cm..mean, 15*x$Layer, type="both", D=.5)) 

as.matrix(D95barleyboth)[1,]
colnames(D95barleyboth)

sapply(split(depthlayerslength, depthfactors), function(x) DX(x$Length.cm..mean, 15*x$Layer, type="both", D=.5)) 

t(sapply(split(depthlayerslength, depthfactors), function(x) DX(x$Length.cm..mean, 15*x$Layer, type="both", D=.5)))

cbind(as.vector(depthfactors[1]), as.vector(depthfactors[2]),sapply(split(depthlayerslength, depthfactors), function(x) DX(x$Length.cm..mean, 15*x$Layer, type="both", D=.5)))


#this returns a list, but we would actually want to return a data frame. the list combines the factors separated by a period but we need them separated as columns

####scratch sheet below

iris.x <- subset(iris, select= -Species)
iris.s <- subset(iris, select= Species)

?subset

#i think this is the most general solution that will be useful for us

sapply(split(iris.x, iris.s), function(x) apply(x, 2, mean)) #surely it can't be this easy???????

depthfactors <- with(RootsField2013.mese, list(Water, Geno))#stolen from my means code, should return the list of factors used for interactions for gruops used by split

sapply(split(RootsField2013.mese, depthfactors))

#in master function, needs a c() for factors to use, a slot for layers, and a single or c() for lengths, volumes, mass or whatever, combine all those for subset, then use those after to define factors etc

depthdata <- subset(RootsField2013.mese, select= c(Water, Geno, Layer, Length.cm..mean))
depthdata2 <- subset(RootsField2013.mese, select= c(Layer, Length.cm..mean))


#upstream of this we need to subset to only the factors needed, the length or volume or mass data used, and the depth data

depthdata2 <- subset(RootsField2013.mese, select= c(Layer, Length.cm..mean))
depthfactors <- with(RootsField2013.mese, list(Water, Geno))


split(depthdata2, depthfactors)

sapply(split(depthdata2, depthfactors), function(x) apply(x, 2, )) 

depthdata2 <- subset(RootsField2013.mese, select= c(Layer, Length.cm..mean))
depthfactors <- with(RootsField2013.mese, list(Water, Geno))

sapply(split(depthdata2, depthfactors), function(x) DX(x$Length.cm..mean, x$Layer, type="both")) 







?split

aggregate