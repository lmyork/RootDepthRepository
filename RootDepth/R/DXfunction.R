#Function for calculating depth at which X percent roots are at or above

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