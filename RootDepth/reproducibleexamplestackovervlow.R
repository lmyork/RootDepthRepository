#reproducible stack overflow question



#create data
fac1 <- c(rep("A", 10), rep("B",10))
fac2 <- rep(c(rep("X", 5), rep("Y",5)),2)
x <- rep(1:5,4)
set.seed(1337)
y <- rep(seq(2, 10, 2), 4) * runif(20, .8, 1.2)

xy <- data.frame(x,y) #bind parameters for regression

factors <- list(fac1, fac2) #split by 2 factors

sapply(split(xy, factors), function(c) coef(lm(c$y~c$x))[2]) #run regression by these 4 groups, pull out slope




#                                      best solution stack overflow - use!

aggregate(cbind(slope=1:nrow(xy))~fac1+fac2,FUN=function(r) coef(lm(y~x,data=xy[r,]))[2])




# http://www.r-bloggers.com/how-to-write-and-debug-an-r-function/

# look above as I look into passing argumetns and such



##basis for answer?

slopes <- sapply(split(xy, factors), function(c) coef(lm(c$y~c$x))[2]) #run regression by these 4 groups, pull out slope

dataframeplz <- as.data.frame(expand.grid(unique(fac1), unique(fac2))) 

dataframeplz$slope <- slopes

#plyr solution

library("plyr")
neatdata <- data.frame(fac1,fac2,x,y)
ddply(neatdata, c("fac1", "fac2"), function(c) coef(lm(c$y~c$x))[2])


#modification to help for our case

aggregate(cbind(slope=row.names(xy))~fac1+fac2,FUN=function(r) coef(lm(y~x,data=xy[r,]))[2])

#can i break it?

aggregate(cbind(slope=row.names(xy))~fac1+fac2,FUN=function(r) coef(lm(xy$y[r]~xy$x[r]))[2])

FUN=function(r) return(r)

aggregate(cbind(slope=row.names(xy))~fac1+fac2,FUN=function(r) return(r))




##someone's answer

dt_res = sapply(split(xy, factors), function(c) coef(lm(c$y~c$x))[2]) #run regression by these 4 groups, pull out slope

dt_res

# A.X.c$x  B.X.c$x  A.Y.c$x  B.Y.c$x 
# 1.861290 2.131431 1.590733 1.746169


dt_res = data.frame(dt_res)
dt_res = data.frame(names=rownames(dt_res),
                    slope=dt_res$dt_res,
                    row.names = NULL)

dt_res$names = gsub(".c[$]x","",dt_res$names)
dt_res$fac1 = substr(dt_res$names,1,1)
dt_res$fac2 = substr(dt_res$names,3,3)
dt_res[,c("fac1","fac2","slope")]

#####getting the names of stuff and stuff

fooFunc <- function( dfNameStr, colNamestr, drop=TRUE) {
  df <- get(dfNameStr)
  return(df[,colNamestr, drop=drop])
}


> myData <- data.frame(ID=1:10, variable1=rnorm(10, 10, 1))
> myData
ID variable1
1   1 10.838590
2   2  9.596791
3   3 10.158037
4   4  9.816136
5   5 10.388900
6   6 10.873294
7   7  9.178112
8   8 10.828505
9   9  9.113271
10 10 10.345151


> fooFunc('myData', 'ID', drop=F)
ID
1   1
2   2
3   3
4   4
5   5
6   6
7   7
8   8
9   9
10 10
> fooFunc('myData', 'ID', drop=T)
[1]  1  2  3  4  5  6  7  8  9 10
