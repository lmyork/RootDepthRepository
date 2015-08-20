### figure concept

plot(0:10, rev(0:10),ylim=c(10,0),xlim=c(0,6),type="n",ylab="depth",xlab="length")
a<-c(1,   2,  2.5, 1.5,  0.5,  0.25, 0.2,  0.1, 0,   0, 0)
b<-c(0.5, 1,  2,   2.5,  3,    2,    1.5,  1,   0.5, 0, 0)
c<-c(0.1, 0.1,0.2, 0.5,  1,    2,    1.5,  1,  0.5,  0, 0)

polygon(x=c(a,rep(0,11)),y=c(0:10,10:0),col="grey20")
polygon(x=c(a+b,rev(a)),y=c(0:10,10:0),col="grey50")
polygon(x=c(a+b+c,rev(a+b)),y=c(0:10,10:0),col="grey80")

legend("bottomright",legend=c("Lg","Md","Sm"),fill=c("grey20","grey50","grey80"))