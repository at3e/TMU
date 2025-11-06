y1<-1+rnorm(10)/5
y2<-3+rnorm(10)/5
y3<-4+rnorm(10)/5
y4<-397+rnorm(10)/5
library(plotrix)
plot(y1,ylim=c(0,10),axes=FALSE,main="Big range plot",ylab="Y values")
points(y2)
points(y3)
box()
axis(2,at=c(1,2,3,4,6,7,8,9),labels=c("1","2","3","4","396","397","398","399"))
axis.break(2,5)
par(new=TRUE)
plot(y4,ylim=c(390,400),axes=FALSE,main="",ylab="",xlab="")
