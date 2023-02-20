a=matrix(c(2,1,4,3,2,1,1,2,3),ncol=3,byrow=TRUE)
a
b=c(16,10,16)
b
solve(a,b)
c=matrix(c(2,6,0,6,20,-6,0,6,-18),byrow=TRUE,ncol=3)
c
d=c(-11,-3,-1)
d
solve(c,d)
det(c)
a1=c(0.98,0.05,0.47,0.4,0.35,0.78,0.86,0.25,0.11,0.41)
a1
b1=c(1.049,1.0025,1.0237,1.0202,1.0173,1.0392,1.043,1.0123,1.0056,1.0206)
b1
c1=c(0.51,0.9749,0.7634,0.7985,0.8272,0.6082,0.5695,0.8771,0.9438,0.7936)
d=data.frame(a1,b1,c1)
hist(a)
hist(a,main="Histogram",xlab="A",ylab="Freq",col="GREEN")
boxplot(b,main="Boxplot",xlab="B",ylab="Frequency of B",col="PINK")
plot(a,b,ylim=range(0,max(b)),type="l",pch=4,lwd=2,col="BLUE")
lines(a,c)
qqplot(a,c,main="Scatterplot",ylab="C",xlab="A")
dist=c("Satara","Sangli","Kolhapur","Other")

dist
x1=c(13,7,8,2)
x2=c(12,8,7,3)
barplot(x1,ylab="Student",names.arg=dist,main="Barplot")
pie(x1,main="Pie chart",col=1:4,labels=dist)
legend(locator(1),legend=dist,fill=1:4)
d=data.frame(x1,x2)
d
attach(d)
barplot(t(d),beside=TRUE,main="Multiple bar plot",xlab="Years",ylab="No of students",legend=c('2012','2013'),col=c("Green","Pink"))
barplot(t(d),main="Subdivided barplot",xlab="Years",ylab="No of students",col=c("Green","Yellow"),legend=c('2012','2013'))

