x=c(1,2,3,3,1,1,1,3,3,1)
y=c(2,2,3,3,1,3,1,2,2,3)
z=c("Pass","Pass","Fail","Pass","Pass","Pass","Fail","ATKT","Pass","ATKT")
z
t=table(x,y)
margin.table(x)
margin.table(y)
prop.table(t)
prop.table(t,1)
prop.table(t,2)
t1=ftable(x,y,z)
t1
ftable(t1)
a=rnorm(10,4,4)
b=rnorm(10,4,4)
c=1+0.2*b+rnorm(1,0,1)
c
d=data.frame(a,b,c)
d
cov(d)
cov(d,method="spearman")
cov(d,method="pearson")
cor(d)
library(car)
library(corrgram)
corrgram(d)
library(corrplot)
corrplot(d)
l=lm(a~b+c)
residuals(l)
s=summary(l)
attach(s)
adj.r.squared
call
coefficients
r.squared
residuals
sigma
terms
plot(residuals,fitted(l))
fitted(l)
qnorm(l)
