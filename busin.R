
library(readxl)
df=BicycleRidership
df=as.data.frame(df[,!apply(is.na(df),2,all)])
df
head(df)
tsv=ts(df$Riders,start = c(2004,1),end = c(2017,3),frequency = 12)
plot(tsv,xlab="Year",ylab="Riders",las=2)
at1=seq(as.Date("2004-01-01"),as.Date("2017-03-01"),by="2 years")
at1
labels1=format(at1,"%b-%Y")
at2=format(at1,"%Y")
par()$mar
par(mar=c(8,4,4,2)+0.1)
plot(tsv,xlab="",ylab="",xaxt="n",yaxt="n")
axis(1,at=at2,labels=labels1,las=2)
axis(2,las=2)
mtext(side=1,text="Month-Year",line=5.5)
mtext(side=2,text="Riders",line=3.0)
graphics.off()

par()$mar

#Used Cars
df1=UsedCars
df1=as.data.frame(df)
df1=df1[,!apply(is.na(df1),2,all)]

colnames(df1)
Age=2017-df1$Mfg_Year
df1=cbind(df1,Age)
head(df1)

df1=df1[,-c(1,2,3)]
head(df1)
str(df1)
df1$Transmission=as.factor(df1$Transmission)
df1$C_Price=as.factor(df1$C_Price)
str(df1)
summary(df1)

#scatterplot

range(df1$KM)
range(df1$Price)
plot(df1$KM,df1$Price,xlim=c(18,180),ylim=c(1,75),xlab="KM",ylab="Price")

df1[df1$Price>70,]
dfb=df1
df1=df1[-23,]

range(df1$KM)
range(df1$Price)
plot(df1$KM,df1$Price,xlim=c(18,180),ylim=c(1,15),xlab = "KM",ylab="Price")

#Bar chart
avgPrice=c(mean(df1[which(df1$Transmission=='0'),]$Price),
           mean(df1[which(df1$Transmission=='1'),]$Price))

#transmission 0 asel tr tyanchy price cha mean
#s=by(df1$Price,df1$Transmission,mean)

Trans=c("0","1")
range(avgPrice)
barplot(avgPrice,names.arg = Trans,xlab = "Transmission",ylab="Average Price",
        ylim=c(0,6))
#frequency
par(mfrow=c(1,1))
pall=c((length(which(df1$Transmission=='0'))/length(df1$Transmission))*100,
       (length(which(df1$Transmission=='1'))/length(df1$Transmission))*100)
pall
barplot(pall,names.arg = Trans,xlab="trann")
#histogram
range(df1$Price)
hist(df1$Price,main=" ",xlim=c(-5,20),ylim=c(0,50),xlab="medv")
#boxplot
range(df1$Price)
boxplot(df1$Price~df1$Transmission,ylim=c(0,50),xlab="trans",ylab="price")
means=by(df1$Price,df1$Transmission,mean)#mean point by transmission
points(1:2,means,pch=3)

range(df1$KM)
boxplot(df1$KM~df1$C_Price,ylim=c(25,180),ylab="km",xlab="c_price")
means=by(df1$KM,df1$C_Price,mean)
points(1:2,means,pch=2)

range(df1$Age)
boxplot(df1$Age~df1$C_Price,ylim=c(0,12),xlab="c_price")
means=by(df1$Age,df1$C_Price,mean)
points(1:2,means,pch=3)
#heatmap
#correlation matrix
m=cor(df1[,-c(1,5,8)])
  colnames(df1)
symnum(m)  
m[upper.tri(m)]=NA;m
#correlational table heatmap
heatmap(m,Rowv=NA,symm=T,col=grey.colors(1000,start=0.8,end=0.2),
        scale = "none",margins = c(8,4))

#data matrix
#missing value heatmap for first 6 records

heatmap(head(as.matrix(df1[,-c(1,5,8)])),Rowv=NA,Colv = NA,col=grey.colors(1000,start = 0.8,end=0)
      ,scale="column",margins = c(8,4))

#multidimesional visualization
palette()
palette(rainbow(6))
palette()
palette("default")
palette()
range(df1$Age)
plot(df1$Age,df1$KM,xlim=c(0,12),xlab="Age",ylab="KM",
     col=df1$C_Price)

#separate panel
age_groups=levels(as.factor(df1$Age))
age_groups
age_groups2=as.numeric(age_groups)
age_groups2
avgPrice1=NULL
avgPrice2=NULL
for(x in age_groups2){
  avgPrice1=c(avgPrice1,mean(df1[which(df1$Age==x&df1$Transmission==0),]$Price))
  avgPrice2=c(avgPrice2,mean(df1[which(df1$Age==x&df1$Transmission==1),]$Price))
}

avgPrice1


avgPrice1[which(avgPrice1=='NaN')]=0
avgPrice2[which(avgPrice2=='NaN')]=0
par(mfrow=c(2,1),cex=0.6,mar=c(3,3,0,0),oma=c(1,1,1,1))
range(avgPrice1)
range(avgPrice2)
barplot(avgPrice1,names.arg = age_groups,xlab="",ylab="",xaxt="n",ylim=c(c(0,9)))
box("plot")
legend("topright",inset = 0.005,c("Trans=0"),bty="n",cex=1)

mtext("Avg(price)",side=2,line=2.2,cex=0.7,adj=0)

barplot(avgPrice2,names.arg = age_groups,xlab="",ylab="",ylim=c(0,9))
box("plot")
mtext("Age",side=1,line=2.2,cex=0.7)
legend("topright",inset = 0.005,c("Trans=1"),bty="n",cex=1)

#scatterplot
pairs(~SR_Price+KM+Price+Age,data = df1)
#rescaling to overcome crowding
par(mfrow=c(2,2),cex=0.6,mar=c(3,3,0,0),oma=c(1,1,1,1))

range(df1$KM)
range(df1$Price)
plot(df1$KM,df1$Price,xlim=c(0,180),ylim=c(0,15),xlab="",ylab="")
mtext("KM",side=1,line=2,cex=0.6)
mtext("Price",side=2,line=2,cex=0.6)
#log scale

plot(df1$KM,df1$Price,log="xy",xlim=c(10,1000),ylim=c(0.1,100),xaxt="n",yaxt="n"
     ,ylab="",xlab="")
axis(1,at=c(10,100,1000),labels=c("10","100","1000"))
axis(2,at=c(0.1,1,10,100),labels=c("0.1","1","10","100"))
mtext("KM",side=1,line=2,cex=0.6)
mtext("Price",side=2,line=2,cex=0.6)

range(df$Price)
boxplot(df$Price~df$Transmission,ylim=c(0,75),xlab = "",ylab = "")
mtext("trans",side = 1,line=2,cex=0.6)
mtext("Price",side=2,line = 2,cex=0.6)

#log scale
boxplot(df$Price~df$Transmission,log="y",ylim=c(0.1,100),xlab = "",ylab="")
mtext("trans",side=1,line=2,cex=0.6)
mtext("KM",side = 2,line=2,cex = 0.6)

#aggregations attaching a curve zooming in


par(mfrow=c(2,2),cex=0.6,mar=c(2.7,2.5,1.0,0.5),oma=c(0,0,0,0))

plot(tsv,xaxt="n",yaxt="n")
axis(1,at=at2,labels=format(at1,"%d/%m/%Y"),cex.axis=0.8)
axis(2,cex.axis=0.8)
mtext(side=1,text="Month",line=2,cex=0.6)
mtext(side=2,text="Riders",line=1.7,cex=0.6)
title(main="overlaying a quadratic curve on Raw series",adj=0,cex.main=0.9)

lines(lowess(tsv),col="red")

t=seq(1,length(df$Month),by=1)
t
tsq=t*t
points(time(tsv),predict(lm(df$Riders~t+tsq)),col="green")
ridebye=NULL
for(i in 1:12){
  ridebye[i]=0
}

i=1##month counter total no of months=12*12+3
while(i<=145){
  ridebye[1]=ridebye[1]+df$Riders[i]
  ridebye[2]=ridebye[2]+df$Riders[i+1]
  ridebye[3]=ridebye[3]+df$Riders[i+2]
  ridebye[4]=ridebye[4]+df$Riders[i+3]
  ridebye[5]=ridebye[5]+df$Riders[i+4]
  ridebye[6]=ridebye[6]+df$Riders[i+5]
  ridebye[7]=ridebye[7]+df$Riders[i+6]
  ridebye[8]=ridebye[8]+df$Riders[i+7]
  ridebye[9]=ridebye[9]+df$Riders[i+8]
  ridebye[10]=ridebye[10]+df$Riders[i+9]
  ridebye[11]=ridebye[11]+df$Riders[i+10]
  ridebye[12]=ridebye[12]+df$Riders[i+11]
  i=i+12
}
ridebye[1]=ridebye[1]+df$Riders[i]
ridebye[2]=ridebye[2]+df$Riders[i+1]
ridebye[3]=ridebye[3]+df$Riders[i+2]

ridebye
avgbym=c(ridebye[1]/14,ridebye[2]/14,ridebye[3]/14,ridebye[4]/13,
         ridebye[5]/13,ridebye[6]/13,ridebye[7]/13,ridebye[8]/13,ridebye[9]/13
         ,ridebye[10]/13,ridebye[11]/13,ridebye[12]/13)
tsv1=ts(avgbym,start=1,end=12,frequency = 1)
plot(tsv1)
at3=seq(as.Date("1Jan","%d%b"),as.Date("1dec","%d%b"),by="1 month")
at4=seq(as.Date("1","%d"),as.Date("12","%d"),by="1 day")
axis(1,at=format(at4,"%d",labels=format(at3,"%b",las=3
                                        ,cex.axis=0.8)))
axis(2,cex.axis=0.8)
mtext("Month",side=1,line=1.7,cex=0.6)
mtext("riders",side=2,line=1.7,cex=0.6)
title("aggregation by month",adj=0,cex.main=0.9)

abline(v=format(at4,"%d"),h=axTicks(2),col="gray",lty=3)
#plot 3

tsv2=window(tsv,start=c(2004,1),end=c(2005,12))
plot(tsv2,xaxt="n",yaxt="n")
i=1
at5=NULL
while(i<=24){
  at5=c(at5,time(tsv2)[i])
  i=i+4
}
at5
at6=seq(as.Date('2004-01-01'),as.Date('2005-12-01'),by='4 months')
axis(1,at=at5,labels = format(at6,"%d/%m/%Y"),cex.axis=0.8)
axis(2,cex.axis=0.8)
mtext(side=1,text="Month",line=1.7,cex=0.8)
mtext(side=2,text = "riders",line=1.7)
title(main='zooming into first two years',adj=0,cex.main=0.9)
abline(v=at5,h=axTicks(2),col='grey',lty=3)

#plot4
plot(aggregate(tsv,FUN = mean),xaxt="n",yaxt="n",cex.axis=0.6)
axis(1,cex.axis=0.8)
axis(2,cex.axis=0.8)
mtext(side=1,text="year",line=1.7,cex = 0.6)
mtext(side=2,text="riders",adj = 0,cex.main=0.9)
title(main='aggregation by year')
grid()



#scatterplot with labeled points
df[df$Price>70,]
df[df$Price>12,]
df[df$KM>150,]
df=df[-c(13,23,29,65,73),]
range(df$KM)
range(df$Price)
plot(df$KM,df$Price,xlim=c(25,120),ylim = c(1,9),xlab="KM"
     ,ylab="Price",panel.first = grid())
text(df$KM,df$Price,df$Model,adj=c(-0.4,-0.4),cex=0.5)

#Large datasets
#scaling up
#promoffers.xlsx
df=data.frame(Promoffers)
df3=df[,!apply(is.na(df),2,all)]
palette()
palette(c("grey","black"))
palette()
#transparent coloring & smaller markers
range(df3$Income)
range(df3$Spending)
plot(df3$Income,df3$Spending,xlim=c(0,225),ylim=c(0,11),
     xlab="Income",ylab="Spending",col=as.factor(df3$Promoffer),pch=19
     ,cex=0.8,panel.first=grid())
#jittering
plot(jitter(df3$Income,1),df3$Spending,xlim=c(0,225),ylim=c(0,11),
     ylab="Spending",xlab="Income",cex=0.8,col=as.factor(df3$Promoffer),pch=20,
     panel.first = grid())

palette("default")

#multivariate plot
#parallel coordinates plot
library(MASS)
par(mfrow=c(2,1),cex=0.6,mar=c(3,3,0,0),oma=c(1,1,1,1))
df4=df1
levels(df4$Fuel_type)=1:length(levels(df4$Fuel_type))#to make numeric factor data
df4=as.data.frame(lapply(df4,FUN = as.numeric))
parcoord(df4[which(df4$C_Price=='1'),-c(4,8)],col="grey")

#network data network graph
#two mode or bipartite graphs
#examples of association

item1=sample(LETTERS[1:10],size=50,replace = T)
pool=letters[1:10]
item2=NULL
for(i in 1:50){
  item2=c(item2,sample(pool[-which(pool==tolower(item1[i]))],
                       size=1,replace=T))
}#to remove pool==item1 letters

df5=data.frame(item1,item2)
df5
library(igraph)
g=graph_from_data_frame(df5,directed=F)
V(g)$label=V(g)$name
V(g)[1:10]$type=1
V[11:20]$type=2
V(g)$x=c(runif(10,0,5),runif(10,10,15))
V(g)$shape='circle'

E(g)$color='grey'
E(g)$color="black"  
E(g)$weight=count.multiple(g)  
g1=simplify(g,remove.multiple = T)
E(g1)$width=E(g1)$weight
size=NULL
for(i in V(g)$name){
  size=c(size,length(E(g)[from(V(g)[i])]))
}
V(g)$size=size+10
par(mar=rep(0.1,4))
plot(g)
