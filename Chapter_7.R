library(quantmod)
library(xts)
# Large-Cap Equity ETF Data
data.SPY<-read.csv("SPY Yahoo.csv",header=TRUE)
data.SPY<-data.SPY[,c(1,2,3,4,5,7,6)]
date<-as.Date(data.SPY$Date,format="%Y-%m-%d")
data.SPY<-cbind(date, data.SPY[,-1])
data.SPY<-data.SPY[order(data.SPY$date),]
library(xts)
data.SPY<-xts(data.SPY[,2:7],order.by=data.SPY[,1])
names(data.SPY)<-paste(c("SPY.Open","SPY.High","SPY.Low","SPY.Close","SPY.Volume","SPY.Adjusted"))
library(quantmod)
SPY.monthly<-to.monthly(data.SPY)
SPY.monthly<-SPY.monthly[,6]
SPY.ret<-Delt(SPY.monthly$data.SPY.Adjusted)
names(SPY.ret)<-paste("SPY.Ret")
SPY.ret[c(1:3,nrow(SPY.ret)),]

# Aggregate Bond Market ETF Data
data.LAG<-read.csv("SPAB Yahoo.csv",header=TRUE)
data.LAG<-data.LAG[,c(1,2,3,4,5,7,6)]
date<-as.Date(data.LAG$Date,format="%Y-%m-%d")
data.LAG<-cbind(date, data.LAG[,-1])
data.LAG<-data.LAG[order(data.LAG$date),]
data.LAG<-xts(data.LAG[,2:7],order.by=data.LAG[,1])
names(data.LAG)<-paste(c("LAG.Open","LAG.High","LAG.Low","LAG.Close","LAG.Volume","LAG.Adjusted"))
LAG.monthly<-to.monthly(data.LAG)
LAG.monthly<-LAG.monthly[,6]
LAG.ret<-Delt(LAG.monthly$data.LAG.Adjusted)
names(LAG.ret)<-paste("LAG.Ret")
LAG.ret[c(1:3,nrow(LAG.ret)),]

### Fewer digits on display
options(digits=3)
### Combined return monthly data for two securities
Ret.monthly<-cbind(SPY.ret[-1,],LAG.ret[-1,])
Ret.monthly[c(1:3,nrow(Ret.monthly)),]

### Mean and Standard Deviation for two securities
SPY.Avg<-mean(Ret.monthly$SPY.Ret)
SPY.Avg
SPY.sd<-sd(Ret.monthly$SPY.Ret)
SPY.sd
LAG.Avg<-mean(Ret.monthly$LAG.Ret)
LAG.Avg
LAG.sd<-sd(Ret.monthly$LAG.Ret)
LAG.sd

### Covariance of two securities' return
covar<-cov(Ret.monthly$SPY.Ret,Ret.monthly$LAG.Ret)
covar

### No labels and display of scientific notation
options(scipen=100)
covar<-covar[1,1]
covar

### Series of sequence of weight of SPY
Portfolio<-data.frame(seq(0,1,by=.01))
names(Portfolio)<-paste("SPY.wgt")
Portfolio[1:5,]
Portfolio[(nrow(Portfolio)-5):nrow(Portfolio),]

### Weight of LAG = 1 - Weight of SPY
Portfolio$LAG.wgt<-1-Portfolio$SPY.wgt
Portfolio[c(1:3,nrow(Portfolio)),]

### Use weight to compute port ret
Portfolio$PortRet<-Portfolio$SPY.wgt*SPY.Avg+Portfolio$LAG.wgt*LAG.Avg
Portfolio[c(1:3,nrow(Portfolio)),]

### sqrt of w1^2*sd1^2 + w2^2*sd2^2 + 2*w1*w2*covariance1,2
Portfolio$PortRisk<-sqrt((Portfolio$SPY.wgt**2*SPY.sd**2)+(Portfolio$LAG.wgt**2*LAG.sd**2)+(2*covar*Portfolio$SPY.wgt*Portfolio$LAG.wgt))
Portfolio[c(1:3,nrow(Portfolio)),]

### min var achieved when risk is minimized
minvar.port<-subset(Portfolio,Portfolio$PortRisk==min(Portfolio$PortRisk))
minvar.port

### tangent portfolio: import idea of sharpe ratio and find maximum of it
riskfree=.0007/12
Portfolio$Sharpe<-(Portfolio$PortRet-riskfree)/Portfolio$PortRisk
Portfolio[c(1:3,nrow(Portfolio)),]
tangency.port<-subset(Portfolio,Portfolio$Sharpe==max(Portfolio$Sharpe))
tangency.port

### Efficient portfolio = return higher than minvar portfolio's return
eff.frontier<-subset(Portfolio,Portfolio$PortRet>=minvar.port$PortRet)
eff.frontier[c(1:3,nrow(eff.frontier)),]


plot(x=Portfolio$PortRisk,xlab="Portfolio Risk",y=Portfolio$PortRet,ylab="Portfolio Return",col="gray40",main=" Mean-Variance Efficient Frontier of Two AssetsBased on the 'Long Way' ")
abline(h=0,lty=1)
points(x=minvar.port$PortRisk,y=minvar.port$PortRet,pch=17,cex=3)
points(x=tangency.port$PortRisk,y=tangency.port$PortRet,pch=19,cex=3)
points(x=eff.frontier$PortRisk,y=eff.frontier$PortRet)

### Convert monthly return data into a matrix with renamed columns 
Ret.monthly[c(1:3,nrow(Ret.monthly)),]
mat.ret<-matrix(Ret.monthly,nrow(Ret.monthly))
mat.ret[1:3,]
colnames(mat.ret)<-c("SPY","LAG")
head(mat.ret)
tail(mat.ret)

### cov command to compute covariance of a matrix in R
VCOV<-cov(mat.ret)
VCOV

### Average return of two securities in matrix
avg.ret<-matrix(apply(mat.ret,2,mean))
colnames(avg.ret)<-paste("Avg.Ret")
rownames(avg.ret)<-paste(c("SPY","LAG"))
avg.ret

### min of average as min, max of average as max
min.ret<-min(avg.ret)
min.ret
max.ret<-max(avg.ret)
max.ret

### Sequence of return with 100-step increment
increments=100
tgt.ret<-seq(min.ret,max.ret,length=increments)
head(tgt.ret)
tail(tgt.ret)

### pre-set sd sequence of 100-step increment
tgt.sd<-rep(0,length=increments)
tgt.sd

### pre-set weight sequence of 100 * 2 format
wgt<-matrix(0,nrow=increments,ncol=length(avg.ret))
head(wgt)
tail(wgt)

library(quadprog)
for (i in 1:increments){ 
  Dmat<-2*VCOV
  dvec<-c(rep(0,length(avg.ret)))
  Amat<-cbind(rep(1,length(avg.ret)),avg.ret,diag(1,nrow=ncol(Ret.monthly)))
  bvec<-c(1,tgt.ret[i],rep(0,ncol(Ret.monthly)))
  soln<-solve.QP(Dmat,dvec,Amat,bvec=bvec,meq=2)
  tgt.sd[i]<-sqrt(soln$value)
  wgt[i,]<-soln$solution}

head(tgt.sd)
tail(tgt.sd)
options(scipen=100)
head(wgt)
tail(wgt)
colnames(wgt)<-paste(c("wgt.SPY","wgt.LAG"))
wgt[1,1]<-0
wgt[nrow(wgt),2]<-0
head(wgt)
tail(wgt)


tgt.port<-data.frame(cbind(tgt.ret,tgt.sd,wgt))
head(tgt.port)
tail(tgt.port)


minvar.port<-subset(tgt.port,tgt.port$tgt.sd==min(tgt.port$tgt.sd))
minvar.port


riskfree
tgt.port$Sharpe<-(tgt.port$tgt.ret-riskfree)/tgt.port$tgt.sd
head(tgt.port)
tail(tgt.port)
tangency.port<-subset(tgt.port,tgt.port$Sharpe==max(tgt.port$Sharpe))
tangency.port


eff.frontier<-subset(tgt.port,tgt.port$tgt.ret>=minvar.port$tgt.ret)
eff.frontier[c(1:3,nrow(eff.frontier)),]

plot(x=tgt.sd,xlab="Portfolio Risk",y=tgt.ret,ylab="Portfolio Return",col="gray40",main="Mean-Variance Efficient Frontier of Two Assets
+ Based on the Quadratic Programming Approach")
abline(h=0,lty=1)
points(x=minvar.port$tgt.sd,y=minvar.port$tgt.ret,pch=17,cex=3)
points(x=tangency.port$tgt.sd,y=tangency.port$tgt.ret,pch=19,cex=3)
points(x=eff.frontier$tgt.sd,y=eff.frontier$tgt.ret)



# Large-Cap ETF Data
SPY.ret[c(1:3,nrow(SPY.ret)),]
# Aggregate Bond Market Data
LAG.ret[c(1:3,nrow(LAG.ret)),]
# Small Cap Data Data
data.SLY<-read.csv("SLY Yahoo.csv",header=TRUE)
data.SLY = data.SLY[,c(1,2,3,4,5,7,6)]
date<-as.Date(data.SLY$Date,format="%Y-%m-%d")
data.SLY<-cbind(date, data.SLY[,-1])
data.SLY<-data.SLY[order(data.SLY$date),]
data.SLY<-xts(data.SLY[,2:7],order.by=data.SLY[,1])
names(data.SLY)<-paste(c("SLY.Open","SLY.High","SLY.Low","SLY.Close","SLY.Volume","SLY.Adjusted"))
SLY.monthly<-to.monthly(data.SLY)
SLY.monthly<-SLY.monthly[,6]
SLY.ret<-Delt(SLY.monthly$data.SLY.Adjusted)
names(SLY.ret)<-paste("SLY.Ret")
SLY.ret[c(1:3,nrow(SLY.ret)),]


# Global Equities Data
data.CWI<-read.csv("CWI Yahoo.csv",header=TRUE)
data.CWI = data.CWI[,c(1,2,3,4,5,7,6)]
date<-as.Date(data.CWI$Date,format="%Y-%m-%d")
data.CWI<-cbind(date, data.CWI[,-1])
data.CWI<-data.CWI[order(data.CWI$date),]
data.CWI<-xts(data.CWI[,2:7],order.by=data.CWI[,1])
names(data.CWI)<-paste(c("CWI.Open","CWI.High","CWI.Low","CWI.Close","CWI.Volume","CWI.Adjusted"))
CWI.monthly<-to.monthly(data.CWI)
CWI.monthly<-CWI.monthly[,6]
CWI.ret<-Delt(CWI.monthly$data.CWI.Adjusted)
names(CWI.ret)<-paste("CWI.Ret")
CWI.ret[c(1:3,nrow(CWI.ret)),]


Ret.monthly<-cbind(SPY.ret[-1,],LAG.ret[-1,],SLY.ret[-1,],CWI.ret[-1,])
Ret.monthly[c(1:3,nrow(Ret.monthly)),]
mat.ret<-matrix(Ret.monthly,nrow(Ret.monthly))
mat.ret[1:3,]
colnames(mat.ret)<-c("SPY","LAG","SLY","CWI")
head(mat.ret)
tail(mat.ret)


VCOV<-cov(mat.ret)
VCOV

avg.ret<-matrix(apply(mat.ret,2,mean))
rownames(avg.ret)<-c("SPY","LAG","SLY","CWI")
colnames(avg.ret)<-c("Avg.Ret")
avg.ret


min.ret<-min(avg.ret)
min.ret
max.ret<-max(avg.ret)
max.ret


increments=100
tgt.ret<-seq(min.ret,max.ret,length=increments)
head(tgt.ret)
tail(tgt.ret)

tgt.sd<-rep(0,length=increments)
tgt.sd

wgt<-matrix(0,nrow=increments,ncol=length(avg.ret))
head(wgt)
tail(wgt)


library(quadprog)
for (i in 1:increments){
  Dmat<-2*VCOV
  dvec<-c(rep(0,length(avg.ret)))
  Amat<-cbind(rep(1,length(avg.ret)),avg.ret,diag(1,nrow=ncol(Ret.monthly)))
  bvec<-c(1,tgt.ret[i],rep(0,ncol(Ret.monthly)))
  soln<-solve.QP(Dmat,dvec,Amat,bvec=bvec,meq=2)
  tgt.sd[i]<-sqrt(soln$value)
  wgt[i,]<-soln$solution}


head(tgt.sd)
tail(tgt.sd)
head(wgt)
tail(wgt)
colnames(wgt)<-c("wgt.SPY","wgt.LAG","wgt.SLY","wgt.CWI")
wgt[,1]<-ifelse(abs(wgt[,1])<=0.0000001,0,wgt[,1])
wgt[,2]<-ifelse(abs(wgt[,2])<=0.0000001,0,wgt[,2])
wgt[,3]<-ifelse(abs(wgt[,3])<=0.0000001,0,wgt[,3])
wgt[,4]<-ifelse(abs(wgt[,4])<=0.0000001,0,wgt[,4])
head(wgt)
tail(wgt)
CHECK<-rowSums(wgt)
CHECK


tgt.port<-data.frame(cbind(tgt.ret,tgt.sd,wgt))
head(tgt.port)
tail(tgt.port)
no.short.tgt.port<-tgt.port

minvar.port<-subset(tgt.port,tgt.port$tgt.sd==min(tgt.port$tgt.sd))
minvar.port


riskfree
tgt.port$Sharpe<-(tgt.port$tgt.ret-riskfree)/tgt.port$tgt.sd
head(tgt.port)
tail(tgt.port)
tangency.port<-subset(tgt.port,tgt.port$Sharpe==max(tgt.port$Sharpe))
tangency.port

eff.frontier<-subset(tgt.port,tgt.port$tgt.ret>=minvar.port$tgt.ret)
eff.frontier[c(1:3,nrow(eff.frontier)),]


plot(x=tgt.sd,y=tgt.ret,col="gray40",xlab="Portfolio Risk",ylab="Portfolio Return",
main="Mean-Variance Efficient Frontier of Four Assets
+ Based on the Quadratic Programming Approach
+ (Not Allowing Short Selling)")
abline(h=0,lty=1)
points(x=minvar.port$tgt.sd,y=minvar.port$tgt.ret,pch=17,cex=3)
points(x=tangency.port$tgt.sd,y=tangency.port$tgt.ret,pch=19,cex=3)
points(x=eff.frontier$tgt.sd,y=eff.frontier$tgt.ret)


tgt.ret<-seq(min.ret,max.ret*2,length=increments)
head(tgt.ret)
tail(tgt.ret)


library(quadprog)
for (i in 1:length(tgt.ret)){
  Dmat<-2*VCOV
  dvec<-c(rep(0,length(avg.ret)))
  Amat<-cbind(rep(1,length(avg.ret)),avg.ret)
  bvec<-c(1,tgt.ret[i])
  soln<-solve.QP(Dmat,dvec,Amat,bvec=bvec,meq=2)
  tgt.sd[i]<-sqrt(soln$value)
  wgt[i,]<-soln$solution}
head(tgt.sd)
tail(tgt.sd)

tgt.port<-data.frame(cbind(tgt.ret,tgt.sd,wgt))
head(tgt.port)
tail(tgt.port)
with.short.tgt.port<-tgt.port


head(wgt)

tail(wgt)
CHECK.wgt<-rowSums(wgt)
CHECK.wgt

minvar.port<-subset(tgt.port,tgt.port$tgt.sd==min(tgt.port$tgt.sd))
minvar.port


riskfree
tgt.port$Sharpe<-(tgt.port$tgt.ret-riskfree)/tgt.port$tgt.sd
head(tgt.port)
tangency.port<-subset(tgt.port,tgt.port$Sharpe==max(tgt.port$Sharpe))
tangency.port

eff.frontier<-subset(tgt.port,tgt.port$tgt.ret>=minvar.port$tgt.ret)
eff.frontier[c(1:3,nrow(eff.frontier)),]


plot(x=tgt.sd,y=tgt.ret,col="gray40",xlab="Portfolio Risk",ylab="Portfolio Return",
main="Mean-Variance Efficient Frontier of Four Assets
+ Based on the Quadratic Programming Approach
+ (Allowing Short Selling)")
abline(h=0,lty=1)
points(x=minvar.port$tgt.sd,y=minvar.port$tgt.ret,pch=17,cex=3)
points(x=tangency.port$tgt.sd,y=tangency.port$tgt.ret,pch=19,cex=3)
points(x=eff.frontier$tgt.sd,y=eff.frontier$tgt.ret)


plot(x=no.short.tgt.port$tgt.sd,y=no.short.tgt.port$tgt.ret,xlab="Portfolio Risk",ylab="Portfolio Return",type="l",lwd=6,lty=3,
main="MV Efficient Frontier for Four Assets
+ With and Without Short Selling")
lines(x=with.short.tgt.port$tgt.sd,y=with.short.tgt.port$tgt.ret,col="gray60",type="l",lwd=2)
legend("bottomright",c("Not Allow Short","Allow Short"),col=c("black","gray60"),lty=c(3,1),lwd=c(6,2))


