install.packages('quantmod')
install.packages('xts')
install.packages("dplyr")
library(quantmod)
library(xts)
data.IBM<-read.csv("IBM Yahooo.csv",header=TRUE)
data.IBM<-data.IBM[,c(1,2,3,4,5,7,6)]
date<-as.Date(data.IBM$Date,format="%Y-%m-%d")
data.IBM<-cbind(date, data.IBM[,-1])
data.IBM<-data.IBM[order(data.IBM$date),]
data.IBM<-xts(data.IBM[,2:7],order.by=data.IBM[,1])
names(data.IBM)<-paste(c("IBM.Open","IBM.High","IBM.Low","IBM.Close","IBM.Volume","IBM.Adjusted"))
data.IBM[c(1:3,nrow(data.IBM)),]

IBM.prc.ret<-data.IBM[,4]
IBM.prc.ret[c(1:3,nrow(IBM.prc.ret)),]

IBM.prc.ret$IBM.prc.ret<-Delt(IBM.prc.ret$IBM.Close)
IBM.prc.ret[c(1:3,nrow(IBM.prc.ret)),]

options(digits=3)
IBM.prc.ret<-IBM.prc.ret[-1,c(2)]
IBM.prc.ret[c(1:3,nrow(IBM.prc.ret)),]
options(digits=7)

data.IBM[715:720,]

IBM.ret<-data.IBM[,6]
IBM.ret[c(1:3,nrow(IBM.ret)),]

IBM.ret$IBM.tot.ret=Delt(IBM.ret$IBM.Adjusted)
IBM.ret[c(1:3,nrow(IBM.ret)),]


options(digits=3)
IBM.total.ret<-IBM.ret[,2]
IBM.total.ret[c(1:3,nrow(IBM.ret)),]
options(digits=7)

IBM.ret<-data.IBM[,6]
IBM.ret[c(1:3,nrow(IBM.ret)),]

IBM.ret$IBM.log.ret<-diff(log(IBM.ret$IBM.Adjusted))
IBM.ret[c(1:3,nrow(IBM.ret)),]

options(digits=3)
IBM.log.ret<-IBM.ret[,2]
IBM.log.ret[c(1:3,nrow(IBM.ret)),]

options(digits=3,scipen=100)
tot.rets<-cbind(IBM.total.ret,IBM.log.ret)
tot.rets[c(1:3,nrow(tot.rets)),]
max(abs(tot.rets$IBM.tot.ret-tot.rets$IBM.log.ret),na.rm=TRUE)
min(abs(tot.rets$IBM.tot.ret-tot.rets$IBM.log.ret),na.rm=TRUE)
options(digits=7,scipen=0)

IBM.acum<-IBM.total.ret
IBM.acum[c(1:3,nrow(IBM.acum)),]

IBM.acum[1,1]<-0
IBM.acum[c(1:3,nrow(IBM.acum)),]

IBM.acum$GrossRet<-1+IBM.acum$IBM.tot.ret
IBM.acum[c(1:3,nrow(IBM.acum)),]

IBM.acum$GrossCum<-cumprod(IBM.acum$GrossRet)
IBM.acum[c(1:3,nrow(IBM.acum)),]

IBM.acum$NetCum<-IBM.acum$GrossCum-1
IBM.acum[c(1:3,nrow(IBM.acum)),]

IBM.logcum<-IBM.log.ret
IBM.logcum[c(1:3,nrow(IBM.logcum)),]

IBM.logcum[1,1]<-0
IBM.logcum[c(1:3,nrow(IBM.logcum)),]

logcumret=sum(IBM.logcum$IBM.log.ret)
logcumret

cumret=exp(logcumret)-1
cumret

IBM.Ret<-cbind(IBM.prc.ret,IBM.acum[,1])
names(IBM.Ret)<-c("prc.ret","tot.ret")
IBM.Ret[c(1:3,nrow(IBM.Ret)),]

IBM.Ret$prc.ret[1]<-0
IBM.Ret$tot.ret[1]<-0
IBM.Ret[c(1:3,nrow(IBM.Ret)),]

IBM.Ret$gross.prc<-1+IBM.Ret$prc.ret
IBM.Ret$gross.tot<-1+IBM.Ret$tot.ret
IBM.Ret[c(1:3,nrow(IBM.Ret)),]

IBM.Ret$cum.prc<-cumprod(IBM.Ret$gross.prc)
IBM.Ret$cum.tot<-cumprod(IBM.Ret$gross.tot)
IBM.Ret[c(1:3,nrow(IBM.Ret)),]

y.range<-range(IBM.Ret[,5:6])
y.range
plot(IBM.Ret$cum.tot,type="l",auto.grid=FALSE,xlab="Date",ylab="Value of Investment ($)",ylim=y.range,minor.ticks=FALSE,main="IBM Stock Performance Based On
+ Total Returns and Price Returns
+ December 31, 2010 - December 31, 2013",col="black")
lines(IBM.Ret$cum.prc,type="l",lty=3,col="black")
legend("topleft",col=c("black","black"),lty=c(1,3),c("Value Based on Total Return","Value Based on Price Return"))
abline(h=1)

data.AMZN<-read.csv("AMZN Yahooo.csv",header=TRUE)
data.AMZN<-data.AMZN[,c(1,2,3,4,5,7,6)]
date<-as.Date(data.AMZN$Date,format="%Y-%m-%d")
data.AMZN<-cbind(date, data.AMZN[,-1])
data.AMZN<-data.AMZN[order(data.AMZN$date),]
data.AMZN<-xts(data.AMZN[,2:7],order.by=data.AMZN[,1])
names(data.AMZN)<-paste(c("AMZN.Open","AMZN.High","AMZN.Low","AMZN.Close","AMZN.Volume","AMZN.Adjusted"))
data.AMZN[c(1:3,nrow(data.AMZN)),]

class(data.AMZN)

wk<-data.AMZN
wk[c(1:3,nrow(data.AMZN)),]

AMZN.weekly<-to.weekly(wk)
AMZN.weekly[c(1:3,nrow(AMZN.weekly)),]

AMZN.weekly<-AMZN.weekly[,6]
AMZN.weekly[c(1:3,nrow(AMZN.weekly)),]

AMZN.weekly$Ret<-Delt(AMZN.weekly$wk.Adjusted)
AMZN.weekly[c(1:3,nrow(AMZN.weekly)),]

AMZN.weekly<-AMZN.weekly[-1,2]
AMZN.weekly[c(1:3,nrow(AMZN.weekly)),]

mo<-data.AMZN
mo[c(1:3,nrow(data.AMZN)),]

AMZN.monthly<-to.monthly(mo)
AMZN.monthly[c(1:3,nrow(AMZN.monthly)),]

AMZN.monthly<-AMZN.monthly[,6]
AMZN.monthly[c(1:3,nrow(AMZN.monthly)),]

AMZN.monthly$Ret<-Delt(AMZN.monthly$mo.Adjusted)
AMZN.monthly[c(1:3,nrow(AMZN.monthly)),]

AMZN.monthly<-AMZN.monthly[-1,2]
AMZN.monthly[c(1:3,nrow(AMZN.monthly)),]

data.AMZN[c(1:3,nrow(data.AMZN)),]
data.IBM[c(1:3,nrow(data.IBM)),]

# AAPL Data
data.AAPL<-read.csv("AAPL Yahooo.csv",header=TRUE)
data.AAPL<-data.AAPL[,c(1,2,3,4,5,7,6)]
date<-as.Date(data.AAPL$Date,format="%Y-%m-%d")
data.AAPL<-cbind(date, data.AAPL[,-1])
data.AAPL<-data.AAPL[order(data.AAPL$date),]
data.AAPL<-xts(data.AAPL[,2:7],order.by=data.AAPL[,1])
names(data.AAPL)[1:6]<-paste(c("AAPL.Open","AAPL.High","AAPL.Low","AAPL.Close","AAPL.Volume","AAPL.Adjusted"))
data.AAPL[c(1:3,nrow(data.AAPL)),]

# NVDA Data
data.NVDA<-read.csv("NVDA Yahooo.csv",header=TRUE)
data.NVDA<-data.NVDA[,c(1,2,3,4,5,7,6)]
date<-as.Date(data.NVDA$Date,format="%Y-%m-%d")
data.NVDA<-cbind(date, data.NVDA[,-1])
data.NVDA<-data.NVDA[order(data.NVDA$date),]
data.NVDA<-xts(data.NVDA[,2:7],order.by=data.NVDA[,1])
names(data.NVDA)[1:6]<-paste(c("NVDA.Open","NVDA.High","NVDA.Low","NVDA.Close","NVDA.Volume","NVDA.Adjusted"))
data.NVDA[c(1:3,nrow(data.NVDA)),]

multi<-data.AMZN[,6]
multi<-merge(multi,data.AAPL[,6])
multi<-merge(multi,data.NVDA[,6])
multi<-merge(multi,data.IBM[,6])
multi[c(1:3,nrow(multi)),]

multi.df<-cbind(data.frame(index(multi)),data.frame(multi))
names(multi.df)<-paste(c("date","AMZN","AAPL","NVDA","IBM"))
multi.df[c(1:3,nrow(multi.df)),]

multi.df$AMZN.idx<-multi.df$AMZN/multi.df$AMZN[1]
multi.df$AAPL.idx<-multi.df$AAPL/multi.df$AAPL[1]
multi.df$NVDA.idx<-multi.df$NVDA/multi.df$NVDA[1]
multi.df$IBM.idx<-multi.df$IBM/multi.df$IBM[1]
multi.df[c(1:3,nrow(multi.df)),6:9]


y.range<-range(multi.df[,6:9])
y.range
par(mfrow=c(1,1))
plot(x=multi.df$date,xlab="Date",
y=multi.df$NVDA.idx,ylim=y.range,ylab="Value of $1 Investment ($)",type="l",col="black",lty=1,lwd=2,main="Value of $1 Invested in AMZN, IBM, AAPL,
+ And the NVDA Based on Total Returns
+ December 31, 2010 - December 31, 2013")
lines(x=multi.df$date,y=multi.df$AMZN.idx,col="black",lty=2,lwd=1)
lines(x=multi.df$date,y=multi.df$IBM.idx,col="gray40",lty=1,lwd=2)
lines(x=multi.df$date,y=multi.df$AAPL.idx,col="gray60",lty=1,lwd=1)
abline(h=1,lty=1,col="black")
legend("topleft",c("AMZN","IBM","AAPL","NVDA"),col=c("black","gray40","gray60","black"),lty=c(2,1,1,1),lwd=c(1,2,1,2))