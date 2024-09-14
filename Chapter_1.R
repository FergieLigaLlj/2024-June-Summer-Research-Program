unlink("D:/R-4.4.0/library/00LOCK", recursive = TRUE)
install.packages("quantmod", dependencies = TRUE, INSTALL_opts = '--no-lock')
install.packages("xts", dependencies = TRUE, INSTALL_opts = '--no-lock')
install.packages("dplyr", dependencies = TRUE, INSTALL_opts = '--no-lock')
data.AMZN<-read.csv("AMZN Yahooo.csv",header=TRUE)
data.AMZN <-data.AMZN[nrow(data.AMZN):1,]
head(data.AMZN)
tail(data.AMZN)
class(data.AMZN$Date)
date<-as.Date(data.AMZN$Date,format="%Y-%m-%d")
head(date)
tail(date)
class(date)
data.AMZN<-cbind(date, data.AMZN[,-1])
head(data.AMZN)
tail(data.AMZN)
data.AMZN<-data.AMZN[order(data.AMZN$date),]
head(data.AMZN)
tail(data.AMZN)
class(data.AMZN)
library(xts)
data.AMZN<-xts(data.AMZN[,2:7],order.by=data.AMZN[,1])
head(data.AMZN)
tail(data.AMZN)
class(data.AMZN)
names(data.AMZN)
names(data.AMZN)<-paste(c("AMZN.Open","AMZN.High","AMZN.Low","AMZN.Close","AMZN.Adjusted","AMZN.Volume"))
head(data.AMZN)
tail(data.AMZN)
plot(data.AMZN$AMZN.Close)
data.missing<-data.AMZN[-400:-500,]
plot(data.missing$AMZN.Close)
dim(data.AMZN)
summary(data.AMZN)
AMZN.onlyFirst<-data.AMZN[1,]
AMZN.onlyFirst
AMZN.delFirst<-data.AMZN[-1,]
head(AMZN.delFirst)
data.AMZN[c(1,nrow(data.AMZN)),]
AMZN.first.week<-data.AMZN[2:6,]
AMZN.first.week
AMZN.last30<-data.AMZN[(nrow(data.AMZN)-29):nrow(data.AMZN),]
AMZN.last30
nrow(AMZN.last30)
data.AMZN[c(1:3,nrow(data.AMZN)),]
names(data.AMZN)
AMZN.onlyPrice<-data.AMZN[,4]
AMZN.onlyPrice[c(1:3,nrow(AMZN.onlyPrice)),]
AMZN.onlyPrice2<-data.AMZN$AMZN.Close
AMZN.onlyPrice2[c(1:3,nrow(AMZN.onlyPrice2)),]
AMZN.delAdjPrice<-data.AMZN[,-5]
AMZN.delAdjPrice[c(1:3,nrow(AMZN.delAdjPrice)),]
AMZN.OpenClose<-data.AMZN[,c(1,4)]
AMZN.OpenClose[c(1:3,nrow(AMZN.OpenClose)),]
AMZN.PriceVol<-data.AMZN[,4:5]
AMZN.PriceVol[c(1:3,nrow(AMZN.PriceVol)),]
AMZN.OpenCloseVol<-data.AMZN[,c(1,4:5)]
AMZN.OpenCloseVol[c(1:3,nrow(AMZN.OpenCloseVol)),]
AMZN.OpenCloseVol<-data.AMZN[,c(-2:-3,-6)]
AMZN.OpenCloseVol[c(1:3,nrow(AMZN.OpenCloseVol)),]
data.vwap<-data.AMZN[((nrow(data.AMZN)-29)):nrow(data.AMZN),c(4,5)]
data.vwap[c(1:3,nrow(data.vwap)),]
class(data.AMZN)
xts.data.AMZN.2012<-subset(data.AMZN[,4],index(data.AMZN) >= "2012-01-01" &index(data.AMZN) <= "2012-12-31")
xts.data.AMZN.2012[c(1:3,nrow(xts.data.AMZN.2012))]
class(index(data.AMZN))
data.frame.AMZN.2012<-cbind(index(data.AMZN),data.frame(data.AMZN[,4]))
data.frame.AMZN.2012[c(1:3,nrow(data.frame.AMZN.2012)),]
class(data.frame.AMZN.2012)
names(data.frame.AMZN.2012)[1]<-paste("date")
rownames(data.frame.AMZN.2012)<-seq(1,nrow(data.frame.AMZN.2012),1)
data.frame.AMZN.2012[c(1:3,nrow(data.frame.AMZN.2012)),]
data.frame.AMZN.2012<-subset(data.frame.AMZN.2012,data.frame.AMZN.2012$date >= "2012-01-01"&data.frame.AMZN.2012$date <= "2012-12-31")
data.frame.AMZN.2012[c(1:3,nrow(data.frame.AMZN.2012)),]



wk<-data.AMZN
wk<-wk[,c(1,2,3,4,6,5)]
data.weekly<-to.weekly(wk)
data.weekly[c(1:3,nrow(data.weekly)),]
data.AMZN[2:6,]
sum(data.AMZN[2:6,6])

mo<-data.AMZN
mo<-mo[,c(1,2,3,4,6,5)]
data.monthly<-to.monthly(mo)
data.monthly[c(1:3,nrow(data.monthly)),]

###change the order of data.AMZN to fit the following plot
data.AMZN<-data.AMZN[,c(1,2,3,4,6,5)]

data.AMZN[c(1:3,nrow(data.AMZN)),]
###Now apply OHLC
mo<-data.AMZN
data.monthly<-to.monthly(mo)
data.monthly[c(1:3,nrow(data.monthly)),]
library(quantmod)
OHLC<-data.monthly[-1,-6]
AMZN.ohlc<-as.quantmod.OHLC(OHLC,col.names=c("Open","High","Low","Close","Volume"))
class(AMZN.ohlc)
AMZN.ohlc[c(1:3,nrow(AMZN.ohlc)),]


chartSeries(AMZN.ohlc,theme="white.mono",name="AMZN OHLC")

###show data objects in memory of R
ls()

###delete all the objects
rm(list=ls())
###show again
ls()



###AMZN
data.AMZN<-read.csv("AMZN Yahooo.csv",header=TRUE)
data.AMZN<-data.AMZN[,c(1,2,3,4,5,7,6)]
date<-as.Date(data.AMZN$Date,format="%Y-%m-%d")
data.AMZN<-cbind(date, data.AMZN[,-1])
data.AMZN<-data.AMZN[order(data.AMZN$date),]
data.AMZN<-xts(data.AMZN[,2:7],order.by=data.AMZN[,1])
names(data.AMZN)<-paste(c("AMZN.Open","AMZN.High","AMZN.Low","AMZN.Close","AMZN.Volume","AMZN.Adjusted"))
data.AMZN[c(1:3,nrow(data.AMZN)),]





###AAPL
data.AAPL<-read.csv("AAPL Yahooo.csv",header=TRUE)
data.AAPL<-data.AAPL[,c(1,2,3,4,5,7,6)]
date<-as.Date(data.AAPL$Date,format="%Y-%m-%d")
data.AAPL<-cbind(date, data.AAPL[,-1])
data.AAPL<-data.AAPL[order(data.AAPL$date),]
data.AAPL<-xts(data.AAPL[,2:7],order.by=data.AAPL[,1])
names(data.AAPL)<-paste(c("AAPL.Open","AAPL.High","AAPL.Low","AAPL.Close","AAPL.Volume","AAPL.Adjusted"))
data.AAPL[c(1:3,nrow(data.AAPL)),]

###IBM
data.IBM<-read.csv("IBM Yahooo.csv",header=TRUE)
data.IBM<-data.IBM[,c(1,2,3,4,5,7,6)]
date<-as.Date(data.IBM$Date,format="%Y-%m-%d")
data.IBM<-cbind(date, data.IBM[,-1])
data.IBM<-data.IBM[order(data.IBM$date),]
data.IBM<-xts(data.IBM[,2:7],order.by=data.IBM[,1])
names(data.IBM)<-paste(c("IBM.Open","IBM.High","IBM.Low","IBM.Close","IBM.Volume","IBM.Adjusted"))
data.IBM[c(1:3,nrow(data.IBM)),]



###NVDA
###read csv file
data.NVDA<-read.csv("NVDA Yahooo.csv",header=TRUE)

###Change order of the columns in raw data
data.NVDA<-data.NVDA[,c(1,2,3,4,5,7,6)]

###Change Date column to be "Date" class object with form of date
date<-as.Date(data.NVDA$Date,format="%Y-%m-%d")

###Replace the first column by the "Date" class object
data.NVDA<-cbind(date, data.NVDA[,-1])

###Order the rows by the "Date" class order
data.NVDA<-data.NVDA[order(data.NVDA$date),]

###Make the data to be "xts" object, 
###where the first column "Date" object transferred to be order and left columns to be variables
data.NVDA<-xts(data.NVDA[,2:7],order.by=data.NVDA[,1])

###Rename the variable columns
names(data.NVDA)<-paste(c("NVDA.Open","NVDA.High","NVDA.Low","NVDA.Close","NVDA.Volume","NVDA.Adjusted"))

###output first three rows and last rows
data.NVDA[c(1:3,nrow(data.NVDA)),]


###Combine the Close columns of the four securities
Close.Prices<-data.AMZN$AMZN.Close
Close.Prices<-cbind(Close.Prices,data.NVDA$NVDA.Close,
                      + data.AAPL$AAPL.Close,data.IBM$IBM.Close)
Close.Prices[c(1:3,nrow(Close.Prices)),]


###combine the index (date above) and the whole table together and transform it into "data.frame" class
multi.df<-cbind(index(Close.Prices),
                  + data.frame(Close.Prices))

###Give the names in a simple way
names(multi.df)<-paste(c("date","AMZN","NVDA","AAPL","IBM"))

###Give the index from 1 to 755
rownames(multi.df)<-seq(1,nrow(multi.df),1)

###Output the first three rows and last row
multi.df[c(1:3,nrow(multi.df)),]

###Normalized by dividing the first price for each security
multi.df$AMZN.idx<-multi.df$AMZN/multi.df$AMZN[1]
multi.df$NVDA.idx<-multi.df$NVDA/multi.df$NVDA[1]
multi.df$AAPL.idx<-multi.df$AAPL/multi.df$AAPL[1]
multi.df$IBM.idx<-multi.df$IBM/multi.df$IBM[1]
options(digits=5)
multi.df[c(1:3,nrow(multi.df)),]





###chart the data by plot NVDA first
plot(x=multi.df$date,y=multi.df$NVDA.idx,type="l",xlab="Date",ylab="Value of Investment within NVDA ($)",col="black",lty=1,lwd=2,main="Value of $1 Investment in
+ AMZN, NVDA, AAPL, and the IBM compared to NVDA
+ December 31, 2010 - December 31, 2013",ylim = c(0.5, 2.5))


###Add the three other lines
lines(x=multi.df$date,y=multi.df$AMZN.idx,col="black",lty=2,lwd=2)
lines(x=multi.df$date,y=multi.df$AAPL.idx,col="gray",lty=1,lwd=3)
lines(x=multi.df$date,y=multi.df$IBM.idx,col="gray",lty=1,lwd=1)
abline(h=1,lty=1,col="black")


###Add the legend
legend("topleft",c("NVDA","AMZN","AAPL","IBM"),col=c("black","black","gray","grey"),lty=c(1,2,1,1),lwd=c(2,2,3,1))


###Find y range
y.range<-range(multi.df[,6:9])
y.range


###Employ all by use this y range
###chart the data by plot NVDA first
plot(x=multi.df$date,y=multi.df$NVDA.idx,type="l",xlab="Date",ylab="Value of Investment within NVDA ($)",col="black",lty=1,lwd=2,main="Value of $1 Investment in
+ AMZN, NVDA, AAPL, and the IBM compared to NVDA
+ December 31, 2010 - December 31, 2013",ylim = y.range)


###Add the three other lines
lines(x=multi.df$date,y=multi.df$AMZN.idx,col="black",lty=2,lwd=2)
lines(x=multi.df$date,y=multi.df$AAPL.idx,col="gray",lty=1,lwd=3)
lines(x=multi.df$date,y=multi.df$IBM.idx,col="gray",lty=1,lwd=1)
abline(h=1,lty=1,col="black")


###Add the legend
legend("topleft",c("NVDA","AMZN","AAPL","IBM"),col=c("black","black","gray","grey"),lty=c(1,2,1,1),lwd=c(2,2,3,1))


###Another approach

###set the top margin
par(oma=c(0,0,3,0))

###Set the outlet of mini-plot
par(mfrow=c(2,2))

###Plot the mini-graphs


plot(x=multi.df$date,xlab="",y=multi.df$NVDA.idx,ylim=y.range,ylab="",type="l",col="gray",main="Amazon Stock")
lines(x=multi.df$date,y=multi.df$AAPL.idx,col="gray")
lines(x=multi.df$date,y=multi.df$IBM.idx,col="gray")
lines(x=multi.df$date,y=multi.df$AMZN.idx,col="black",lwd=2)
abline(h=1)
plot(x=multi.df$date,xlab="",y=multi.df$NVDA.idx,ylim=y.range,ylab="",type="l",col="gray",main="IBM Stock")
lines(x=multi.df$date,y=multi.df$AMZN.idx,col="gray")
lines(x=multi.df$date,y=multi.df$AAPL.idx,col="gray")
lines(x=multi.df$date,y=multi.df$IBM.idx,col="black",lwd=2)
abline(h=1)
plot(x=multi.df$date,xlab="",y=multi.df$IBM.idx,ylim=y.range,ylab="",type="l",col="gray",main="NVDA Stock")
lines(x=multi.df$date,y=multi.df$AMZN.idx,col="gray")
lines(x=multi.df$date,y=multi.df$AAPL.idx,col="gray")
lines(x=multi.df$date,y=multi.df$NVDA.idx,col="black",lwd=2)
abline(h=1)
plot(x=multi.df$date,xlab="",y=multi.df$NVDA.idx,ylim=y.range,ylab="",type="l",col="gray",main="AAPL Index")
lines(x=multi.df$date,y=multi.df$AMZN.idx,col="gray")
lines(x=multi.df$date,y=multi.df$IBM.idx,col="gray")
lines(x=multi.df$date,y=multi.df$AAPL.idx,col="black",lwd=2)
abline(h=1)


###Set the global titles


title1="Value of $1 Invested in NVDA, IBM, AMZN, and AAPL."
title2="December 31, 2010 - December 31, 2013"
title(main=paste(title1,"\n",title2),outer=T)



AMZN.sma<-data.AMZN[,4]
AMZN.sma[c(1:3,nrow(AMZN.sma)),]
AMZN.sma$sma50<-rollmeanr(AMZN.sma$AMZN.Close,k=50)
AMZN.sma$sma200<-rollmeanr(AMZN.sma$AMZN.Close,k=200)
AMZN.sma[c(1:3,nrow(AMZN.sma)),]

AMZN.sma[48:52,]
AMZN.sma[198:202,]

AMZN.sma2012<-subset(AMZN.sma,index(AMZN.sma)>="2012-01-01")
AMZN.sma2012[c(1:3,nrow(AMZN.sma2012)),]

y.range<-range(AMZN.sma2012,na.rm=TRUE)
y.range
par(mfrow=c(1,1))
plot(x=index(AMZN.sma2012),xlab="Date",y=AMZN.sma2012$AMZN.Close,ylim=y.range,ylab="Price ($)",type="l",main="Amazon - Simple Moving Average
+ January 1, 2012 - December 31, 2013")
lines(x=index(AMZN.sma2012),y=AMZN.sma2012$sma50)
lines(x=index(AMZN.sma2012),y=AMZN.sma2012$sma200,lty=2)
legend("topleft",c("Amazon Price","50-Day Moving Average","200-Day Moving Average"),lty=c(1,1,2))



AMZN.bb<-data.AMZN[,4]
AMZN.bb[c(1:3,nrow(AMZN.bb)),]

AMZN.bb$avg<-rollmeanr(AMZN.bb$AMZN.Close,k=20)
AMZN.bb$sd<-rollapply(AMZN.bb$AMZN.Close,width=20,FUN=sd,fill=NA)
AMZN.bb[c(1:3,nrow(AMZN.bb)),]

AMZN.bb[18:22,]

AMZN.bb2013<-subset(AMZN.bb,index(AMZN.bb)>="2013-01-01")
AMZN.bb2013[c(1:3,nrow(AMZN.bb2013)),]

AMZN.bb2013$sd2up<-AMZN.bb2013$avg+2*AMZN.bb2013$sd
AMZN.bb2013$sd2down<-AMZN.bb2013$avg-2*AMZN.bb2013$sd
AMZN.bb2013[c(1:3,nrow(AMZN.bb2013)),]



###plot bollinger bands
y.range<-range(AMZN.bb2013[,-3],na.rm=TRUE)
y.range
plot(x=index(AMZN.bb2013),xlab="Date",y=AMZN.bb2013$AMZN.Close,ylim=y.range,ylab="Price ($)",type="l",lwd=3,main="Amazon - Bollinger Bands (20 days, 2 deviations)
+ January 1, 2013 - December 31, 2013")
lines(x=index(AMZN.bb2013),y=AMZN.bb2013$avg,lty=2)
lines(x=index(AMZN.bb2013),y=AMZN.bb2013$sd2up,col="gray40")
lines(x=index(AMZN.bb2013),y=AMZN.bb2013$sd2down,col="gray40")
legend("topleft",c("Amazon Price","20-Day Moving Average","Upper Band","Lower Band"),lty=c(1,2,1,1),lwd=c(3,1,1,1),col=c("black","black","gray40","gray40"))


AMZN.RSI<-data.AMZN[,4]
AMZN.RSI$delta<-diff(AMZN.RSI$AMZN.Close)
AMZN.RSI[c(1:3,nrow(AMZN.RSI)),]

AMZN.RSI$up<-ifelse(AMZN.RSI$delta>0,1,0)
AMZN.RSI$down<-ifelse(AMZN.RSI$delta<0,1,0)
AMZN.RSI[c(1:3,nrow(AMZN.RSI)),]

AMZN.RSI$up.val<-AMZN.RSI$delta*AMZN.RSI$up
AMZN.RSI$down.val<--AMZN.RSI$delta*AMZN.RSI$down
AMZN.RSI<-AMZN.RSI[-1,]
AMZN.RSI[c(1:3,nrow(AMZN.RSI)),]


AMZN.RSI$up.first.avg<-rollapply(AMZN.RSI$up.val,width=14,FUN=mean,fill=NA,na.rm=TRUE)
AMZN.RSI$down.first.avg<-rollapply(AMZN.RSI$down.val,width=14,FUN=mean,fill=NA,na.rm=TRUE)
AMZN.RSI[c(1:15,nrow(AMZN.RSI)),]



up.val<-as.numeric(AMZN.RSI$up.val)
down.val<-as.numeric(AMZN.RSI$down.val)

AMZN.RSI$up.avg<-AMZN.RSI$up.first.avg
for (i in 15:nrow(AMZN.RSI)){AMZN.RSI$up.avg[i] <-((AMZN.RSI$up.avg[i-1]*13+up.val[i])/14)}
AMZN.RSI$down.avg<-AMZN.RSI$down.first.avg
for (i in 15:nrow(AMZN.RSI)){AMZN.RSI$down.avg[i]<-((AMZN.RSI$down.avg[i-1]*13+down.val[i])/14)}
AMZN.RSI[c(1:20,nrow(AMZN.RSI)),]

AMZN.RSI$RS<-AMZN.RSI$up.avg/AMZN.RSI$down.avg
AMZN.RSI$RSI<-100-(100/(1+AMZN.RSI$RS))
AMZN.RSI[c(14:20,nrow(AMZN.RSI)),]

AMZN.RSI2012<-subset(AMZN.RSI[,ncol(AMZN.RSI)],index(AMZN.RSI)>="2012-01-01")
AMZN.RSI2012[c(1:3,nrow(AMZN.RSI2012)),]

title1<-"Amazon - Relative Strength Index"
title2<-"January 2012 - December 2013"
plot(x=index(AMZN.RSI2012),xlab="Date",y=AMZN.RSI2012$RSI,ylab="RSI (14-Day Moving Average)",ylim=c(0,100),type="l",main=paste(title1,"\n",title2))
abline(h=c(30,70),lty=2)


chartSeries(data.AMZN[,4],theme="white.mono",TA=c(addSMA(n=c(50,200))))
zoomChart("2012::2013")