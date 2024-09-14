install.packages('quantmod')
install.packages('xts')
install.packages("dplyr")
library(quantmod)
library(xts)

### It should be noted that every "Output" comment means output first three rows combined with the last row ###

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


###True Chapter 3
###Periodic Return between first day and last day of the time duration
period.ret<-multi[c(1,nrow(multi)),]
period.ret

###lapply for applying Delt for all four variables
rets<-lapply(period.ret,Delt)
rets

###convert from list to data.frame
rets<-data.frame(rets)
rets

###Only keep the second row since first row "NA"
rets<-rets[2,]*100
###Rename with efficiency
names(rets)<-paste(c("AMZN","AAPL","NVDA","IBM"))
###output
rets

###Dollars in AMZN
i.AMZN<-50000
###Dollars in AAPL
i.AAPL<-10000
###Dollars in NVDA
i.NVDA<-30000
###Dollars in IBM
i.IBM<-10000
###AMZN weight
w.AMZN<-i.AMZN/(i.AMZN+i.AAPL+i.NVDA+i.IBM)
w.AMZN
###AAPL weight
w.AAPL<-i.AAPL/(i.AMZN+i.AAPL+i.NVDA+i.IBM)
w.AAPL
###NVDA weight
w.NVDA<-i.NVDA/(i.AMZN+i.AAPL+i.NVDA+i.IBM)
w.NVDA
###IBM weight
w.IBM<-i.IBM/(i.AMZN+i.AAPL+i.NVDA+i.IBM)
w.IBM

###TOTAL PERCENTAGE RATE RETURN BY WEIGHTED SUMMATION
port.ret.4asset<-w.AMZN*rets$AMZN+w.AAPL*rets$AAPL+ w.NVDA*rets$NVDA+w.IBM*rets$IBM
###OUTPUT
port.ret.4asset

###Weight row vector with a row of 4 numbers
wgt<-c(0.5,0.1,0.3,0.1)
mat.wgt<-matrix(wgt,1)
mat.wgt

###Return, a four-row vector with one return each in a row
ret<-c(rets$AMZN,rets$AAPL,rets$NVDA,rets$IBM)
mat.ret<-matrix(ret,4)
mat.ret

###Two vectors multiplied together
port.ret<-mat.wgt %*% mat.ret
port.ret

###Make sure that AMZN's data still in R Memory
data.AMZN[c(1:3,nrow(data.AMZN)),]

###Make sure that NVDA's data still in R Memory
data.NVDA[c(1:3,nrow(data.NVDA)),]

###Make sure that IBM's data still in R Memory
data.IBM[c(1:3,nrow(data.IBM)),]

###Merge Close and Adjusted Price for each three security
port<-data.AMZN[,c(4,6)]
port<-merge(port,data.NVDA[,c(4,6)])
port<-merge(port,data.IBM[,c(4,6)])
port[c(1:3,nrow(port)),]

###For each security,we use delta of adjusted close price to calculate return
port$AMZN.ret<-Delt(port$AMZN.Adjusted)
port$NVDA.ret<-Delt(port$NVDA.Adjusted)
port$IBM.ret<-Delt(port$IBM.Adjusted)
port[c(1:3,nrow(port)),]

###To only show data from 2012-12-31 to 2013-12-31 by using subset
port<-subset(port,
               + index(port) >= "2012-12-31" &
                 + index(port) <= "2013-12-31")
port[c(1:3,nrow(port)),]

###The provement of not efficient codes and output in the book with codes pasted here
port<-cbind(data.frame(index(port)),
              + data.frame(port))
names(port)[1]<-paste("date")
port[c(1:3,nrow(port)),]
port<-subset(port,
               + port$date >= "2012-12-31" &
                 + port$date <= "2013-12-31")
port[c(1:3,nrow(port)),]

### Keep 1st,8th,9th,10th column of above port data
ewport<-port[c(1,8:10)]
### Output
ewport[c(1:3,nrow(ewport)),]

### Rename in a way of simplicity
names(ewport)<-paste(c("date","AMZN","NVDA","IBM"))
### Reorder the index from 1 to 2 to 3 to 4 to ...
rownames(ewport)<-seq(1:nrow(ewport))
### Output
ewport[c(1:3,nrow(ewport)),]

### Replace the net return by gross return for AMZN by adding one to net return
ewport$AMZN<-1+ewport$AMZN
### Replace the net return by gross return for NVDA by adding one to net return
ewport$NVDA<-1+ewport$NVDA
### Replace the net return by gross return for IBM by adding one to net return
ewport$IBM<-1+ewport$IBM
### Output
ewport[c(1:3,nrow(ewport)),]

### We limit date from 2012-12-31 to 2013-03-31 to initiate the Q1 return, note that 2012-12-31
### is the date for investment, functioning as the placeholder
ew.q1<-subset(ewport,
                + ewport$date >= as.Date("2012-12-31") &
                  + ewport$date <= as.Date("2013-03-31"))
### Output
ew.q1[c(1:3,nrow(ew.q1)),]

### Set the first row data except "date" as 1,since 2012-12-31 is a placeholder for indexing,
### whose return on that date is meaningless, so set as "1".
ew.q1[1,2:4]<-1
### use cumprod command to calculate cumulative return in Q1 for AMZN
ew.q1$AMZN<-cumprod(ew.q1$AMZN)
### use cumprod command to calculate cumulative return in Q1 for NVDA
ew.q1$NVDA<-cumprod(ew.q1$NVDA)
### use cumprod command to calculate cumulative return in Q1 for IBM
ew.q1$IBM<-cumprod(ew.q1$IBM)
ew.q1[c(1:3,nrow(ew.q1)),]

### Set up a variable that means the number of securities = 3
num.sec<-3
num.sec

### Calculate the index value of AMZN by multiplying Gross Return with percentage proportion
ew.q1$AMZN.idx<-(1/num.sec)*ew.q1$AMZN
### Calculate the index value of NVDA by multiplying Gross Return with percentage proportion
ew.q1$NVDA.idx<-(1/num.sec)*ew.q1$NVDA
### Calculate the index value of IBM by multiplying Gross Return with percentage proportion
ew.q1$IBM.idx<-(1/num.sec)*ew.q1$IBM
### Output
ew.q1[c(1:3,nrow(ew.q1)),]

### Use rowSums command to calculate the sum of 5th to 7th column as EW portfolio value
### Then transform it to be data.frame object
q1.val<-data.frame(rowSums(ew.q1[,5:7]))
### Output
q1.val[c(1:3,nrow(q1.val)),]
### Give a name to the value
names(q1.val)<-paste("port.val")
### Import "date" column into the q1.val dataset
q1.val$date<-ew.q1$date
### Output again
q1.val[c(1:3,nrow(q1.val)),]
### In the last row, choose the value in the first column as q2.inv,
### which is the final EW return for Q1
q2.inv<-q1.val[nrow(q1.val),1]
### Output this value
q2.inv

### Limit Q2 duration from 2013-04-01 to 2013-06-30
ew.q2<-subset(ewport,
                + ewport$date >= as.Date("2013-04-01") &
                  + ewport$date <= as.Date("2013-06-30"))
### Output
ew.q2[c(1:3,nrow(ew.q2)),]

### Again,Calculate cumulative return for each security for Q2
ew.q2$AMZN<-cumprod(ew.q2$AMZN)
ew.q2$NVDA<-cumprod(ew.q2$NVDA)
ew.q2$IBM<-cumprod(ew.q2$IBM)
### Output
ew.q2[c(1:3,nrow(ew.q2)),]

### Again,calculate the index value of each security by multiplying Gross Return with percentage proportion
ew.q2$AMZN.idx<-(q2.inv/num.sec)*ew.q2$AMZN
ew.q2$NVDA.idx<-(q2.inv/num.sec)*ew.q2$NVDA
ew.q2$IBM.idx<-(q2.inv/num.sec)*ew.q2$IBM
ew.q2[c(1:3,nrow(ew.q2)),]

### Use rowSums command to calculate the sum of 5th to 7th column as EW portfolio value
### Then transform it to be data.frame object
q2.val<-data.frame(rowSums(ew.q2[,5:7]))
### Output
q2.val[c(1:3,nrow(q2.val)),]

### Give a name to the value
names(q2.val)<-paste("port.val")
### Import "date" column into the q2.val dataset
q2.val$date<-ew.q2$date
### Output again
q2.val[c(1:3,nrow(q2.val)),]

### In the last row, choose the value in the first column as q3.inv,
### which is the final EW return for Q2
q3.inv<-q2.val[nrow(q2.val),1]
### Output this value
q3.inv



### Limit Q3 duration from 2013-07-01 to 2013-09-30
ew.q3<-subset(ewport,
              + ewport$date >= as.Date("2013-07-01") &
                + ewport$date <= as.Date("2013-09-30"))
### Output
ew.q3[c(1:3,nrow(ew.q3)),]

### Again,Calculate cumulative return for each security for Q3
ew.q3$AMZN<-cumprod(ew.q3$AMZN)
ew.q3$NVDA<-cumprod(ew.q3$NVDA)
ew.q3$IBM<-cumprod(ew.q3$IBM)
### Output
ew.q3[c(1:3,nrow(ew.q3)),]

### Again,calculate the index value of each security by multiplying Gross Return with percentage proportion
ew.q3$AMZN.idx<-(q3.inv/num.sec)*ew.q3$AMZN
ew.q3$NVDA.idx<-(q3.inv/num.sec)*ew.q3$NVDA
ew.q3$IBM.idx<-(q3.inv/num.sec)*ew.q3$IBM
ew.q3[c(1:3,nrow(ew.q3)),]

### Use rowSums command to calculate the sum of 5th to 7th column as EW portfolio value
### Then transform it to be data.frame object
q3.val<-data.frame(rowSums(ew.q3[,5:7]))
### Output
q3.val[c(1:3,nrow(q3.val)),]

### Give a name to the value
names(q3.val)<-paste("port.val")
### Import "date" column into the q3.val dataset
q3.val$date<-ew.q3$date
### Output again
q3.val[c(1:3,nrow(q3.val)),]

### In the last row, choose the value in the first column as q3.inv,
### which is the final EW return for Q3
q4.inv<-q3.val[nrow(q3.val),1]
### Output this value
q4.inv





### Limit Q4 duration from 2013-10-01 to 2013-12-31
ew.q4<-subset(ewport,
              + ewport$date >= as.Date("2013-10-01") &
                + ewport$date <= as.Date("2013-12-31"))
### Output
ew.q4[c(1:3,nrow(ew.q4)),]

### Again,Calculate cumulative return for each security for Q4
ew.q4$AMZN<-cumprod(ew.q4$AMZN)
ew.q4$NVDA<-cumprod(ew.q4$NVDA)
ew.q4$IBM<-cumprod(ew.q4$IBM)
### Output
ew.q4[c(1:3,nrow(ew.q4)),]

### Again,calculate the index value of each security by multiplying Gross Return with percentage proportion
ew.q4$AMZN.idx<-(q4.inv/num.sec)*ew.q4$AMZN
ew.q4$NVDA.idx<-(q4.inv/num.sec)*ew.q4$NVDA
ew.q4$IBM.idx<-(q4.inv/num.sec)*ew.q4$IBM
ew.q4[c(1:3,nrow(ew.q4)),]

### Use rowSums command to calculate the sum of 5th to 7th column as EW portfolio value
### Then transform it to be data.frame object
q4.val<-data.frame(rowSums(ew.q4[,5:7]))
### Output
q4.val[c(1:3,nrow(q4.val)),]

### Give a name to the value
names(q4.val)<-paste("port.val")
### Import "date" column into the q3.val dataset
q4.val$date<-ew.q4$date
### Output again
q4.val[c(1:3,nrow(q4.val)),]

### Combine four data-sets one after another by stacking on top of one by one
ew.portval<-rbind(q1.val,q2.val,q3.val,q4.val)
### Output
ew.portval[c(1:3,nrow(ew.portval)),]






### Value-weighted portfolio
### Select the columns of adjusted closing prices and net returns for these three securities
vwport<-port[,c(1,2,4,6,8:10)]
### Output
vwport[c(1:3,nrow(vwport)),]

### change the index series from date object to sequence from 1 to the end
rownames(vwport)<-seq(1:nrow(vwport))
### Output
vwport[c(1:3,nrow(vwport)),]


### Again,net return is overwritten by gross return for each security of these three.
### Method is just plus one from the basic net return
vwport$AMZN.ret<-1+vwport$AMZN.ret
vwport$NVDA.ret<-1+vwport$NVDA.ret
vwport$IBM.ret<-1+vwport$IBM.ret
### Output
vwport[c(1:3,nrow(vwport)),]

### Create a sequence of "date" object from end of 2012 to end of 2013
date<-seq(as.Date("2012-12-31"),as.Date("2013-12-31"),by=1)
### Convert data: "date" to data.frame object
date<-data.frame(date)
### Output
date[c(1:3,nrow(date)),]

### Extract the first four columns of price data
PRICE.qtr<-vwport[,c(1,2,3,4)]
### Output
PRICE.qtr[c(1:3,nrow(PRICE.qtr)),]

### The merge command combines the two data objects.
### Using the all.x=TRUE option tells R that when merging we should 
### keep all the data in the “by” variable that is available in x=date data object. 
### The “by” variable in this case is the date. 
### What the na.locf command does is that it copies over the last available data in y=PRICE.qtr 
### when there is a date in x that is not available in y. 
PRICE.qtr<-na.locf(merge(x=date,y=PRICE.qtr,by="date",all.x=TRUE))
### Output
PRICE.qtr[c(1:3,nrow(PRICE.qtr)),]

### Use subset to limit PRICE.qtr data range of only four dates,
### which are the beginning date of the four quarters in 2013 
PRICE.qtr<-subset(PRICE.qtr,
                    + PRICE.qtr$date==as.Date("2012-12-31") |
                      + PRICE.qtr$date==as.Date("2013-03-31") |
                      + PRICE.qtr$date==as.Date("2013-06-30") |
                      + PRICE.qtr$date==as.Date("2013-09-30"))
### Output
PRICE.qtr

### For each security, manually set up the quarter share outstandings
PRICE.qtr$AMZN.shout<-c(4193066000,4227876000,3890850000,3095592000)
PRICE.qtr$NVDA.shout<-c(26158724000,25697000000,26973324000,17906988000)
PRICE.qtr$IBM.shout<-c(269206612,238283505,292425198,243346561)
### Output
PRICE.qtr

### Output the detail of the structure of the PRICE.qtr dataset
str(PRICE.qtr)

### rename dataset with more relation to the weights
weights<-PRICE.qtr
### calculate the market cap for each security by multiplying close price by share outstandings at the end of each quarter
weights$AMZN.mcap<-weights$AMZN.Close*weights$AMZN.shout
weights$NVDA.mcap<-weights$NVDA.Close*weights$NVDA.shout
weights$IBM.mcap<-weights$IBM.Close*weights$IBM.shout
### Output
weights

### Calculate the total market cap by summarize in row of column 8th to 10th
weights$tot.mcap<-rowSums(weights[8:10])
### Output
weights

### For each security, dividing each security's market cap by total market cap makes weight.
weights$AMZN.wgt<-weights$AMZN.mcap/weights$tot.mcap
weights$NVDA.wgt<-weights$NVDA.mcap/weights$tot.mcap
weights$IBM.wgt<-weights$IBM.mcap/weights$tot.mcap
### Output
weights

### Keep the only weights data
WEIGHT<-weights[,c(1,12:14)]
### Output
WEIGHT

### The last day's weight are applicable to the next first date of each quarter
### So we can add one to the date
WEIGHT$date<-WEIGHT$date+1
### Output
WEIGHT

### Create a sequence of date from the end of 2012 to the end of 2013
date<-seq(as.Date("2012-12-31"),as.Date("2013-12-31"),by=1)
### Convert data type to be data.frame
date<-data.frame(date)
### Output
date[c(1:3,nrow(date)),]

### Merge date and WEIGHT according to date, all variables in x=date are kept, missing values will be filled by last available variables
vwret<-na.locf(merge(x=date,y=WEIGHT,by="date",all.x=TRUE))
### Output
vwret[c(1:3,nrow(vwret)),]

### Again, view details of the dataset vwret
str(vwret)

### Extract first quarter weight by subset
q1.vw.wgt<-subset(vwret,vwret$date==as.Date("2013-01-01"))
### Output
q1.vw.wgt

### Extract second quarter weight by subset
q2.vw.wgt<-subset(vwret,vwret$date==as.Date("2013-04-01"))
### Output
q2.vw.wgt

### Extract third quarter weight by subset
q3.vw.wgt<-subset(vwret,vwret$date==as.Date("2013-07-01"))
### Output
q3.vw.wgt

### Extract fourth quarter weight by subset
q4.vw.wgt<-subset(vwret,vwret$date==as.Date("2013-10-01"))
### Output
q4.vw.wgt

### Generate a 2x2 plot
par(mfrow=c(2,2))
### Construct the numeric data except first column of date
Q1.pie.values<-as.numeric(q1.vw.wgt[,-1])
### Use the labels of the selected securities in the weight dataset above
Q1.pie.labels<-names(q1.vw.wgt[,-1])
### pie command to show pie chart with 3 type rainbow as color
pie(Q1.pie.values,labels=Q1.pie.labels,col=rainbow(length(Q1.pie.labels)),main="Q1 Value Weighting")
Q2.pie.values<-as.numeric(q2.vw.wgt[,-1])
Q2.pie.labels<-names(q1.vw.wgt[,-1])
pie(Q2.pie.values,labels=Q2.pie.labels,col=rainbow(length(Q2.pie.labels)),main="Q2 Value Weighting")
Q3.pie.values<-as.numeric(q3.vw.wgt[,-1])
Q3.pie.labels<-c("Amazon","NVDA","IBM")
pct<-round(Q3.pie.values*100)
Q3.pie.labels<-paste(Q3.pie.labels,pct) # Add Pct to Labels
Q3.pie.labels<-paste(Q3.pie.labels,"%",sep="") # Add % Sign
pie(Q3.pie.values,labels=Q3.pie.labels,col=rainbow(length(Q3.pie.labels)),main="Q3 Value Weighting")
### Construct the numeric data except first column of date
Q4.pie.values<-as.numeric(q4.vw.wgt[,-1])
### Input the three security names manually
Q4.pie.labels<-c("Amazon","NVDA","IBM")
### For each share, we use percentage data type, so multiplying 100 first
pct<-round(Q4.pie.values*100)
### paste percentage after the name of security
Q4.pie.labels<-paste(Q4.pie.labels,pct) # Add Pct to Labels
### paste the % sign after above label
Q4.pie.labels<-paste(Q4.pie.labels,"%",sep="") # Add % Sign
### pie command to show pie chart with 3 type rainbow as color
pie(Q4.pie.values,labels=Q4.pie.labels,col=rainbow(length(Q4.pie.labels)),main="Q4 Value Weighting")

### Use sub-net command to limit date range in Q1, including only three columns of return variables for three securities
vw.q1<-subset(vwport[,c(1,5:7)],
                + vwport$date >= as.Date("2012-12-31") &
                  + vwport$date <= as.Date("2013-03-31"))
### Output
vw.q1[c(1:3,nrow(vw.q1)),]

### Rename the names of columns
names(vw.q1)<-paste(c("date","AMZN","NVDA","IBM"))
### Output
vw.q1[c(1:3,nrow(vw.q1)),]

### Set the 2th to 4th columns data of first row = 1,
### since 2012-12-31 data is the placeholder
vw.q1[1,2:4]<-1
### Use cumprod command to calculate gross return for each security
vw.q1$AMZN<-cumprod(vw.q1$AMZN)
vw.q1$NVDA<-cumprod(vw.q1$NVDA)
vw.q1$IBM<-cumprod(vw.q1$IBM)
### Output
vw.q1[c(1:3,nrow(vw.q1)),]

### For each security, multiplying Q1 gross return by value-weighted weight makes the index value
vw.q1$AMZN.idx<-(1*q1.vw.wgt$AMZN.wgt)*vw.q1$AMZN
vw.q1$NVDA.idx<-(1*q1.vw.wgt$NVDA.wgt)*vw.q1$NVDA
vw.q1$IBM.idx<-(1*q1.vw.wgt$IBM.wgt)*vw.q1$IBM
vw.q1[c(1:3,nrow(vw.q1)),]

### Summarize the 5th to 7th column index values to get total return for every day of the portfolio
q1.vw.val<-data.frame(rowSums(vw.q1[,5:7]))
### Output
q1.vw.val[c(1:3,nrow(q1.vw.val)),]
### Name the data-set with "port.val" = "portfolio.value"
names(q1.vw.val)<-paste("port.val")
### Make a new column of date variable in previous data-set
q1.vw.val$date<-vw.q1$date
### Output
q1.vw.val[c(1:3,nrow(q1.vw.val)),]
### Take the index value of last day in Q1 as beginning value for Q2 
q2.vw.inv<-q1.vw.val[nrow(q1.vw.val),1]
q2.vw.inv

### Q2
vw.q2<-subset(vwport[,c(1,5:7)],
                + vwport$date >= as.Date("2013-04-01") &
                  + vwport$date <= as.Date("2013-06-30"))
vw.q2[c(1:3,nrow(vw.q2)),]
names(vw.q2)<-paste(c("date","AMZN","NVDA","IBM"))
vw.q2[c(1:3,nrow(vw.q2)),]
vw.q2$AMZN<-cumprod(vw.q2$AMZN)
vw.q2$NVDA<-cumprod(vw.q2$NVDA)
vw.q2$IBM<-cumprod(vw.q2$IBM)
vw.q2[c(1:3,nrow(vw.q2)),]
vw.q2$AMZN.idx<-(q2.vw.inv*q2.vw.wgt$AMZN.wgt)*vw.q2$AMZN
vw.q2$NVDA.idx<-(q2.vw.inv*q2.vw.wgt$NVDA.wgt)*vw.q2$NVDA
vw.q2$IBM.idx<-(q2.vw.inv*q2.vw.wgt$IBM.wgt)*vw.q2$IBM
vw.q2[c(1:3,nrow(vw.q2)),]
q2.vw.val<-data.frame(rowSums(vw.q2[,5:7]))
q2.vw.val[c(1:3,nrow(q2.vw.val)),]
names(q2.vw.val)<-paste("port.val")
q2.vw.val$date<-vw.q2$date
q2.vw.val[c(1:3,nrow(q2.vw.val)),]
q3.vw.inv<-q2.vw.val[nrow(q2.vw.val),1]
q3.vw.inv

### Q3
vw.q3<-subset(vwport[,c(1,5:7)],
                + vwport$date >= as.Date("2013-07-01") &
                  + vwport$date <= as.Date("2013-09-30"))
vw.q3[c(1:3,nrow(vw.q3)),]
names(vw.q3)<-paste(c("date","AMZN","NVDA","IBM"))
vw.q3[c(1:3,nrow(vw.q3)),]
vw.q3$AMZN<-cumprod(vw.q3$AMZN)
vw.q3$NVDA<-cumprod(vw.q3$NVDA)
vw.q3$IBM<-cumprod(vw.q3$IBM)
vw.q3[c(1:3,nrow(vw.q3)),]
vw.q3$AMZN.idx<-(q3.vw.inv*q3.vw.wgt$AMZN.wgt)*vw.q3$AMZN
vw.q3$NVDA.idx<-(q3.vw.inv*q3.vw.wgt$NVDA.wgt)*vw.q3$NVDA
vw.q3$IBM.idx<-(q3.vw.inv*q3.vw.wgt$IBM.wgt)*vw.q3$IBM
vw.q3[c(1:3,nrow(vw.q3)),]
q3.vw.val<-data.frame(rowSums(vw.q3[,5:7]))
q3.vw.val[c(1:3,nrow(q3.vw.val)),]
names(q3.vw.val)<-paste("port.val")
q3.vw.val$date<-vw.q3$date
q3.vw.val[c(1:3,nrow(q3.vw.val)),]
q4.vw.inv<-q3.vw.val[nrow(q3.vw.val),1]
q4.vw.inv

### Q4
vw.q4<-subset(vwport[,c(1,5:7)],
                + vwport$date >= as.Date("2013-10-01") &
                  + vwport$date <= as.Date("2013-12-31"))
vw.q4[c(1:3,nrow(vw.q4)),]
names(vw.q4)<-paste(c("date","AMZN","NVDA","IBM"))
vw.q4[c(1:3,nrow(vw.q4)),]
vw.q4$AMZN<-cumprod(vw.q4$AMZN)
vw.q4$NVDA<-cumprod(vw.q4$NVDA)
vw.q4$IBM<-cumprod(vw.q4$IBM)
vw.q4[c(1:3,nrow(vw.q4)),]
vw.q4$AMZN.idx<-(q4.vw.inv*q4.vw.wgt$AMZN.wgt)*vw.q4$AMZN
vw.q4$NVDA.idx<-(q4.vw.inv*q4.vw.wgt$NVDA.wgt)*vw.q4$NVDA
vw.q4$IBM.idx<-(q4.vw.inv*q4.vw.wgt$IBM.wgt)*vw.q4$IBM
vw.q4[c(1:3,nrow(vw.q4)),]
q4.vw.val<-data.frame(rowSums(vw.q4[,5:7]))
q4.vw.val[c(1:3,nrow(q4.vw.val)),]
names(q4.vw.val)<-paste("port.val")
q4.vw.val$date<-vw.q4$date
q4.vw.val[c(1:3,nrow(q4.vw.val)),]

### Combine the four quarterly v-w portfolio gross return
vw.portval<-rbind(q1.vw.val,q2.vw.val,q3.vw.val,q4.vw.val)
### Output
vw.portval[c(1:3,nrow(vw.portval)),]

### merge two method portfolio by the sharing date object
port.val<-merge(vw.portval,ew.portval,by="date")
### Output
port.val[c(1:3,nrow(port.val)),]

### Rename with simple column names
names(port.val)<-paste(c("date","VW.cum","EW.cum"))
### Output
port.val[c(1:3,nrow(port.val)),]

### Create a 1x1 plot
par(mfrow=c(1,1))
### Calculate y range of VW and EW index returns
y.range<-range(port.val[,2:3])
### Output
y.range
plot(port.val$EW.cum,type="l",xlab="Date",ylab="Value of Investment",ylim=y.range,lty=1,main="Value of $1 Investment in Equal-Weighted and
+ Value-Weighted Portfolios of AMZN, YHOO, and IBM
+ December 31, 2012 - December 31, 2013")
lines(port.val$VW.cum,lty=2)
abline(h=1,lty=1)
legend("topleft",c("Equal-Weighted Portfolio","Value-Weighted Portfolio"),lty=c(1,2))

### Use xts(·) to convert to xts data type
### Only select VW and EW columns, ordered by date
port.xts<-xts(port.val[,2:3],order.by=port.val[,1])
### Output
port.xts[c(1:3,nrow(port.xts)),]

### Calculate Lagged datasets, which means one day earlier data variables
port.xts$Lag.VW<-Lag(port.xts$VW.cum,k=1)
port.xts$Lag.EW<-Lag(port.xts$EW.cum,k=1)
### Output
port.xts[c(1:3,nrow(port.xts)),]

### For each type of portfolio, daily return is calculate by gross index values divided by gross lagged index values
port.xts$VW.ret<-port.xts$VW.cum/port.xts$Lag.VW-1
port.xts$EW.ret<-port.xts$EW.cum/port.xts$Lag.EW-1
### Output
port.xts[c(1:3,nrow(port.xts)),]

### Only keep cumulative index value and daily return values
Port.Ret<-port.xts[,c(1,2,5,6)]
### Output
Port.Ret[c(1:3,nrow(Port.Ret)),]

### Since the index of Port.Ret is time format, data.frame(index(Port.Ret)) just gives the date columns
### in data.frame format, combined with original Port.Ret
csv.port<-cbind(data.frame(index(Port.Ret)),data.frame(Port.Ret))
### Give name of date for date object
names(csv.port)[1]<-paste("date")
### Output
csv.port[c(1:3,nrow(csv.port)),]

### Re-Assign the sequence series of numerical index of 1,2,3,...
### This is achieved by rownames(·) command and sequence(·) command
### Note we use double ":" in sequence(·) to pass the date sequence
rownames(csv.port)<-seq(1:nrow(csv.port))
### Output
csv.port[c(1:3,nrow(csv.port)),]

### Use write.csv command to save the csv file of above dataset
write.csv(csv.port,"Hypothetical Portfolio (Daily).csv")