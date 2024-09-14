install.packages('quantmod')
install.packages('xts')
install.packages("dplyr")
library(quantmod)
library(xts)


### Amazon Data
data.AMZN<-read.csv("AMZN Yahooo.csv",header=TRUE)
data.AMZN<-data.AMZN[,c(1,2,3,4,5,7,6)]
date<-as.Date(data.AMZN$Date,format="%Y-%m-%d")
data.AMZN<-cbind(date, data.AMZN[,-1])
data.AMZN<-data.AMZN[order(data.AMZN$date),]
data.AMZN<-xts(data.AMZN[,2:7],order.by=data.AMZN[,1])
names(data.AMZN)<-paste(c("AMZN.Open","AMZN.High","AMZN.Low","AMZN.Close","AMZN.Volume","AMZN.Adjusted"))
data.AMZN[c(1:3,nrow(data.AMZN)),]
AMZN.monthly<-to.monthly(data.AMZN)
AMZN.monthly[c(1:3,nrow(AMZN.monthly)),]
AMZN.monthly<-AMZN.monthly[,6]
AMZN.ret<-Delt(AMZN.monthly$data.AMZN.Adjusted)
names(AMZN.ret)<-paste("AMZN.ret")
AMZN.ret[c(1:3,nrow(AMZN.ret)),]
### Nvidia Data
data.NVDA<-read.csv("NVDA Yahooo.csv",header=TRUE)
data.NVDA<-data.NVDA[,c(1,2,3,4,5,7,6)]
date<-as.Date(data.NVDA$Date,format="%Y-%m-%d")
data.NVDA<-cbind(date, data.NVDA[,-1])
data.NVDA<-data.NVDA[order(data.NVDA$date),]
data.NVDA<-xts(data.NVDA[,2:7],order.by=data.NVDA[,1])
names(data.NVDA)<-paste(c("NVDA.Open","NVDA.High","NVDA.Low","NVDA.Close","NVDA.Volume","NVDA.Adjusted"))
data.NVDA[c(1:3,nrow(data.NVDA)),]
NVDA.monthly<-to.monthly(data.NVDA)
NVDA.monthly[c(1:3,nrow(NVDA.monthly)),]
NVDA.monthly<-NVDA.monthly[,6]
NVDA.ret<-Delt(NVDA.monthly$data.NVDA.Adjusted)
names(NVDA.ret)<-paste("NVDA.ret")
NVDA.ret[c(1:3,nrow(NVDA.ret)),]
### IBM Data
data.IBM<-read.csv("IBM Yahooo.csv",header=TRUE)
data.IBM<-data.IBM[,c(1,2,3,4,5,7,6)]
date<-as.Date(data.IBM$Date,format="%Y-%m-%d")
data.IBM<-cbind(date, data.IBM[,-1])
data.IBM<-data.IBM[order(data.IBM$date),]
data.IBM<-xts(data.IBM[,2:7],order.by=data.IBM[,1])
names(data.IBM)<-paste(c("IBM.Open","IBM.High","IBM.Low","IBM.Close","IBM.Volume","IBM.Adjusted"))
data.IBM[c(1:3,nrow(data.IBM)),]
IBM.monthly<-to.monthly(data.IBM)
IBM.monthly[c(1:3,nrow(IBM.monthly)),]
IBM.monthly<-IBM.monthly[,6]
IBM.ret<-Delt(IBM.monthly$data.IBM.Adjusted)
names(IBM.ret)<-paste("IBM.ret")
IBM.ret[c(1:3,nrow(IBM.ret)),]
port<-cbind(AMZN.ret,NVDA.ret,IBM.ret)
port[c(1:3,nrow(port)),]
port$port.ret<-rowMeans(port)
port[c(1:3,nrow(port)),]
port<-port[-1,4]
port[c(1:3,nrow(port)),]
csv.port<-cbind(data.frame(index(port)),data.frame(port))
names(csv.port)[1]<-paste("date")
csv.port[c(1:3,nrow(csv.port)),]
rownames(csv.port)<-seq(1,nrow(csv.port),by=1)
csv.port[c(1:3,nrow(csv.port)),]
write.csv(csv.port,"Hypothetical Portfolio (Monthly).csv")

### Read the monthly saved file of hypothesis
port<-read.csv("Hypothetical Portfolio (Monthly).csv")
### Output
port[c(1:3,nrow(port)),]

### Show current class of port$date
class(port$date)

### Change to year and month by as.yearmon command
### %b as the three-letter month and %Y as the four-digit year
port$date<-as.yearmon(as.character(port$date),"%b %Y")
### Output
port[c(1:3,nrow(port)),]
### Show the class again to see the convertion
class(port$date)

### Convert to data.frame type
port.df<-data.frame(port)
### Output
port.df[c(1:3,nrow(port.df)),]

### Use AAPL to model the market instead of using GSPC
data.AAPL<-read.csv("AAPL Yahooo.csv",header=TRUE)
data.AAPL<-data.AAPL[,c(1,2,3,4,5,7,6)]
date<-as.Date(data.AAPL$Date,format="%Y-%m-%d")
data.AAPL<-cbind(date, data.AAPL[,-1])
data.AAPL<-data.AAPL[order(data.AAPL$date),]
data.mkt<-xts(data.AAPL[,2:7],order.by=data.AAPL[,1])
names(data.mkt)[1:6]<-paste(c("AAPL.Open","AAPL.High","AAPL.Low","AAPL.Close","AAPL.Volume","AAPL.Adjusted"))
data.mkt[c(1:3,nrow(data.mkt)),]
### Convert to monthly data
mkt.monthly<-to.monthly(data.mkt)
### Output
mkt.monthly[c(1:3,nrow(mkt.monthly)),]
### Only Adjusted price is needed
mkt.monthly<-mkt.monthly[,6]
### Calculate return by delta function
mkt.ret<-Delt(mkt.monthly$data.mkt.Adjusted)
### Rename meaningfully
names(mkt.ret)<-paste("mkt.ret")
###  Output
mkt.ret[c(1:3,nrow(mkt.ret)),]
### Delete first row
mkt.ret<-mkt.ret[-1,]
### Output
mkt.ret[c(1:3,nrow(mkt.ret)),]
### Also convert to data.frame type
market.df<-data.frame(mkt.ret)
### See the head
head(market.df)

### Read the file of risk-free rate
rf<-read.csv("DGS3MO.csv",header=TRUE)
### See first three rows
rf[1:3,]

### create a Date object date to replace first column by converting
rf$date<-as.Date(rf$DATE,"%Y-%m-%d")
### Convert to character and then numeric values of 2nd column
rf$DGS3MO<-as.numeric(as.character(rf$DGS3MO))
### output
rf[c(1:3,nrow(rf)),]
### See details of data-set
str(rf)

### Change to xts and select data column and order by date object
rf<-xts(rf$DGS3MO,order.by=rf$date)
### Output
rf[1:3,]
### Rename
names(rf)<-paste("DGS3MO")
### Output
rf[1:3,]

### Convert to monthly data
rf.monthly<-to.monthly(rf)
### Show first three rows
rf.monthly[1:3,]

### Not to convert into scientific notation
options(scipen="100")
### convert annualized monthly data into monthly data
rf.monthly<-(1+rf.monthly[,1]/100)**(1/12)-1
### Output
rf.monthly[c(1:3,nrow(rf.monthly)),]

### Use subset to limit into the needed time duration
rf.sub<-subset(rf.monthly,index(rf.monthly) >= as.yearmon("1月 2011") &index(rf.monthly) <= as.yearmon("12月 2013"))
### Output
rf.sub[c(1:3,nrow(rf.sub)),]

### Combine market, risk-free data and portfolio return
combo<-cbind(market.df,data.frame(rf.sub),port.df$port.ret)
### Output
combo[c(1:3,nrow(combo)),]
### Rename meaningfully
names(combo)<-paste(c("mkt.ret","rf","port.ret"))
### Output
combo[c(1:3,nrow(combo)),]

### excess portfolio return
combo$exret<-combo$port.ret-combo$rf
### excess market return
combo$exmkt<-combo$mkt.ret-combo$rf
### output
combo[c(1:3,nrow(combo)),]

### at least 3 digits after first non-zero digit
options(digits=3)
### Use lm to run the regression
CAPM<-lm(combo$exret~combo$exmkt)
summary(CAPM)

### Left to Right, Up and Down
### 2nd and 8th numeric number of coefficients data-set
beta<-summary(CAPM)$coefficients[2]
beta
beta.pval<-summary(CAPM)$coefficients[8]
beta.pval
beta_pval = summary(CAPM)$coefficients[c(2,8)]
beta_pval

### Adjusted Beta reverse to market Beta by weighted-average with 1
adj.beta<-(2/3)*beta+(1/3)*1
### Output
adj.beta
### More effective digits
options(digits=7)

### Return to default
options(digits=3)
### Market Model Regression
reg<-lm(combo$port.ret~combo$mkt.ret)
### Summary
summary(reg)

### Adjusted Beta reversing to 1
beta.mktmod<-summary(reg)$coefficients[2]
beta.mktmod
adj.beta.mktmod<-(2/3)*beta.mktmod+(1/3)*1
adj.beta.mktmod
options(digits=7)


data.AMZN
data.AAPL<-xts(data.AAPL[,2:7],order.by=data.AAPL[,1])
names(data.AAPL)[1:6]<-paste(c("AAPL.Open","AAPL.High","AAPL.Low","AAPL.Close","AAPL.Volume","AAPL.Adjusted"))
data.AAPL

### difference of log return of two securities
rets<-diff(log(data.AMZN$AMZN.Adjusted))
rets$AAPL<-diff(log(data.mkt$AAPL.Adjusted))
### Rename
names(rets)[1]<-"AMZN"
### Output
rets[c(1:3,nrow(rets)),]
### Delete first row
rets<-rets[-1,]
### Output
rets[c(1:3,nrow(rets)),]

### Require zoo library
require(zoo)
### Use rollapply command to regression the coefficients
coeffs<-rollapply(rets,width=252,FUN=function(X){roll.reg=lm(AMZN~AAPL,data=as.data.frame(X))
                      + return(roll.reg$coef)},by.column=FALSE)

coeffs[c(1,251:253,nrow(coeffs)),]

### Delete all NA coeffs
coeffs<-na.omit(coeffs)
coeffs[c(1:3,nrow(coeffs)),]

### Delete only one data in 2011
coeffs<-coeffs[-1,]
### Rename meaningfully
names(coeffs)<-c("Alpha","Beta")
### Fewer digits in output
options(digits=3)
### Output
coeffs[c(1:3,nrow(coeffs)),]

### Plot the data
par(oma=c(0,0,4,0))
par(mfrow=c(2,1))
plot(x=index(coeffs),xlab="Date",y=coeffs$Alpha,ylab="alpha",type="l")
plot(x=index(coeffs),xlab="Date",y=coeffs$Beta,ylab="beta",type="l")
title(main="Amazon.com Inc. Alpha and Beta
+ Using Rolling 252-Day Windowns and
+ Daily Returns From 2012 to 2013",outer=TRUE)
par(mfrow=c(1,1))

### Check R memory
port[c(1:3,nrow(port)),]

### Import FF Data
FF.raw<-read.fwf(file="F-F_Research_Data_Factors.txt",widths=c(6,8,8,8,8),skip=4)
head(FF.raw)
tail(FF.raw)
### Delete not useful info
FF.raw<-FF.raw[-1175:-1277,]
### Rename
names(FF.raw)<-paste(c("text.date","RmxRf","SMB","HML","Rf"))
head(FF.raw)
tail(FF.raw)


### Delete first column
FF.raw<-FF.raw[,-1]
### Change type and percentage
FF.raw$RmxRf<-as.numeric(as.character(FF.raw$RmxRf))/100
FF.raw$Rf<-as.numeric(as.character(FF.raw$Rf))/100
FF.raw$SMB<-as.numeric(as.character(FF.raw$SMB))/100
FF.raw$HML<-as.numeric(as.character(FF.raw$HML))/100
### Create sequence of date object type Date
FF.raw$FF.date<-seq(as.Date("1926-07-01"),as.Date("2024-04-30"),by="months")
### Change to yearmon type
FF.raw$FF.date<-as.yearmon(FF.raw$FF.date,"%Y-%m-%d")
### Output
FF.raw[c(1:3,nrow(FF.raw)),]

### Subset date period
FF.data<-subset(FF.raw,FF.raw$FF.date>="2011-01-01" &FF.raw$FF.date<="2013-12-31")
### Output
FF.data[c(1:3,nrow(FF.data)),]

### Not too many digits
options(digits=3)
### Combine portfolio return and FF data
FF.data<-cbind(FF.data,data.frame(port))
### Output
FF.data[c(1:3,nrow(FF.data)),]

### define rownames
rownames(FF.data)<-seq(1,nrow(FF.data))
### Change date format
FF.data$date<-format(FF.data$date,"%Y-%m")
### excess return = portfolio return - risk-free asset value
FF.data$exret<-FF.data$port.ret-FF.data$Rf
### output
FF.data[c(1:3,nrow(FF.data)),]

### Run the regression of the equation
FF.reg<-lm(FF.data$exret~RmxRf+SMB+HML,data=FF.data)
### Get summary of regression
summary(FF.reg)

### CAPM regression
CAPM.reg<-lm(exret~RmxRf,data=FF.data)
### Summary
summary(CAPM.reg)

### Use rbind and cbind command to see beta, p and R-squared data for two factor models
betas<-rbind(cbind(summary(FF.reg)$coefficient[2],summary(FF.reg)$coefficient[14],summary(FF.reg)$adj.r.squared),cbind(summary(CAPM.reg)$coefficient[2],summary(CAPM.reg)$coefficient[8],summary(CAPM.reg)$adj.r.squared))
betas
colnames(betas)<-paste(c("Beta","p-Value","Adj. R-Squared"))
rownames(betas)<-paste(c("Fama-French","CAPM"))
betas
options(digits=7)

### NFLX Data from 2012-07-20 to 2013-07-23
data.NFLX<-read.csv("NFLX Yahoo (event study).csv",header=TRUE)
data.NFLX<-data.NFLX[,c(1,2,3,4,5,7,6)]
date<-as.Date(data.NFLX$Date,format="%Y-%m-%d")
data.NFLX<-cbind(date, data.NFLX[,-1])
data.NFLX<-data.NFLX[order(data.NFLX$date),]
library(xts)
firm<-xts(data.NFLX[,2:7],order.by=data.NFLX[,1])
firm[c(1:3,nrow(firm)),]
names(firm)<-paste(c("Firm.Open","Firm.High","Firm.Low","Firm.Close","Firm.Volume","Firm.Adjusted"))
firm[c(1:3,nrow(firm)),]

### SPY Market Index
data.SPY<-read.csv("SPY Yahoo (event study).csv",header=TRUE)
data.SPY<-data.SPY[,c(1,2,3,4,5,7,6)]
date<-as.Date(data.SPY$Date,format="%Y-%m-%d")
data.SPY<-cbind(date, data.SPY[,-1])
data.SPY<-data.SPY[order(data.SPY$date),]
market<-xts(data.SPY[,2:7],order.by=data.SPY[,1])
market[c(1:3,nrow(market)),]
names(market)<-paste(c("Mkt.Open","Mkt.High","Mkt.Low","Mkt.Close","Mkt.Volume","Mkt.Adjusted"))
market[c(1:3,nrow(market)),]

### combine firm and market adj price
data.all<-merge(firm[,6],market[,6])
data.all[c(1:3,nrow(data.all)),]

### log return = difference of log of adj price and then percentage it 
library(quantmod)
data.all$Firm.Ret<-diff(log(data.all$Firm.Adjusted))*100
data.all$Mkt.Ret<-diff(log(data.all$Mkt.Adjusted))*100
data.all[c(1:3,nrow(data.all)),]

### Only remain firm and market return
est.per<-data.all[c(-1,-nrow(data.all)),3:4]
est.per[c(1:3,nrow(est.per)),]
### Run the least square regression
mkt.model<-lm(est.per$Firm.Ret~est.per$Mkt.Ret)
summary(mkt.model)

### Summary of all statistics we need
### True firm and market return
event.window<-data.all[nrow(data.all),3:4]
### Output
event.window
### prediction = intercept + regression beta * true market return
event.window$Pred.Ret<-summary(mkt.model)$coefficients[1]+summary(mkt.model)$coefficients[2]*event.window$Mkt.Ret
### Abnormal return = True firm return - Predicted firm return
event.window$Ab.Ret<-event.window$Firm.Ret-event.window$Pred.Ret
### t statistic = abnormal return / sigma
event.window$tStat<-event.window$Ab.Ret/summary(mkt.model)$sigma
### P value calculated by above t statistic and degree of freedom = n - 2
event.window$pval<-2*(1-pt(abs(event.window$tStat),df=nrow(est.per)-2))
### Not too many digits in Output
options(digits=3)
### Output
event.window
### Return back
options(digits=7)

### Just draw the Adjusted Price of Netflix over estimation period
title1<-"Netflix Stock Price"
title2<-"July 20, 2012 to July 23, 2013"
plot(data.all$Firm.Adjusted,auto.grid=FALSE,xlab="Date",ylab="Price ($)",minor.ticks="auto",main=paste(title1,"\n",title2))
### Identify jumping date
subset(est.per,index(est.per)>="2013-01-01" & index(est.per)<="2013-01-31")

### Subset again to the new estimation period
est.per2<-subset(est.per,index(est.per)>="2013-01-28")
est.per2[c(1,3,nrow(est.per2)),]
nrow(est.per2)

### Run the regression again
mkt.model2<-lm(est.per2$Firm.Ret~est.per2$Mkt.Ret)
### Summary
summary(mkt.model2)

### Again, like before summary of statistics
### Summary of all statistics we need
### True firm and market return
event.window2<-data.all[nrow(data.all),3:4]
### Output
event.window2
### prediction = intercept + regression beta * true market return
event.window2$Pred.Ret<-summary(mkt.model2)$coefficients[1]+summary(mkt.model2)$coefficients[2]*event.window2$Mkt.Ret
### Abnormal return = True firm return - Predicted firm return
event.window2$Ab.Ret<-event.window2$Firm.Ret-event.window2$Pred.Ret
### t statistic = abnormal return / sigma
event.window2$tStat<-event.window2$Ab.Ret/summary(mkt.model2)$sigma
### P value calculated by above t statistic and degree of freedom = n - 2
event.window2$pval<-2*(1-pt(abs(event.window2$tStat),df=nrow(est.per2)-2))
### Not too many digits in Output
options(digits=3)
### Output
event.window2
### Return back
options(digits=7)