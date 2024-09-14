library(quantmod)
library(xts)

### Import data from hypothetical Portfolio (Monthly)
port<-read.csv("Hypothetical Portfolio (Monthly).csv",header=TRUE)
port[c(1:3,nrow(port)),]

### Import SPY index data as alternative data
SPY<-read.csv("SPY Yahoo.csv",header=TRUE)
SPY = SPY[,c(1,2,3,4,5,7,6)]
date<-as.Date(SPY$Date,format="%Y-%m-%d")
SPY<-cbind(date, SPY[,-1])
SPY<-SPY[order(SPY$date),]
library(xts)
SPY<-xts(SPY[,2:7],order.by=SPY[,1])
names(SPY)<-paste(c("SPY.Open","SPY.High","SPY.Low","SPY.Close","SPY.Volume","SPY.Adjusted"))
SPY[c(1:3,nrow(SPY)),]
SPY<-to.monthly(SPY)
SPY[c(1:3,nrow(SPY)),]
SPY<-SPY[,6]
library(quantmod)
altport.ret<-Delt(SPY$SPY.Adjusted)
names(altport.ret)<-paste("altport.ret")
altport.ret[c(1:3,nrow(altport.ret)),]

### Again, AAPL to substitute ^GSPC
AAPL<-read.csv("AAPL Yahooo.csv",header=TRUE)
AAPL = AAPL[,c(1,2,3,4,5,7,6)]
date<-as.Date(AAPL$Date,format="%Y-%m-%d")
AAPL<-cbind(date, AAPL[,-1])
AAPL<-AAPL[order(AAPL$date),]
AAPL<-xts(AAPL[,2:7],order.by=AAPL[,1])
names(AAPL)<-paste(c("AAPL.Open","AAPL.High","AAPL.Low","AAPL.Close","AAPL.Volume","AAPL.Adjusted"))
AAPL[c(1:3,nrow(AAPL)),]
AAPL<-to.monthly(AAPL)
AAPL[c(1:3,nrow(AAPL)),]
benchmark.ret<-Delt(AAPL$AAPL.Adjusted)
names(benchmark.ret)<-paste("bench.ret")
benchmark.ret[c(1:3,nrow(benchmark.ret)),]

### Fewer useful digits
options(digits=3)
### cbind all three returns of portfolio
port<-cbind(port[,-1],data.frame(altport.ret[-1,],data.frame(benchmark.ret[-1,])))
### Output
port[c(1:3,nrow(port)),]

### Re-index numerically
rownames(port)<-seq(1,nrow(port),1)
port[c(1:3,nrow(port)),]

### risk-free rate on 2013-12-31
Rf=0.0007
Rf

### Annual return = monthly return's mean * 12
annual.port.ret<-mean(port$port.ret)*12
annual.port.ret

### Annual sd = monthly sd * 12
annual.port.sd<-sd(port$port.ret)*sqrt(12)
annual.port.sd

### Sharpe ratio = (annual return - risk-free) / annual sd
Sharpe.port<-(annual.port.ret-Rf)/annual.port.sd
Sharpe.port

### Repeat above steps for Alternative Portfolio
annual.altport.ret<-mean(port$altport.ret)*12
annual.altport.ret
annual.altport.sd<-sd(port$altport.ret)*sqrt(12)
annual.altport.sd
Sharpe.altport<-(annual.altport.ret-Rf)/annual.altport.sd
Sharpe.altport

### Minimum Acceptable Return
mar<-0.015
mar
### Recall in R memory
annual.port.ret
annual.port.sd

### Roy SF ratio = (annual return - minimum acceptable return) / annual sd
Roy.SF.port<-(annual.port.ret-mar)/annual.port.sd
Roy.SF.port

### Repeat above steps for Alternative Portfolio
annual.altport.ret
annual.altport.sd
Roy.SF.altport<-(annual.altport.ret-mar)/annual.altport.sd
Roy.SF.altport

### recall annual port ret
annual.port.ret

### Not in scientific notation
options(scipen=100)
### Risk-free
Rf

### lm regression to find beta
port[c(1:3,nrow(port)),]
reg<-lm(port$port.ret~port$bench.ret)
port.beta<-summary(reg)$coefficients[2]
port.beta

### Tr ratio = (annual port ret - risk-free) / beta
Treynor.port<-(annual.port.ret-Rf)/port.beta
Treynor.port

### Again, repeat above steps for alternative portfolio
annual.altport.ret
Rf
reg.altport<-lm(port$altport.ret~port$bench.ret)
beta.altport<-summary(reg.altport)$coefficients[2]
beta.altport
Treynor.altport<-(annual.altport.ret-Rf)/beta.altport
Treynor.altport

### MAR
mar
### Period MAR
period.mar<-mar/12
period.mar

### if port ret < period MAR, dummy = 1, else 0.
port[c(1:3,nrow(port)),]
downside.port<-port
downside.port$dummy<-ifelse(port$port.ret<period.mar,1,0)
downside.port[c(1:3,nrow(downside.port)),]

### Select dummy ==1
downside.port<-subset(downside.port,downside.port$dummy==1)
downside.port[c(1:3,nrow(downside.port)),]

### downside deviation annually
dd.port<-sd(downside.port$port.ret)*sqrt(12)
dd.port

### Sortino ratio = ( annual ret - MAR ) / dd 
Sortino.port=(annual.port.ret-mar)/dd.port
Sortino.port

### Again for alternative portfolio
downside.altport<-port
downside.altport$dummy<-ifelse(port$altport.ret<period.mar,1,0)
downside.altport[c(1:3,nrow(downside.altport)),]
downside.altport<-subset(downside.altport,downside.altport$dummy==1)
downside.altport[c(1:3,nrow(downside.altport)),]
dd.altport<-sd(downside.altport$altport.ret)*sqrt(12)
dd.altport
Sortino.altport<-(annual.altport.ret-mar)/dd.altport
Sortino.altport

### Act ret port = port ret - bench ret
port[c(1:3,nrow(port)),]
Act.Ret.port<-port$port.ret-port$bench.ret
head(Act.Ret.port)

### alpha port = mean of act ret port * 12
alpha.port<-mean(Act.Ret.port)*12
alpha.port

### tract error = sd of active return of portfolio
tracking.error.port<-sd(Act.Ret.port)*sqrt(12)
tracking.error.port

### info ratio = alpha / tract error
IR.port<-alpha.port/tracking.error.port
IR.port

### Again, repeat for alternative portfolio against benchmark portfolio
Act.Ret.altport<-port$altport.ret-port$bench.ret
head(Act.Ret.altport)
alpha.altport<-mean(Act.Ret.altport)*12
alpha.altport
tracking.error.altport<-sd(Act.Ret.altport)*sqrt(12)
tracking.error.altport
IR.altport<-alpha.altport/tracking.error.altport
IR.altport


RARet<-rbind(cbind(Sharpe.port,Roy.SF.port,Treynor.port,Sortino.port,IR.port),
+             cbind(Sharpe.altport,Roy.SF.altport,Treynor.altport,Sortino.altport,IR.altport))
RARet

### Rename columns and rows meaningfully
colnames(RARet)<-paste(c("Sharpe","Roy SF","Treynor","Sortino","Info Ratio"))
rownames(RARet)<-paste(c("Portfolio","Alt Portfolio"))
### Output
RARet
### Return back setting of digits
options(digits=7)


