install.packages('quantmod')
install.packages('xts')
install.packages("dplyr")
library(quantmod)
library(xts)


### Let's begin!
### Read the .txt file and save as FF.raw data
### widths mean the width of each column which is fixed for better visualization
### skip = 4 means the first four lines are skipped
FF.raw<-read.fwf(file="F-F_Research_Data_Factors.txt",widths=c(6,8,8,8,8),skip=4)
### See the head of the raw data
head(FF.raw)
### See the tail of the raw data
tail(FF.raw)

### Remove the data rows from 977th row to 1066th row which we don't need
FF.raw<-FF.raw[-977:-1066,]
### Rename meaningfully
names(FF.raw)<-paste(c("text.date","RmxRf","SMB","HML","Rf"))
### See the head of the changed data
head(FF.raw)
### See the tail of the changed data
tail(FF.raw)

### See details of this data object
str(FF.raw)
### Remove first, third and fourth columns
FF.raw<-FF.raw[,c(-1,-3,-4)]
### change RmxRf column to be character then to be numeric and then divided by 100 to make percentage
FF.raw$RmxRf<-as.numeric(FF.raw$RmxRf)/100
### change Rf column to be character then to be numeric and then divided by 100 to make percentage
FF.raw$Rf<-as.numeric(FF.raw$Rf)/100
### Create a sequence of date object of monthly data
FF.raw$date<-seq(as.Date("1926-07-01"),as.Date("2007-10-31"),by="months")
### "yearmon" is a class for representing monthly data
FF.raw$date<-as.yearmon(FF.raw$date,"%Y-%m-%d")
### Output
FF.raw[c(1:3,nrow(FF.raw)),]
### See details of this data object after changes
str(FF.raw)

### Raw market return = (Raw market return - Raw risk-free rate return) + Raw risk-free rate return
FF.raw$Rm<-FF.raw$RmxRf+FF.raw$Rf
### Output
FF.raw[c(1:3,nrow(FF.raw)),]

### Use subset to limit time duration of 50 years beginning from 1957 to 2006
### Note that December, 1956 is imported as a placeholder
FF<-subset(FF.raw,FF.raw$date>="1956-12-01" & FF.raw$date<="2006-12-31")
### Output
FF[c(1:3,nrow(FF)),]

### Gross return of Rm should add one
FF$Gross.Rm<-1+FF$Rm
### Set first row of Gross Rm = 1 since 1956.12 is not calculated for return
FF$Gross.Rm[1]<-1
### Gross return of Rf should add one
FF$Gross.Rf<-1+FF$Rf
### Set first row of Gross Rf = 1 since 1956.12 is not calculated for return
FF$Gross.Rf[1]<-1
### Output
FF[c(1:3,nrow(FF)),]

### use cumprod command to calculate cumulative return for both Rm and Rf
FF$cum.Rm<-cumprod(FF$Gross.Rm)
FF$cum.Rf<-cumprod(FF$Gross.Rf)
### Output
FF[c(1:3,nrow(FF)),]

### Calculate y range of two cumulative returns
y.range<-range(FF$cum.Rm,FF$cum.Rf)
### Output y range
y.range
### Create two titles
title1<-"Stock vs. Bond Returns"
title2<-"1957 to 2006"
### Plot the data
plot(x=FF$date,y=FF$cum.Rm,type="l",xlab="Date",ylab="Value of $1 Investment ($)",ylim=y.range,main=paste(title1,"\n",title2))
lines(x=FF$date,y=FF$cum.Rf,lty=2)
legend("topleft",c("Stocks (2013 Ending Value: $156.34)","Bonds (2013 Ending Value: $13.60)"),lty=c(1,2))

### Calculate y range of two returns
y.range<-range(FF$Rm,FF$Rf)
### Output y range
y.range
### Create two titles
title1<-"Volatility of Stock vs. Bond Returns"
title2<-"1964 to 2013"
### Plot the data
plot(x=FF$date,FF$Rm,type="l",xlab="Date",ylab="Returns (%)",ylim=y.range,col="red",main=paste(title1,"\n",title2))
lines(x=FF$date,y=FF$Rf)
abline(h=0)
legend("topleft",c("Stocks","Bonds"),lty=c(1,2),col=c("red","black"))

### Read the csv file, which contains the header
data.AMZN<-read.csv("AMZN Yahooo.csv",header=TRUE)
### Change the order of the columns
data.AMZN<-data.AMZN[,c(1,2,3,4,5,7,6)]
### Change the Date column to be a "Date" object with a specific format by creating a new variable
date<-as.Date(data.AMZN$Date,format="%Y-%m-%d")
### combine the new variable with the remaining columns
data.AMZN<-cbind(date, data.AMZN[,-1])
### order the data-set by the date column
data.AMZN<-data.AMZN[order(data.AMZN$date),]
### Use the library of "xts"
library(xts)
### keep the first column as order and remaining column as content
data.AMZN<-xts(data.AMZN[,2:7],order.by=data.AMZN[,1])
### Rename meaningfully
names(data.AMZN)<-paste(c("AMZN.Open","AMZN.High","AMZN.Low","AMZN.Close","AMZN.Volume","AMZN.Adjusted"))
### Output
data.AMZN[c(1:3,nrow(data.AMZN)),]

### Extract the sixth column as raw return
AMZN.ret<-data.AMZN[,6]
### Open the library quantmod
library(quantmod)
### make a variable Return by taking delta of adjusted price in the AMZN.ret data-set
AMZN.ret$Return=Delt(AMZN.ret$AMZN.Adjusted)
### Eliminate first row and take only second column Return
AMZN.ret<-AMZN.ret[-1,2]
### Output
AMZN.ret[c(1:3,nrow(AMZN.ret)),]

### Use var command to output the total variance of the data-set in all time
AMZN.var.full<-var(AMZN.ret$Return)
AMZN.var.full
### Use sd command to output the total standard deviation of the data-set in all time
AMZN.sd.full<-sd(AMZN.ret$Return)
AMZN.sd.full

### Use subset command to limit the year to be 2011, with index to set the range of dates
AMZN.2011<-subset(AMZN.ret,index(AMZN.ret) >= "2011-01-01" &index(AMZN.ret) <= "2011-12-31")
### Output
AMZN.2011[c(1:3,nrow(AMZN.2011)),]

### Again use var and sd command to calculate variance and standard deviation, but for 2011.
AMZN.var.2011<-var(AMZN.2011)
### Output
AMZN.var.2011
AMZN.sd.2011<-sd(AMZN.2011)
### Output
AMZN.sd.2011

### Use subset command to limit the year to be 2012, with index to set the range of dates
AMZN.2012<-subset(AMZN.ret,index(AMZN.ret) >= "2012-01-01" & index(AMZN.ret) <= "2012-12-31")
### Output
AMZN.2012[c(1:3,nrow(AMZN.2012)),]

### Again use var and sd command to calculate variance and standard deviation, but for 2012.
AMZN.var.2012<-var(AMZN.2012)
### Output
AMZN.var.2012
AMZN.sd.2012<-sd(AMZN.2012)
### Output
AMZN.sd.2012

### Use subset command to limit the year to be 2013, with index to set the range of dates
AMZN.2013<-subset(AMZN.ret,index(AMZN.ret) >= "2013-01-01" &index(AMZN.ret) <= "2013-12-31")
### Output
AMZN.2013[c(1:3,nrow(AMZN.2013)),]

### Again use var and sd command to calculate variance and standard deviation, but for 2013.
AMZN.var.2013<-var(AMZN.2013)
### Output
AMZN.var.2013
AMZN.sd.2013<-sd(AMZN.2013)
### Output
AMZN.sd.2013

### Use mean command to calculate average return for four periods: full, 2011, 2012 and 2013
### And then output the numeric value
mean.ret.full<-mean(AMZN.ret)
mean.ret.full
mean.ret.2011<-mean(AMZN.2011)
mean.ret.2011
mean.ret.2012<-mean(AMZN.2012)
mean.ret.2012
mean.ret.2013<-mean(AMZN.2013)
mean.ret.2013

### Use rbind command to stack rows together by their sequence of order
AMZN.risk<-rbind(
  ### Use cbind to combine four vars
  + cbind(AMZN.var.full,AMZN.var.2011,
          + AMZN.var.2012,AMZN.var.2013),
  ### Use cbind to combine four sds
  + cbind(AMZN.sd.full,AMZN.sd.2011,
          + AMZN.sd.2012,AMZN.sd.2013),
  ### Use cbind to combine four means
  + cbind(mean.ret.full,mean.ret.2011,
          + mean.ret.2012,mean.ret.2013))
### Output
AMZN.risk

### At least 3 digits after first numeric "0" digit
options(digits=3)
### Rename rows
rownames(AMZN.risk)<-c("Variance","Std Dev","Mean")
### Rename columns
colnames(AMZN.risk)<-c("2011-2013","2011","2012","2013")
### Output
AMZN.risk
### Change to be default of digits
options(digits=7)

### At least 3 digits after first numeric "0" digit
options(digits=3)
### Re-assignment
annual.vol<-AMZN.risk
### annual variance = 252 * daily variance
annual.vol[1,]<-annual.vol[1,]*252
### annual standard deviation = sqrt(252) * daily deviation
annual.vol[2,]<-annual.vol[2,]*sqrt(252)
### annual mean = 252 * daily mean
annual.vol[3,]<-annual.vol[3,]*252
### Output
annual.vol
### Change to be default of digits
options(digits=7)

### Set the original weight of two securities
wgt.AMZN=.25
wgt.IBM=.75

### Import AMZN data
### Read the csv file, which contains the header
data.AMZN<-read.csv("AMZN Yahooo.csv",header=TRUE)
### Change the order of the columns
data.AMZN<-data.AMZN[,c(1,2,3,4,5,7,6)]
### Change the Date column to be a "Date" object with a specific format by creating a new variable
date<-as.Date(data.AMZN$Date,format="%Y-%m-%d")
### combine the new variable with the remaining columns
data.AMZN<-cbind(date, data.AMZN[,-1])
### order the data-set by the date column
data.AMZN<-data.AMZN[order(data.AMZN$date),]
### Use the library of "xts"
library(xts)
### keep the first column as order and remaining column as content
data.AMZN<-xts(data.AMZN[,2:7],order.by=data.AMZN[,1])
### Rename meaningfully
names(data.AMZN)<-paste(c("AMZN.Open","AMZN.High","AMZN.Low","AMZN.Close","AMZN.Volume","AMZN.Adjusted"))
### Output
data.AMZN[c(1:3,nrow(data.AMZN)),]

### Import IBM data
### Read the csv file, which contains the header
data.IBM<-read.csv("IBM Yahooo.csv",header=TRUE)
### Change the order of the columns
data.IBM<-data.IBM[,c(1,2,3,4,5,7,6)]
### Change the Date column to be a "Date" object with a specific format by creating a new variable
date<-as.Date(data.IBM$Date,format="%Y-%m-%d")
### combine the new variable with the remaining columns
data.IBM<-cbind(date, data.IBM[,-1])
### order the data-set by the date column
data.IBM<-data.IBM[order(data.IBM$date),]
### Use the library of "xts"
library(xts)
### keep the first column as order and remaining column as content
data.IBM<-xts(data.IBM[,2:7],order.by=data.IBM[,1])
### Rename meaningfully
names(data.IBM)<-paste(c("IBM.Open","IBM.High","IBM.Low","IBM.Close","IBM.Volume","IBM.Adjusted"))
### Output
data.IBM[c(1:3,nrow(data.IBM)),]

### Calculate return by delta of adjusted prices for AMZN
AMZN.Ret<-Delt(data.AMZN$AMZN.Adjusted)
### Output
AMZN.Ret[c(1:3,nrow(AMZN.Ret)),]

### Calculate return by delta of adjusted prices for IBM
IBM.Ret<-Delt(data.IBM$IBM.Adjusted)
### Output
IBM.Ret[c(1:3,nrow(IBM.Ret)),]

### Use cbind command to combine two return variables together
returns<-cbind(AMZN.Ret,IBM.Ret)
### Output
returns[c(1:3,nrow(returns)),]
### Rename meaningfully
names(returns)<-paste(c("AMZN.Ret","IBM.Ret"))
### Output
returns[c(1:3,nrow(returns)),]

### Eliminate first row for placeholder
returns<-returns[-1,]
### Output
returns[c(1:3,nrow(returns)),]

### Calculate standard deviation and covariance for AMZN and IBM annually
### And then output
sd.AMZN<-sd(returns$AMZN.Ret)*sqrt(252)
sd.AMZN
sd.IBM<-sd(returns$IBM.Ret)*sqrt(252)
sd.IBM
ret.cov<-cov(returns$AMZN.Ret,returns$IBM.Ret)*252
ret.cov

### Calculate the variance of the portfolio by using the formula
port.var<-wgt.AMZN**2*sd.AMZN**2 + wgt.IBM**2*sd.IBM**2 + 2*ret.cov*wgt.AMZN*wgt.IBM
### Output
port.var
### Use sqrt() command to calculate standard deviation of the portfolio
port.sd<-sqrt(port.var)
### Output
port.sd

### Make a series of weights
WGT.2asset<-c(0.25,0.75)
### Output
WGT.2asset
### Make a row matrix of the series by 1x2
WGT.2asset<-matrix(WGT.2asset,1)
### Output
WGT.2asset

### Make the transpose of the row matrix above
tWGT.2asset<-t(WGT.2asset)
### Output
tWGT.2asset

### Convert returns data-set to be matrix object
mat.Ret<-as.matrix(returns)
### Output
head(mat.Ret)

### Prevent scientific notations
options(scipen="100")
### Output the covariance matrix
cov(mat.Ret)

### multiply 252 to the VCOV matrix to annualize the matrix
VCOV.2asset<-cov(mat.Ret)*252
### Output
VCOV.2asset

### Make a sequence of multiplications together by the formula to calculate variances of the portfolio
mat.var2asset<-WGT.2asset %*% VCOV.2asset %*% tWGT.2asset
### Output
mat.var2asset

### Use sqrt command to convert variance to be standard deviation
mat.sd2asset<-sqrt(mat.var2asset)
### Output
mat.sd2asset

### Check if AMZN imported
data.AMZN[c(1:3,nrow(data.AMZN)),]
### Check if IBM imported
data.IBM[c(1:3,nrow(data.IBM)),]

# Import AAPL
data.AAPL<-read.csv("AAPL Yahooo.csv",header=TRUE)
data.AAPL<-data.AAPL[,c(1,2,3,4,5,7,6)]
date<-as.Date(data.AAPL$Date,format="%Y-%m-%d")
data.AAPL<-cbind(date, data.AAPL[,-1])
data.AAPL<-data.AAPL[order(data.AAPL$date),]
data.AAPL<-xts(data.AAPL[,2:7],order.by=data.AAPL[,1])
names(data.AAPL)<-paste(c("AAPL.Open","AAPL.High","AAPL.Low","AAPL.Close","AAPL.Volume","AAPL.Adjusted"))
data.AAPL[c(1:3,nrow(data.AAPL)),]

# Import NVDA
data.NVDA<-read.csv("NVDA Yahooo.csv",header=TRUE)
data.NVDA<-data.NVDA[,c(1,2,3,4,5,7,6)]
date<-as.Date(data.NVDA$Date,format="%Y-%m-%d")
data.NVDA<-cbind(date, data.NVDA[,-1])
data.NVDA<-data.NVDA[order(data.NVDA$date),]
data.NVDA<-xts(data.NVDA[,2:7],order.by=data.NVDA[,1])
names(data.NVDA)<-paste(c("NVDA.Open","NVDA.High","NVDA.Low","NVDA.Close","NVDA.Volume","NVDA.Adjusted"))
data.NVDA[c(1:3,nrow(data.NVDA)),]

### Extract the last column of each security by the command of merge
multi<-data.AMZN[,6]
multi<-merge(multi,data.NVDA[,6])
multi<-merge(multi,data.IBM[,6])
multi<-merge(multi,data.AAPL[,6])
### Output
multi[c(1:3,nrow(multi)),]

### Convert multi to be a matrix with nrow(multi) rows
mat.price<-matrix(multi,nrow(multi))
### Define the function of Delt
prc2ret<-function(x) Delt(x)
### Use apply command to apply delta
mat.ret<-apply(mat.price,2,function(x){prc2ret(c(x))})
### Output
mat.ret[1:4,]

### Delete first row for NAs
mat.ret<-mat.ret[-1,]
### Output
mat.ret[1:4,]
### Rename the columns meaningfully
colnames(mat.ret)<-c("AMZN","NVDA","IBM","AAPL")
### Output
mat.ret[1:4,]

### Calculate the covariance of the matrix of 4 returns
VCOV<-cov(mat.ret)
### Output
VCOV

### Annualization the covariance matrix
VCOV.annual<-252 * VCOV
### Output
VCOV.annual

### Create a series of data of the weight
wgt=c(.2,.2,.3,.3)
### Convert it into a row matrix
mat.wgt<-matrix(wgt,1)
### Output
mat.wgt

### Use t command to calculate transpose of matrix
tmat.wgt<-t(mat.wgt)
### Output
tmat.wgt

### Use formula to calculate variance of portfolio
port.var<-mat.wgt %*% VCOV.annual %*% tmat.wgt
### Output
port.var
port.var[1,1]

### Use sd command to calculate standard deviation
port.sd<-sqrt(port.var)
### [1,1] in output because value is outputed in 1x1 matrix
port.sd[1,1]

### Import the csv file of hypothetical portfolio data of daily returns
port.ret<-read.csv("Hypothetical Portfolio (Daily).csv")
### Output raw data
port.ret[c(1:3,nrow(port.ret)),]
### We only need value-weighted return here and then delete first row NA
port.ret<-port.ret$VW.ret[-1]
### See first five values
port.ret[1:5]

### Calculate average return of the portfolio
port.mean<-mean(port.ret)
### Output
port.mean
### Calculate standard deviation of the portfolio
port.risk<-sd(port.ret)
### Output
port.risk

### See what qnorm look like
qnorm(0.01)
### Significant level = 0.01
### calculate by formula
VaR01.Gaussian<- -(port.mean+port.risk*qnorm(0.01))*1317664
### Convert to the form containing ","
VaR01.Gaussian<-format(VaR01.Gaussian,big.mark=",")
### Output
VaR01.Gaussian

### Significant level = 0.05
### calculate by formula
VaR05.Gaussian<- -(port.mean+port.risk*qnorm(0.05))*1317664
### Convert to the form containing ","
VaR05.Gaussian<-format(VaR05.Gaussian,big.mark=",")
### Output
VaR05.Gaussian

### Output the AMZN data
data.AMZN[c(1:3,nrow(data.AMZN)),]
### Use delta function to calculate the return
AMZN.Ret<-Delt(data.AMZN$AMZN.Adjusted)
### Output the return data-set
AMZN.Ret[c(1:3,nrow(AMZN.Ret)),]
### Output the NVDA data
data.NVDA[c(1:3,nrow(data.NVDA)),]
### Use delta function to calculate the return
NVDA.Ret<-Delt(data.NVDA$NVDA.Adjusted)
### Output the return data-set
NVDA.Ret[c(1:3,nrow(NVDA.Ret)),]
### Output the IBM data
data.IBM[c(1:3,nrow(data.IBM)),]
### Use delta function to calculate the return
IBM.Ret<-Delt(data.IBM$IBM.Adjusted)
### Output the return data-set
IBM.Ret[c(1:3,nrow(IBM.Ret)),]

### combine the three returns and delete first rows of them
ret.data<-cbind(AMZN.Ret[-1,],NVDA.Ret[-1,],IBM.Ret[-1,])
### Output
ret.data[c(1:3,nrow(ret.data)),]
### Rename them meaningfully
names(ret.data)<-paste(c("AMZN.Ret","NVDA.Ret","IBM.Ret"))
### Output
ret.data[c(1:3,nrow(ret.data)),]

### Pre-set the index of ending value of three securities
last.idx<-c(0.4576889,0.5346666,0.3253085)*1000000
### Output
last.idx
### Portfolio ending value
port.val<-sum(last.idx)
### Output
port.val

### Use volume multiply return for all three securities and take sum for a specific date
sim.portPnL<-last.idx[1]*ret.data$AMZN.Ret + last.idx[2]*ret.data$NVDA.Ret + last.idx[3]*ret.data$IBM.Ret
### Output
sim.portPnL[c(1:3,nrow(sim.portPnL)),]
### Rename with Profit and Loss
names(sim.portPnL)<-paste("Port.PnL")
### Output
sim.portPnL[c(1:3,nrow(sim.portPnL)),]

### Use quantile to calculate VaR
### The first argument is the negative of the simulated portfolio P&L and the second argument is the 1 − α
### confidence level.
VaR01.Historical=quantile(-sim.portPnL$Port.PnL,0.99)
### Format to read easier
VaR01.Historical<-format(VaR01.Historical,big.mark=",")
### Output
VaR01.Historical
### Use quantile to calculate VaR
### The first argument is the negative of the simulated portfolio P&L and the second argument is the 1 − α
### confidence level.
VaR05.Historical=quantile(-sim.portPnL$Port.PnL,0.95)
### Format to read easier
VaR05.Historical<-format(VaR05.Historical,big.mark=",")
### Output
VaR05.Historical

### Use density function to get the details of density of PnL
ret.d=density(sim.portPnL$Port.PnL)
### Output
ret.d

### Plot the density
plot(ret.d,xlab="Profit & Loss",ylab="",yaxt="n",main="Density of Simulated Portfolio P&L Over Three Years
+ And 1% and 5% 1-Day Historical Value-at-Risk (VaR)")
### Line of estimate of 1%
abline(v=-quantile(-sim.portPnL$Port.PnL,0.99),col="red",lty=1)
### line of estimate of 5%
abline(v=-quantile(-sim.portPnL$Port.PnL,0.95),col="green",lty=2)

### Create a sequence x from minimum to maximum of density with 1000 elements
x<-seq(min(sim.portPnL$Port.PnL),max(sim.portPnL$Port.PnL),length=1000)
### See head of x
head(x)
### See tail of x
tail(x)

### Use dnorm command to assign values to the normal distribution
### three arguments: 1. x series of point intervals 2. mean. 3. standard deviation.
y<-dnorm(x,mean=mean(sim.portPnL$Port.PnL),sd=sd(sim.portPnL$Port.PnL))
### See head of y density
head(y)
### See tail of y density
tail(y)
### Add line of normal distribution by mean and standard deviation
lines(x,y,type="l",col="blue",lwd=1,lty=3)
### Add the legend
legend("topright",c("Simulated P&L Distribution","Normal Distribution","1% 1-Day VaR","5% 1-Day VaR"),col=c("black","blue","red","green"),lty=c(1,3,1,2))

### If transfer from 1-day to 10-day, multiply sqrt(10) by 1-day quantile
VaR01.10day<-quantile(-sim.portPnL$Port.PnL,0.99)*sqrt(10)
### Output
VaR01.10day

###   Gaussian Expected Shortfall
### alpha = .01
### Calculate by formula
ES01.Gaussian<-1317664*(port.mean+port.risk*(dnorm(qnorm(.01))/.01))
### Format easier to read
ES01.Gaussian<-format(ES01.Gaussian,big.mark=",")
### Output
ES01.Gaussian
### alpha = .05
### Calculate by formula
ES05.Gaussian<-1317664*(port.mean+port.risk*(dnorm(qnorm(.05))/.05))
### Format easier to read
ES05.Gaussian<-format(ES05.Gaussian,big.mark=",")
### Output
ES05.Gaussian

### Calculate limit of VaR historical lost of alpha = 0.01
VaR01.hist=-quantile(-sim.portPnL$Port.PnL,0.99)
### Output
VaR01.hist
### Calculate limit of VaR historical lost of alpha = 0.05
VaR05.hist=-quantile(-sim.portPnL$Port.PnL,0.95)
### Output
VaR05.hist

### Extract the PnL from previous portfolio
ES.PnL<-sim.portPnL$Port.PnL
### Output
ES.PnL[c(1:3,nrow(ES.PnL)),]
### If PnL < VaR01, 1 ; else, 0
ES.PnL$dummy01<-ifelse(ES.PnL$Port.PnL<VaR01.hist,1,0)
### If PnL < VaR05, 1 ; else, 0
ES.PnL$dummy05<-ifelse(ES.PnL$Port.PnL<VaR05.hist,1,0)
### Output
ES.PnL[c(1:3,nrow(ES.PnL)),]

### Create ES of .01 by subset of PnL < VaR01
shortfall01<-subset(ES.PnL,ES.PnL$dummy01==1)
### See head
head(shortfall01)
### Create ES of .05 by subset of PnL < VaR05
shortfall05<-subset(ES.PnL,ES.PnL$dummy05==1)
### See head
head(shortfall05)

### Mean of .01 ES
avg.ES01<--mean(shortfall01$Port.PnL)
### Output
avg.ES01
### Format to read
ES01.Historical<-format(avg.ES01,big.mark=",")
### Output
ES01.Historical
### Mean of .05 ES
avg.ES05<--mean(shortfall05$Port.PnL)
### Output
avg.ES05
### Format to read
ES05.Historical<-format(avg.ES05,big.mark=",")
### Output
ES05.Historical

### rbind to stack together, cbind to combine all 4 risk by 01 and 05
VaR.ES.Combined<-data.frame(rbind(cbind(VaR01.Historical,ES01.Historical[1],VaR01.Gaussian,ES01.Gaussian[1]),cbind(VaR05.Historical,ES05.Historical[1],VaR05.Gaussian,ES05.Gaussian[1])))
### Output
VaR.ES.Combined

### Rename the columns meaningfully
names(VaR.ES.Combined)<-paste(c("VaR Historical","ES Historical","VaR Gaussian","ES Gaussian"))
### Rename row names meaningfully
rownames(VaR.ES.Combined)<-paste(c("1% 1-Day","5% 1-Day"))
VaR.ES.Combined

### Read the AMZN data
data.AMZN<-read.csv("AMZN Yahooo.csv",header=TRUE)
### Change the order of columns
data.AMZN<-data.AMZN[,c(1,2,3,4,5,7,6)]
### Create a "Date" object from the AMZN$Date
date<-as.Date(data.AMZN$Date,format="%Y-%m-%d")
### Replace the data-set with date variable and data-set without first column
data.AMZN<-cbind(date, data.AMZN[,-1])
### rows are ordered by created date
data.AMZN<-data.AMZN[order(data.AMZN$date),]
### Open xts library
library(xts)
### convert data-set to be xts object, order from 1st column and content with remaining
data.AMZN<-xts(data.AMZN[,2:7],order.by=data.AMZN[,1])
### Rename meaningfully
names(data.AMZN)<-paste(c("AMZN.Open","AMZN.High","AMZN.Low","AMZN.Close","AMZN.Volume","AMZN.Adjusted"))
### Output
data.AMZN[c(1:3,nrow(data.AMZN)),]

### Keep only high and low price for Parkinson and delete first row
parkinson<-data.AMZN[-1,2:3]
### Output
parkinson[c(1:3,nrow(parkinson)),]

### Take log of high/low
parkinson$log.hi.low<-log(parkinson$AMZN.High/parkinson$AMZN.Low)
### Square
parkinson$log.square<-(parkinson$log.hi.low)**2
### Output
parkinson[c(1:3,nrow(parkinson)),]

### Take the sum of log.square
parkinson.sum<-sum(parkinson$log.square)
### Output
parkinson.sum

### Use formula to calculate Parkinson daily risk
parkinson.vol<-sqrt(1/(4*nrow(parkinson)*log(2))*parkinson.sum)
### Output
parkinson.vol

### output by annualization of multiplying sqrt(252)
annual.parkinson.vol<-parkinson.vol*sqrt(252)
### Output
annual.parkinson.vol

### Garman and Klass
### Delete first row and keep only open,hign,low,close
garman.klass<-data.AMZN[-1,1:4]
### Output
garman.klass[c(1:3,nrow(garman.klass)),]

### First term in garman.klass in square root by importing parkinson.sum
garman.klass.one<-(1/(2*nrow(garman.klass)))*parkinson.sum
### Output
garman.klass.one

### Use the formula to calculate second term
### Note that Close and Open Price is a series of numbers, so sum function must be applied to turn to be a value
garman.klass.two<-((2*log(2)-1)/nrow(garman.klass))*sum(log(garman.klass$AMZN.Close/garman.klass$AMZN.Open)**2)
### Output
garman.klass.two

### Square the minus of two terms by 1st - 2nd
garman.klass.vol<-sqrt(garman.klass.one-garman.klass.two)
### Output final daily value
garman.klass.vol

### Annualize the risk by multiply daily risk by square root of 252 (marketing dates)
annual.garman.klass.vol<-garman.klass.vol*sqrt(252)
### Output annual risk value
annual.garman.klass.vol

### Rogers, Satchell and Yoon
### Delete first row and contains only open,high,low,close price
rsy.vol<-data.AMZN[-1,1:4]
### Output
rsy.vol[c(1:3,nrow(rsy.vol)),]

### 1st = high/close
rsy.one<-log(rsy.vol$AMZN.High/rsy.vol$AMZN.Close)
### Output
rsy.one[c(1:3,nrow(rsy.one)),]
### 2nd = high/open
rsy.two<-log(rsy.vol$AMZN.High/rsy.vol$AMZN.Open)
### Output
rsy.two[c(1:3,nrow(rsy.two)),]
### 1st * 2nd = 1,2 term
rsy.one.two<-rsy.one*rsy.two
### Output
rsy.one.two[c(1:3,nrow(rsy.one.two)),]

### 3rd = low/close
rsy.three<-log(rsy.vol$AMZN.Low/rsy.vol$AMZN.Close)
### Output
rsy.three[c(1:3,nrow(rsy.three)),]
### 4th = low/open
rsy.four<-log(rsy.vol$AMZN.Low/rsy.vol$AMZN.Open)
### Output
rsy.four[c(1:3,nrow(rsy.four)),]
### 3rd * 4th = 3,4 term
rsy.three.four<-rsy.three*rsy.four
### Output
rsy.three.four[c(1:3,nrow(rsy.three.four)),]

### Use formula
rsy.vol<-sqrt((1/nrow(rsy.vol))*sum(rsy.one.two+rsy.three.four))
### Output
rsy.vol

### Annualize the risk by multiplying square root of 252 which is marketing dates
annual.rsy.vol<-rsy.vol*sqrt(252)
### Output
annual.rsy.vol

### Yang and Zhang
### Only keep the 4 prices
yz.vol<-data.AMZN[,1:4]
### Use lag command to calculate the close price yesterday
yz.vol$Lag.Close<-Lag(yz.vol$AMZN.Close,k=1)
### Output
yz.vol[c(1:3,nrow(yz.vol)),]

### Delete the first row data as placeholder
yz.vol<-yz.vol[-1,]
### Output
yz.vol[c(1:3,nrow(yz.vol)),]

### calculate mean log open/close
yz.one.mean<-mean(log(yz.vol$AMZN.Open/yz.vol$Lag.Close))
### Output
yz.one.mean
### Insert mean into the formula
yz.one<-1/(nrow(yz.vol)-1)*sum((log(yz.vol$AMZN.Open/yz.vol$Lag.Close)-yz.one.mean)**2)
### Output 1st term
yz.one

### Calculate the mean in the 2nd term
yz.two.mean<-mean(log(yz.vol$AMZN.Close/yz.vol$AMZN.Open))
### Output
yz.two.mean
### Insert mean into the formula
yz.two<-1/(nrow(yz.vol)-1)*sum((
    + log(yz.vol$AMZN.Close/yz.vol$AMZN.Open)-yz.two.mean)**2)
### Output
yz.two

### alpha = 1.34 is suggested by Yang and Zhang
k=0.34/(1.34+(nrow(yz.vol)+1)/(nrow(yz.vol)-1))
### Output the value of k
k

### Use the formula above to calculate the volatility and convert to annual data by multiplication of sqrt(252)
annual.yz.vol<-sqrt(yz.one+k*yz.two+(1-k)*rsy.vol**2)*sqrt(252)
### Output
annual.yz.vol

### Only keep Adjusted Price
AMZN.ret<-data.AMZN[,6]
### Open quantmod library
library(quantmod)
### Use Delt command to calculate daily return
AMZN.ret$Return=Delt(AMZN.ret$AMZN.Adjusted)
### Delete first row and keep only Return column
AMZN.ret<-AMZN.ret[-1,2]
### Output
AMZN.ret[c(1:3,nrow(AMZN.ret)),]

### Construct a new data-set
cl2cl.ret<-AMZN.ret
### log return = log(gross return)
cl2cl.ret$logret<-log(1+cl2cl.ret$Return)
### Output
cl2cl.ret[c(1:3,nrow(cl2cl.ret)),]

### Calculate standard deviation of log return
cl2cl.vol<-sd(cl2cl.ret$logret)
### Output
cl2cl.vol

### Annualize sd by multiplying square root of 252 of number of marketing dates
annual.cl2cl.vol<-cl2cl.vol*sqrt(252)
### Output
annual.cl2cl.vol

### Combine all volatility measurement indices above with close-to-close volatility
vol.measures<-rbind(annual.cl2cl.vol,annual.parkinson.vol,
                      + annual.garman.klass.vol,annual.rsy.vol,annual.yz.vol)
### Give names to all kinds of volatility
rownames(vol.measures)<-c("Close-to-Close","Parkinson","Garman-Klass","Rogers et al","Yang-Zhang")
### Point out volatility for comparison
colnames(vol.measures)<-c("Volatility")
### Output
vol.measures