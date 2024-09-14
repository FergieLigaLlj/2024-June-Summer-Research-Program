library(quantmod)
library(xts)
us.rgdp<-read.csv("USRGDP IMF WEO.csv",header=FALSE)
us.rgdp[c(1:3,nrow(us.rgdp)),]


colnames(us.rgdp)<-paste(c("Year","Value"))
us.rgdp<-data.frame(us.rgdp)
us.rgdp[c(1:3,nrow(us.rgdp)),]


us.rgdp$historical<-ifelse(us.rgdp$Year<=2012,us.rgdp$Value,0)
us.rgdp[c(1:3,nrow(us.rgdp)),]
us.rgdp$projected<-ifelse(us.rgdp$Year>2012,us.rgdp$Value,0)
us.rgdp[c(1:3,nrow(us.rgdp)),]


us.rgdp<-us.rgdp[,3:4]
us.rgdp[c(1:3,nrow(us.rgdp)),]
us.mat<-as.matrix(us.rgdp)
t.us<-t(us.mat)
head(t.us)


xlabel=seq(1980,2018,by=1)
even.years<-xlabel %% 2==0
even.years
years.even<-cbind(data.frame(xlabel),data.frame(even.years))
head(years.even)
years.even$Year<-ifelse(years.even$even.years=="TRUE",xlabel," ")
xlabel.even<-years.even[,3]
head(xlabel.even)


range(us.rgdp)
y.range<-c(-4,8)
y.range
barplot(t.us,col=c("black","gray60"),ylab="Real GDP Growth (%)",ylim=y.range,names.arg=xlabel.even,las=2,
main="United States Real GDP Growth
+ Historical (1980-212) and Forecasted (2013-2018)")
legend("topright",c("Historical","Forecasted"),col=c("black","gray60"),pch=c(15,15))


US.unempl<-read.csv("UNRATE FRED.csv",header = TRUE)
US.unempl$date<-as.Date(US.unempl$DATE,"%Y-%m-%d")
US.unempl$UNRATE<-as.numeric(as.character(US.unempl$UNRATE))
US.unempl<-xts(US.unempl$UNRATE,order.by=US.unempl$date)
names(US.unempl)<-paste("UNRATE")
US.unempl[1:3,]


US.unempl<-subset(US.unempl,index(US.unempl)>="1964-01-01" &index(US.unempl)<="2013-12-31")
US.unempl[1:3,]


lt.avg<-mean(US.unempl$UNRATE)
lt.avg


plot(x=index(US.unempl),xlab="Date (Quarters)",y=US.unempl$UNRATE,ylab="Unemployment Rate (%)",ylim=c(2,12),type="l",
main="US Unemployment Rate
+ From 1964 to 2013")
abline(h=lt.avg,lty=2)
text(as.Date("2001-01-01"),7.4,"Long-Term")
text(as.Date("2001-01-01"),7,"Avg. = 6.1%")
arrows(x0=as.Date("2001-01-01"),y0=6.9,x1=as.Date("2001-01-01"),y1=6.2,code=2,length=0.10)
points(as.Date("1982-11-01"),10.8,pch=16)
text(as.Date("1992-01-01"),11.5,"November 1982")
arrows(x0=as.Date("1992-01-01"),y0=11.3,x1=as.Date("1983-07-01"),y1=10.9,code=2,length=0.10)
points(as.Date("2009-10-01"),10,pch=16)
text(as.Date("2010-01-01"),11,"October 2009")
arrows(x0=as.Date("2009-10-01"), y0=10.8, x1=as.Date("2009-10-01"), y1=10.1, code=2, length=0.10)


US.CPI<-read.csv("CPIAUCNS FRED.csv",header = TRUE)
US.CPI$date<-as.Date(US.CPI$DATE,"%Y-%m-%d")
US.CPI$CPIAUCNS<-as.numeric(as.character(US.CPI$CPIAUCNS))
US.CPI<-xts(US.CPI$CPIAUCNS,order.by=US.CPI$date)
names(US.CPI)<-paste("CPIAUCNS")
US.CPI[1:3,]

US.Lag12<-Lag(US.CPI$CPIAUCNS,k=12)
US.Lag12[1:20,]


US.CPI<-merge(US.CPI,US.Lag12)
names(US.CPI)<-paste(c("us.cpi","lag.cpi"))
US.CPI[10:15,]

US.CPI$inflation<-(US.CPI$us.cpi/US.CPI$lag.cpi-1)*100
US.CPI[10:15,]

US.CPI<-subset(US.CPI[,3],index(US.CPI)>="1964-01-01" &index(US.CPI)<="2013-12-01")
US.CPI[c(1:3,nrow(US.CPI)),]

plot(x=index(US.CPI),y=US.CPI$inflation,xlab="Date", ylab="Inflation Rate (%)", type="l", main="US Inflation Rates From 1964 to 2013
+ Based on Year Over Year Changes in CPI")

shade<-par("usr")
shade
rect(as.Date("2007-12-01"),shade[2],
       + as.Date("2009-06-01"),shade[3],col="gray60",lty=0)
rect(as.Date("2001-03-01"),shade[2],
       + as.Date("2001-11-01"),shade[3],col="gray60",lty=0)
rect(as.Date("1990-07-01"),shade[2],
       + as.Date("1991-03-01"),shade[3],col="gray60",lty=0)
rect(as.Date("1981-07-01"),shade[2],
       + as.Date("1982-11-01"),shade[3],col="gray60",lty=0)
rect(as.Date("1980-01-01"),shade[2],
       + as.Date("1980-07-01"),shade[3],col="gray60",lty=0)
rect(as.Date("1973-11-01"),shade[2],
       + as.Date("1975-03-01"),shade[3],col="gray60",lty=0)
rect(as.Date("1969-12-01"),shade[2],
       + as.Date("1970-11-01"),shade[3],col="gray60",lty=0)
rect(as.Date("1960-04-01"),shade[2],
       + as.Date("1961-02-01"),shade[3],col="gray60",lty=0)
box(which="plot",lty=1)
lines(x=index(US.CPI),y=US.CPI$inflation)
abline(h=0)


### US Treasury
t3mo<-read.csv("DGS3MO.csv",header = TRUE)
t3mo$date<-as.Date(t3mo$DATE,"%Y-%m-%d")
t3mo$DGS3MO<-as.numeric(as.character(t3mo$DGS3MO))
t3mo<-xts(t3mo$DGS3MO,order.by=t3mo$date)
names(t3mo)<-paste("DGS3MO")
t3mo[1:3,]
t6mo<-read.csv("DGS6MO.csv",header = TRUE)
t6mo$date<-as.Date(t6mo$DATE,"%Y-%m-%d")
t6mo$DGS6MO<-as.numeric(as.character(t6mo$DGS6MO))
t6mo<-xts(t6mo$DGS6MO,order.by=t6mo$date)
names(t6mo)<-paste("DGS6MO")
t6mo[1:3,]

t1yr<-read.csv("DGS1.csv",header = TRUE)
t1yr$date<-as.Date(t1yr$DATE,"%Y-%m-%d")
t1yr$DGS1<-as.numeric(as.character(t1yr$DGS1))
t1yr<-xts(t1yr$DGS1,order.by=t1yr$date)
names(t1yr)<-paste("DGS1")
t1yr[1:3,]

t2yr<-read.csv("DGS2.csv",header = TRUE)
t2yr$date<-as.Date(t2yr$DATE,"%Y-%m-%d")
t2yr$DGS2<-as.numeric(as.character(t2yr$DGS2))
t2yr<-xts(t2yr$DGS2,order.by=t2yr$date)
names(t2yr)<-paste("DGS2")
t2yr[1:3,]

t3yr<-read.csv("DGS3.csv",header = TRUE)
t3yr$date<-as.Date(t3yr$DATE,"%Y-%m-%d")
t3yr$DGS3<-as.numeric(as.character(t3yr$DGS3))
t3yr<-xts(t3yr$DGS3,order.by=t3yr$date)
names(t3yr)<-paste("DGS3")
t3yr[1:3,]

t5yr<-read.csv("DGS5.csv",header = TRUE)
t5yr$date<-as.Date(t5yr$DATE,"%Y-%m-%d")
t5yr$DGS5<-as.numeric(as.character(t5yr$DGS5))
t5yr<-xts(t5yr$DGS5,order.by=t5yr$date)
names(t5yr)<-paste("DGS5")
t5yr[1:3,]

t7yr<-read.csv("DGS7.csv",header = TRUE)
t7yr$date<-as.Date(t7yr$DATE,"%Y-%m-%d")
t7yr$DGS7<-as.numeric(as.character(t7yr$DGS7))
t7yr<-xts(t7yr$DGS7,order.by=t7yr$date)
names(t7yr)<-paste("DGS7")
t7yr[1:3,]

t10yr<-read.csv("DGS10.csv",header = TRUE)
t10yr$date<-as.Date(t10yr$DATE,"%Y-%m-%d")
t10yr$DGS10<-as.numeric(as.character(t10yr$DGS10))
t10yr<-xts(t10yr$DGS10,order.by=t10yr$date)
names(t10yr)<-paste("DGS10")
t10yr[1:3,]

t20yr<-read.csv("DGS20.csv",header = TRUE)
t20yr$date<-as.Date(t20yr$DATE,"%Y-%m-%d")
t20yr$DGS20<-as.numeric(as.character(t20yr$DGS20))
t20yr<-xts(t20yr$DGS20,order.by=t20yr$date)
names(t20yr)<-paste("DGS20")
t20yr[1:3,]

t30yr<-read.csv("DGS30.csv",header = TRUE)
t30yr$date<-as.Date(t30yr$DATE,"%Y-%m-%d")
t30yr$DGS30<-as.numeric(as.character(t30yr$DGS30))
t30yr<-xts(t30yr$DGS30,order.by=t30yr$date)
names(t30yr)<-paste("DGS30")
t30yr[1:3,]


treasury<-t3mo
treasury<-merge(treasury,t6mo)
treasury<-merge(treasury,t1yr)
treasury<-merge(treasury,t2yr)
treasury<-merge(treasury,t3yr)
treasury<-merge(treasury,t5yr)
treasury<-merge(treasury,t7yr)
treasury<-merge(treasury,t10yr)
treasury<-merge(treasury,t20yr)
treasury<-merge(treasury,t30yr)
treasury[1:3,]


extreme<-subset(treasury,index(treasury) >= "2020-01-01" &index(treasury) <= "2024-6-26")
extreme<-na.omit(extreme[,c(1,8,10)])
extreme[c(1:3,nrow(extreme)),]

### extreme inverted
extreme$sign.diff<-extreme$DGS30-extreme$DGS3MO
extreme$inverted<-ifelse(extreme$sign.diff==min(extreme$sign.diff),1,0)
inverted<-subset(extreme,extreme$inverted==1)
inverted
### extreme upward
extreme$upward<-ifelse(extreme$sign.diff==max(extreme$sign.diff),1,0)
upward<-subset(extreme,extreme$upward==1)
upward


extreme$abs.diff<-abs(extreme$DGS30-extreme$DGS3MO)
extreme$flat<-ifelse(extreme$abs.diff==min(extreme$abs.diff),1,0)
flat<-subset(extreme,extreme$flat==1)
flat$abs.diff2<-abs(flat$DGS30-flat$DGS10)
flat$flat2<-ifelse(flat$abs.diff2==min(flat$abs.diff2),1,0)
flat[,c(-4:-6)]


invert.date<-as.Date("2023-06-26")
normal.date<-as.Date("2021-03-18")
flat.date<-as.Date("2022-10-31")
current.date<-as.Date("2020-01-08")

tyld.curve<-subset(treasury,
                       + index(treasury) == invert.date |
                         + index(treasury) == flat.date |
                         + index(treasury) == normal.date |
                         + index(treasury) == current.date)
tyld.curve


class(tyld.curve)
tyld.curve<-t(tyld.curve)
tyld.curve
class(tyld.curve)
rownames(tyld.curve)<-paste(c(0.25,0.5,1,2,3,5,7,10,20,30))
colnames(tyld.curve)<-paste(c("inverted","flat","normal","current"))
tyld.curve


TTM<-c(0.25,0.5,1,2,3,5,7,10,20,30)
TTM
y.range<-range(tyld.curve)
y.range
plot(x=TTM,y=tyld.curve[,1],type="o",ylim=y.range,xlab="Time to Maturity",ylab="Yield (Percent)",col="gray60",lwd=2,
main="Shapes of the Treasury Yield Curve")
lines(x=TTM,y=tyld.curve[,2],type="o",lty=3,col="black")
lines(x=TTM,y=tyld.curve[,3],type="o",col="black",lwd=3)
lines(x=TTM,y=tyld.curve[,4],type="o",lty=3,col="gray40",lwd=2)
legend("bottomright",c("Inverted (11/24/2000)", "Flat (08/26/2006)","Normal (01/11/2010)", "December 31, 2013"),col=c("gray60","black","black","gray40"), lty=c(1,3,1,3), lwd=c(2,1,3,2))



slope<-t3mo
slope<-merge(t3mo,t30yr)
slope[1:3,]
slope<-subset(slope,index(slope)>= "2020-01-01" &index(slope)<= "2024-06-26")
slope[c(1:3,nrow(slope)),]
slope<-na.omit(slope)
slope[c(1:3,nrow(slope)),]
y.range<-range(slope)
y.range
plot(x=index(slope),xlab="Date",y=slope$DGS30,ylab="Yield (Percent)",ylim=y.range,type="l",col="black",lty=1,lwd=1,main="Yields on 3-Month and 30-Year Treasuries (2020-2024/06/24)")
shade<-par("usr")
shade
rect(as.Date("2020-12-01"),shade[2],as.Date("2024-06-01"),shade[3],col="gray60",lty=0)
box(which="plot",lty=1)
lines(x=index(slope),y=slope$DGS30)
lines(x=index(slope),y=slope$DGS3MO,lty=2,col="black")
legend("topright",c("3-Month Treasury","30-Year Treasury"),lty=c(2,1))

slope$slope<-(slope$DGS30-slope$DGS3MO)*100

plot(x=index(slope),xlab="Date",y=slope$slope, ylab="Spread (bps)",type="l",lty=1,lwd=1,main="Slope of the US Treasury Yield Curve from 2007 to 2013
+ Based on the Spread Between the
+ 3-Month and 30-Year Treasury Yields")
shade<-par("usr")
shade
rect(as.Date("2020-12-01"),shade[2],as.Date("2024-06-26"),shade[3],col="gray60",lty=0)
box(which="plot",lty=1)
abline(h=0,lty=1)
lines(x=index(slope),y=slope$slope,lty=1,lwd=1)