getwd()
setwd("/Users/annie/TSA/Final Presentation")
data=read.csv("netflix.csv")
library(astsa)
library(TTR)
library(xts)
library(quantmod)
library(ggplot2)
library(MASS)
library(forecast)
library(tseries)
#####1. EDA#####
head(data)
summary(data)
date=as.Date(data$Date)
###轉換資料
data["Date_time"]=date
#基本圖表 #到2022-12-20
NFLX<-getSymbols("NFLX",from="2017-12-29",auto.assign = FALSE,src = "yahoo")
head(NFLX) 
dim(NFLX)
#basic stat
basicStats(NFLX$NFLX.Close[2:1253])
#畫圖
par(mfrow=c(3,2))
plot(NFLX$NFLX.Open)
plot(NFLX$NFLX.High)
plot(NFLX$NFLX.Low)
plot(NFLX$NFLX.Close)
plot(NFLX$NFLX.Volume)
plot(NFLX$NFLX.Adjusted)

#close price
par(mfrow=c(1,1))
Netflix_Close=NFLX$NFLX.Close[2:1260]
plot(Netflix_Close,col=1)
points(c(978,692),col=2)
which.max(close)
plot(close,col=1,type="l")
points(1098,167,col=2)
which.min(close)
Netflix_Close[978]
#ADF test
par(mfrow=c(1,2))
adf.test(Netflix_Close) #non-stationary
acf(Netflix_Close,main="Neflix Close Price ACF Plot",lag.max = 20)
pacf(Netflix_Close,main="Neflix Close Price PACF Plot",lag.max = 20)

#--------------Data Transformaton--------------#
par(mfrow=c(1,1))
close<-drop(coredata(NFLX$NFLX.Close))[2:1260]
#(1)log trans
log_price<-log(NFLX$NFLX.Close)
plot(log_price)
adf.test(log_price) #nonstationary #不能用

#(2)diff trans
dim(diff_price)
diff_NFLX_Closing<-diff(NFLX$NFLX.Close)[2:1260]
plot(diff_NFLX_Closing,type="l")
adf.test(diff_NFLX_Closing) #stationary
par(mfrow=c(1,2))
acf(diff_NFLX_Closing,main="Neflix Diff Close Price ACF Plot",lag.max = 20)
pacf(diff_NFLX_Closing,main="Neflix Diff Close Price PACF Plot",lag.max = 20)


#(3)log+diff
logDiff<-diff(log_price)[2:1253]
plot(logDiff)
adf.test(logDiff) #stationary

#--------------ARIMA--------------#
train=Close[1:1200]
test=Close[1201:1259]
dim(Close)
model=auto.arima(train)
summary(model) #random walk?
checkresiduals(model1)

model1<-Arima(close,c(0,1,0))
summary(model1)

#residual check
par(mfrow=c(1,1))
plot(model1$residuals,ylab="residual",main="")
checkresiduals(model1)

shapiro.test(resid(model1))
Box.test(resid(model1))
t.test(resid(model1))

pred=forecast(model1,h=length(test))
plot(close,main="Forecast from ARIMA(0,1,0)",ylab="price",type="l",lwd=1.2)
lines(1201:1259,pred$mean,col=2,lty=1,lwd=1.5)
abline(v=1200,col="blue",lty=2,lwd=1.2)
legend("topright",col=c(1,2),c("actual","predict"),lty=c(1,1),cex=0.8)

lines(1201:1259,pred$lower,col=2,lty=2,lwd=1.5)

pred$mean
summary(pred)
plot(pred)
#--------------ARCH Effect--------------#
#install.packages("rugarch")
library(rugarch)
#install.packages("tseries")
library(tseries)
#install.packages("fBasics")
library(fBasics)
#install.packages(zoo)
library(zoo)
install.packages("vars")
library(vars)
library(nortsTest)
plot(diff_price^2)
plot(abs(diff_price))
Lm.test(train,alpha = 0.05)


59+10+63



