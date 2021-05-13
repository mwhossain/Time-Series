library(fpp)
library(TSA)
library(forecast)
library(astsa)
library("xts")

dataPath<-"C:/Users/manya/Downloads/"
tourism <- read.csv(paste0(dataPath,"TourismTimeSeries.csv"))

middleeast<-tourism[,14:14]
#month<-as.Date(tourism[,1:1])
dates<- seq(as.Date("1999-1-1"), by = "month", leng = 264)

mideast<-xts(x = middleeast, order.by = dates)
plot(mideast)
mideast

#mideast<-ts(middleeast)

geo.ts<-ts(middleeast, frequency = 12, start = c(1999, 1))
tourism.ts <- window(geo.ts, start = c(2002,1), end = c(2019,12))

plot(tourism.ts)
adf.test(trainf)
kpss.test(trainf)

adjmideast<-mideast[37:240]
tourism.ts<-mideast[37:240]
plot(adjmideast)

bestLambda <- BoxCox.lambda(adjmideast)
bestLambda
tmideast<-BoxCox(adjmideast, bestLambda)
plot(tmideast)

tourism.seasonal.diff1 <- diff(tmideast, lag = 12)
tourism.seasonal.diff2 <- diff(tourism.seasonal.diff1, lag = 12)
tsdisplay(tourism.seasonal.diff2)


kpss.test(tourism.seasonal.diff1)

trainf <- mideast[37:240]
testf <- mideast[241:252]
trainf[240]
trainf
testf
testf[12]
plot(trainf)

bestLambda <- BoxCox.lambda(trainf)
bestLambda
train_l<-BoxCox(trainf, bestLambda)
train_l
plot(train_l)

#Fit ARIMA
fit <- auto.arima(trainf,lambda = "auto")
fit
auto.arima(train_l, D= 1, lambda = TRUE)
checkresiduals(fit)

forecast.arima <- forecast(fit,h=24,level = c(80,95))

autoplot(forecast.arima) 
 #abline(testf, col = "blue")

manual<-Arima(train_l, order=c(4,1,2), seasonal=list(order=c(0,6,0),period=12))
manual2<-Arima(train_l, order=c(3,1,5), seasonal=list(order=c(0,6,0),period=12))
manual
checkresiduals(manual)

predict2019<-predict(manual, n.ahead = 12)
predict2019
inv_pred<-InvBoxCox(predict2019$pred, bestLambda, biasadj = FALSE, fvar = NULL)
inv_pred

forc2019<-forecast(manual, h=12)
forc2019mean<-forc2019$mean
forc2019mean
inv_forc<-InvBoxCox(forc2019mean, bestLambda, biasadj = FALSE, fvar = NULL)
inv_forc
testf

forc2019auto<-forecast(fit, h=12)
aforc2019mean<-forc2019auto$mean
aforc2019mean
inv_auto<-InvBoxCox(aforc2019mean, bestLambda, biasadj = FALSE, fvar = NULL)
inv_auto

plot(inv_forc)
plot(testf)
plot(aforc2019mean)
#plot(inv_auto)
inv_auto

Compare<-cbind(testf, aforc2019mean, inv_forc)#inv_auto
Compare

autoplot(testf) +
  autolayer(inv_forc, series="Arima") +
  xlab("Year") + ylab("travel")

(fit.ets <- ets(tourism.seasonal.diff2))
forcETS<-forecast(fit.ets, h=12)
plot(forcETS)
forcETS



##To Do:
##Explore ETS further



