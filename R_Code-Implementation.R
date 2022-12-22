


library(forecast)

d <- read.csv("D:/END TO END/Business-Forecasting-Gold-Price/1979-2021.csv")
View(d)

d$Date <- format(as.POSIXct(d$Date,format='%d-%m-%Y'),format='%b-%Y')

head(d)
tail(d)

usd.ts <- ts(d$Europe.EUR., start = c(2005, 1), end = c(2016, 7), freq = 12)

train.ts <- window(usd.ts, start = c(2005, 1), end = c(2015, 3))
valid.ts <- window(usd.ts, start = c(2015, 4), end = c(2016, 7))


usd.lm <-  tslm(train.ts ~ trend + I(trend^2))
usd.lm


usd.lm.pred <- forecast(usd.lm, h = 16, level = 0)
names(usd.lm.pred)
#draw using training-period values and forecasts
plot(usd.ts, ylab = "usd", xlab = "Time")
lines(usd.lm.pred$fitted, lwd = 2)
lines(usd.lm.pred$mean, lwd = 2, lty=2)


lines(c(2015, 2015), c(0, 3500)) 
lines(c(2016.7, 2016.7), c(0, 3500))
text(2010, 600, "Training")
text(2016, 600, "Validation")


#forecast accuracy measures
accuracy(usd.lm.pred$mean, valid.ts)


#draw forecast errors
plot(usd.lm.pred$residuals, ylab = "Residuals", xlab = "Time", xlim = c(1979,2021))
#valid.ts-ridership.lm.pred$mean: forest errors in the validation period
lines(valid.ts - usd.lm.pred$mean, lwd = 1)
lines(c(2015.25, 2015.25), c(-500, 3500))
text(1996.25, 270, "Training")
text(2019.75, 270, "Validation")


#histogram of forecast errors
hist(usd.lm.pred$residuals,  ylab = "Frequency", xlab = "Forecast Error")



#forecasts and their 95% forecast intervals
usd.lm.pred.95 <- forecast(usd.lm, h=76, level = 95)
#draw training-period values and forecasts
plot(usd.lm.pred.95, ylab = "usd", xlab = "Time", flty = 2)
#trend line
lines(usd.lm.pred$fitted, lwd = 2)
#validation-period values
lines(valid.ts)
lines(c(2015.25, 2015.25), c(0, 3500)) 
text(1995, 1999, "Training")
text(2019, 1999, "Validation")


#naive forecast
naive.pred<-naive(train.ts, h=72)
#seasonal naive forecast
snaive.pred<-snaive(train.ts, h=72)
accuracy(naive.pred,valid.ts)
accuracy(snaive.pred,valid.ts)
accuracy(usd.lm.pred$mean, valid.ts)

###############################################################################



###############################################################################

#trailing moving average
ma.trailing <- rollmean(train.ts, k = 12, align = "right")
last.ma <- tail(ma.trailing, 1)
#trailing moving average for forecasting
ma.trailing.pred <- ts(rep(last.ma, 36), start = c(2015, 4), end = c(2021, 7), freq = 12)

plot(usd.ts, ylim = c(200, 2600),  ylab = "USD", xlab = "Time", xlim = c(1979,2021.7))
lines(ma.trailing, lwd = 2) 
lines(ma.trailing.pred, lwd = 2, col = "blue", lty = 2) 
lines(c(2015.25, 2015.25), c(0, 3500)) 
text(1996.25, 2500, "Training")
text(2020, 2500, "Validation")

#---Figure 5-4
par(mfrow = c(2,2))
plot(usd.ts, ylab = "usd", xlab = "Time", main = "usd")
#lag-12 differencing
plot(diff(usd.ts, lag = 12), ylab = "Lag-12", xlab = "Time", main = "Lag-12 Difference")
#lag-1 differencing
plot(diff(usd.ts, lag = 1), ylab = "Lag-1", xlab = "Time", main = "Lag-1 Difference")
#Twice differencing
plot(diff(diff(usd.ts, lag = 12), lag = 1), ylab = "Lag-12, then Lag-1", xlab = "Time", main = "Twice-Differenced (Lag-12, Lag-1)")
dev.off()

# Figure 5-5 (modified)
#exponential smoothing with alpha=.2
ses <- ets(train.ts, model = "ANN", alpha = 0.2)
#forecasts
ses.pred <- forecast(ses, h = 76, level = 0)
plot(usd.ts,ylab = "USD (Twice-Differenced)", xlab = "Time")
lines(ses.pred$fitted, lwd=2, col="blue")
lines(ses.pred$mean, lwd=2, lty=2, col="blue")
lines(c(2015.25, 2015.25), c(0, 3500)) 
text(1996.25, 1700, "Training")
text(2020, 1700, "Validation")

#---Table 5.1
#exponential smoothing without presetting the alpha
#the alpha is chosen automatically
ses.opt <- ets(train.ts, model = "ANN")
ses.opt.pred <- forecast(ses.opt, h = 76, level = 0)
#accuracy measures when alpha=.2
accuracy(ses.pred$mean, valid.ts)
#accuracy measures when the alpha is chosen automatically
accuracy(ses.opt.pred$mean, valid.ts)

#---Figure 5.6
#exponential smoothing with model=MAA
hwin <- ets(train.ts, model = "MAA")
#calculate forecasts
hwin.pred <- forecast(hwin, h = 76, level = 0)
plot(usd.ts,ylab = "usd", xlab = "Time")
lines(hwin.pred$fitted, lwd = 2, col = "blue")
lines(hwin.pred$mean, lwd = 2, lty=2, col = "blue")
lines(c(2015.25, 2015.25), c(0, 3500)) 
text(1996.25, 1700, "Training")
text(2020, 1700, "Validation")

#---Table 5.4
#exponential smoothing with parameters chosen automatically 
ets.opt <- ets(train.ts)
ets.opt.pred <- forecast(ets.opt, h = 76, level = 0)
plot(usd.ts,ylab = "usd", xlab = "Time")
lines(ets.opt.pred$fitted, lwd = 2, col = "blue")
lines(ets.opt.pred$mean, lwd = 2, lty=2, col = "blue")
lines(c(2015.25, 2015.25), c(0, 3500)) 
text(1996.25, 1700, "Training")
text(2020, 1700, "Validation")

