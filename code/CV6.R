library(readxl)
library(DIMORA)
library(forecast)
library(tseries)

cv<- read_excel("Sid Meier's Civilization VI.xlsx")
Players = cv$Players * 0.04366329
player <- ts(Players, frequency = 1, start = 1) 
player <- player[-length(player)]

price <- cv$`Final Price`
price <- price[-length(price)]

quarter <-cv$Quarter
quarter <- quarter[-length(quarter)]

par(mar=c(5, 4, 4, 6) + 0.1)  # Adjust the last value for the right margin as needed

# Plot for player changes
plot(player, type="o", col="#ADD8E6", xlab="Time", ylab="Number of Players Change",
     main="Player Numbers and Price Fluctuations Over Time", pch=16, cex=1.2, lty=1, ylim=c(0, max(player)*1.2))

par(new=TRUE)
plot(price, type="o", col="#F08080", axes=FALSE, xlab="", ylab="", pch=17, cex=1.2, lty=2, ylim=c(0, max(price)*1.2))
axis(side=4, col="#F08080", col.axis="#F08080")
mtext("Price Fluctuation", side=4, line=3, col="#F08080")

legend("topright", inset=c(-0.004,0), legend=c("Number of Players Change", "Price Fluctuation (€)"),
       col=c("#ADD8E6", "#F08080"), lty=1:2, pch=16:17, cex=0.8, pt.cex=1.5, bty="n", xpd=TRUE)

#Bass Model
bm_cv <- BM(player, display = TRUE)

summary(bm_cv)

pred_bmcv<- predict(bm_cv, newx=c(1:32))
pred.instcv<- make.instantaneous(pred_bmcv)

plot(cumsum(player), type= "b",xlab="Time", ylab="Total Change in Player Numbers",  pch=16, lty=3, cex=0.6)
lines(pred_bmcv, lwd=2, col=2)

plot(player, type= "b",xlab="Time", ylab="Quarterly Change in Player Numbers",  pch=16, lty=3, cex=0.6)
lines(pred.instcv, lwd=2, col=2)

#Arima model 
a1<- Arima(player, order=c(2,1,2), seasonal=c(1,0,1)[4])
fit1<- fitted(a1)
summary(fit1)#!

plot(player)
lines(fit1, col=2)

f1<- forecast(a1)
plot(f1)

r1<- residuals(a1)
tsdisplay(r1) 

checkresiduals(f1)

#SARMAX refinement
##Exploratory plots
plot(player, type= "b",xlab="Quarter", ylab="Quarterly Change in Player Numbers",  pch=16, lty=3, cex=0.6, xaxt="n")
axis(1, at=c(1, 10, 15, 26, 32), labels=format(quarter[c(1, 10, 15, 26, 32)]))
#
#
plot(cumsum(player), type= "b",xlab="Quarter", ylab="Total Change in Player Numbers",  pch=16, lty=3, cex=0.6, xaxt="n")
axis(1, at=c(1, 10, 15, 26, 32), labels=format(quarter[c(1, 10, 15, 26, 32)]))
#
#
##BM 
bm_cv<- BM(player, display = TRUE)
#
##Prediction with pred_bm_cv2
pred_bm_cv<- predict(bm_cv, newx=c(1:32))
pred_bm_cv_i<- make.instantaneous(pred_bm_cv)[-1]
#
#
##Plots observed vs predicted
plot(player, type= "b",xlab="Quarter", ylab="Quarterly Change in Player Numbers",  pch=16, lty=3, cex=0.6, xaxt="n", col=1)
axis(1, at=c(1, 10, 15, 26, 32), labels=format(quarter[c(1, 10, 15, 26, 32)]))
lines(pred_bm_cv_i, lwd=2, col=2)
#
#
plot(cumsum(player), type= "b",xlab="Quarter", ylab="Total Change in Player Numbers",  pch=16, lty=3, cex=0.6, xaxt="n",col=1)
axis(1, at=c(1, 10, 15, 26, 32), labels=format(quarter[c(1, 10, 15, 26, 32)]))
lines(pred_bm_cv, lwd=2, col=2)
#
#
##SARMAX refinement

#
fit.player<- fitted(bm_cv)
s2 <- Arima(cumsum(player), order = c(2,1,2), seasonal=list(order=c(1,0,1), period=4), xreg = fit.player)
summary(s2)
#
pres2 <- make.instantaneous(fitted(s2))
#
##Plots observed vs predicted with SARMAX refinement
plot(player, type= "b",xlab="Quarter", ylab="Quarterly Change in Player Numbers",  pch=16, lty=3, cex=0.6, xaxt="n", col=1)
axis(1, at=c(1, 10, 15, 26, 32), labels=format(quarter[c(1, 10, 15, 26, 32)]))
lines(pred_bm_cv_i, lty=2, lwd=2, col=2)
lines(pres2, lty=1,lwd=2, col=4)
legend("topleft", legend=c("BM_cv","BM_cv+SARMAX"), lty=c(2,1), lwd = c(2,2), col = c(2,4))
#
plot(cumsum(player), type= "b",xlab="Quarter", ylab="Total Change in Player Numbers",  pch=16, lty=3, cex=0.6, xaxt="n",col=1)
axis(1, at=c(1, 10, 15, 26, 32), labels=format(quarter[c(1, 10, 15, 26, 32)]))
lines(pred_bm_cv, lty=2, lwd=2, col=2)
lines(cumsum(pres2), lty=3, lwd=2, col=4)
legend("topleft", legend=c("BM_cv","BM_cv+SARMAX"), lty=c(2,3), lwd = c(2,2), col=c(2,4))
