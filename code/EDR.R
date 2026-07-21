library(readxl)
library(prophet)
library(DIMORA)
library(forecast)

Edr<- read_excel("Eleden Ring Players.xls")
Players = Edr$Players * 0.2982192
player <- ts(Players, frequency = 1, start = 1) 

Edr_price <- read_excel("EDR Price.xlsx")
price_change <- Edr_price$`Final price`

par(mar=c(5, 4, 4, 6) + 0.1)  # Adjust the last value for the right margin as needed

# Plot for player changes
plot(player, type="o", col="#ADD8E6", xlab="Time", ylab="Number of Players Change",
     main="Player Numbers and Price Fluctuations Over Time", pch=16, cex=1.2, lty=1, ylim=c(0, max(player)*1.15))

par(new=TRUE)
plot(price_change, type="o", col="#F08080", axes=FALSE, xlab="", ylab="", pch=17, cex=1.2, lty=2, ylim=c(0, max(price_change)*1.15))
axis(side=4, col="#F08080", col.axis="#F08080")
mtext("Price Fluctuation", side=4, line=3, col="#F08080")

legend("topright", inset=c(-0.004,0), legend=c("Number of Players Change", "Price Fluctuation (€)"),
      col=c("#ADD8E6", "#F08080"), lty=1:2, pch=16:17, cex=0.8, pt.cex=1.5, bty="n", xpd=TRUE)

###prediction (out-of-sample)
bm_edr <- GBM(player,shock = "exp",nshock = 2, prelimestimates = c(2.803218e+07, 7.386279e-05, 1.797903e-03, 10, -5.023506e-02, 6.997694e+01, 850, -2.512700e-02, 1.142238e-02))
summary(bm_edr)

###prediction (out-of-sample)
pred_bmedr<- predict(bm_edr, newx = (1:1016))
pred.instedr<- make.instantaneous(pred_bmedr)

###plot of fitted model 
plot(cumsum(player), type= "b",xlab="Time", ylab="Total Change in Player Numbers",  pch=16, lty=1, cex=0.8)
lines(pred_bmedr, lwd=2, col=2)

plot(player, type= "b",xlab="Time", ylab="Number of Players Change",  pch=16, lty=3, cex=0.6)
lines(pred.instedr, lwd=2, col=2)

###estimate the model with 50% of the data
bm_edr50<-GBM(player[1:500],shock = "exp",nshock = 2, prelimestimates = c(2.8600004e+07, 2.550158e-05, 8.935422e-04, 10, -3.027675e-02,  3.701211e+00, 319, -0.001, 0.1))
summary(bm_edr50)

pred_bmedr50<- predict(bm_edr50, newx=c(1:500))
pred.instedr50<- make.instantaneous(pred_bmedr50)

plot(player, type= "b",xlab="Time", ylab="Number of Players Change",  pch=16, lty=3, cex=0.6)
lines(pred.instedr50, lwd=2, col=2)

###estimate the model with 50% to 100% of the data
bm_edr50_100<-GBM(player[501:1016],shock = "exp",nshock = 1, prelimestimates = c(2.8600004e+07, 6.389492e-04, 4.552686e-03, 350, -4.142344e-02,  8.827355e+00))
summary(bm_edr50_100)

pred_bmedr50_100<- predict(bm_edr50_100, newx=c(1:516))
pred.instedr50_100<- make.instantaneous(pred_bmedr50_100)

x_values <- 1:length(pred.instedr50_100)
adjusted_x <- x_values + 516
plot(player, type= "b", xlab="Time", ylab="Number of Players Change", pch=16, lty=3, cex=0.6)
lines(adjusted_x, pred.instedr50_100, lwd=2, col=2)


# Prophet 
Average_player <- data.frame(player, Edr$DateTime, cap=284330)
colnames(Average_player)=c("y","ds","cap")
plot(y = Average_player$y, x = Average_player$ds, type = "o",
     xlab = "Day", ylab = "Number of Players Change", lwd = 2, pch = 16, cex = 1.2)

shock_dates <- data.frame(
  holiday = c('shock_event_1', 'shock_event_2', 'shock_event_3'),
  ds = as.Date(c('2022-03-05', '2023-01-08', '2024-06-22')),  # Example shock dates
  lower_window = c(-10,-18,-3),
  upper_window = c(60,180,90)  # Duration of the effect
)

m = prophet(Average_player, growth = "logistic", holidays = shock_dates)
summary(m)

future <- make_future_dataframe(m, periods = 90, freq="day", include_history = T)
tail(future)
future$cap=284330

forecast <- predict(m, future)
tail(forecast[c("ds", "yhat", "yhat_lower", "yhat_upper")])

plot(m, forecast)

#Dynamic plot
dyplot.prophet(m, forecast) 

#plot with change points

plot(m, forecast) +add_changepoints_to_plot(m, threshold=0)

#dates corresponding to change points
m$changepoints

##Prophet with no change points and multiplicative seasonality 
m2=prophet(Average_player,  growth="logistic", n.changepoints=0, holidays = shock_dates, seasonality.mode='multiplicative')

summary(m2)
m2$seasonalities 

future2 <- make_future_dataframe(m2, periods = 90, freq="day", include_history = T)
tail(future2)
future2$cap=284330

forecast2 <- predict(m2, future2)
tail(forecast2[c("ds", "yhat", "yhat_lower", "yhat_upper")])

#prediction plot

plot(m2, forecast2)
dyplot.prophet(m2, forecast2) 
