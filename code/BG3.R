library(readxl)
library(prophet)
library(DIMORA)
library(forecast)

BG3<- read_excel("Baldur's Gate 3 _Players.xls")
Players = BG3$Players * 0.1343096
player <- ts(Players, frequency = 1, start = 1) 

BG3_price<-read_excel("Baldur's Gate 3 _Price.xlsx")
price_change <- BG3_price$`Final price`

par(mar=c(5, 4, 4, 6) + 0.1)  # Adjust the last value for the right margin as needed

# Plot for player changes
plot(player, type="o", col="#ADD8E6", xlab="Time", ylab="Number of Players Change",
     main="Player Numbers and Price Fluctuations Over Time", pch=16, cex=1.2, lty=1, ylim=c(0, max(player)*1.1))


par(new=TRUE)
plot(price_change, type="o", col="#F08080", axes=FALSE, xlab="", ylab="", pch=17, cex=1.2, lty=2, ylim=c(0, max(price_change)*1.1))
axis(side=4, col="#F08080", col.axis="#F08080")
mtext("Price Fluctuation", side=4, line=3, col="#F08080")

legend("topright", inset=c(-0.004,0), legend=c("Number of Players Change", "Price Fluctuation (€)"),
       col=c("#ADD8E6", "#F08080"), lty=1:2, pch=16:17, cex=0.8, pt.cex=1.5, bty="n", xpd=TRUE)


###prediction (out-of-sample)
bm_bg <- GBM(player,shock = "exp",nshock = 2, prelimestimates = c(1.088884e+07, 0.003 , 0.1, 11, -0.9, 1, 152, -0.5, 1))
summary(bm_bg)

###prediction (out-of-sample)
pred_bmbg<- predict(bm_bg, newx=c(1:492))
pred.instbg<- make.instantaneous(pred_bmbg)

###plot of fitted model 
plot(cumsum(player), type= "b",xlab="Time", ylab="Total Change in Player Numbers",  pch=16, lty=1, cex=0.8, xlim=c(1,492), ylim=c(0,1.2e+7))
lines(pred_bmbg, lwd=2, col=2)

plot(player, type= "b",xlab="Time", ylab="Number of Players Change",  pch=16, lty=3, cex=0.6, xlim=c(1,492))
lines(pred.instbg, lwd=2, col=2)

###estimate the model with 50% of the data
bm_bg50<-GBM(player[1:246],shock = "exp",nshock = 2, prelimestimates = c(1.088884e+07, 2.729707e-04 , 1.270825e-02, 11, -0.9, 1, 152, -3.892836e-02, -8.554876e-01))
summary(bm_bg50)

pred_bmbg50<- predict(bm_bg50, newx=c(1:246))
pred.instbg50<- make.instantaneous(pred_bmbg50)

plot(player, type= "b",xlab="Time", ylab="Number of Players Change",  pch=16, lty=3, cex=0.6)
lines(pred.instbg50, lwd=2, col=2)

# Prophet
Average_player <- data.frame(player, BG3$DateTime, cap=118000)
colnames(Average_player)=c("y","ds","cap")
plot(Average_player$y, x=Average_player$ds, ylab = "Number of Players Change", xlab = "Day")

shock_dates <- data.frame(
  holiday = c('shock_event_1', 'shock_event_2'),
  ds = as.Date(c('2023-08-13', '2024-01-07')),  # Example shock dates
  lower_window = c(-7,-20),
  upper_window = c(35,35)  # Duration of the effect
)

m = prophet(Average_player, growth = "logistic", holidays = shock_dates)
summary(m)

##create a future 'window' for prediction
future <- make_future_dataframe(m, periods = 90, freq="day", include_history = T)
tail(future)
future$cap=118000

forecast <- predict(m, future)
tail(forecast[c("ds", "yhat", "yhat_lower", "yhat_upper")])

plot(m, forecast)

#Dynamic plot
dyplot.prophet(m, forecast) 

#plot with change points

plot(m, forecast)+add_changepoints_to_plot(m, threshold=0)

#dates corresponding to change points
m$changepoints

##Prophet with no change points and multiplicative seasonality 
m2=prophet(Average_player,  growth="logistic", n.changepoints=0, holidays = shock_dates, seasonality.mode='multiplicative')

summary(m2)
m2$seasonalities 

future2 <- make_future_dataframe(m2, periods = 90, freq="day", include_history = T)
tail(future2)
future2$cap=118000

forecast2 <- predict(m2, future2)
tail(forecast2[c("ds", "yhat", "yhat_lower", "yhat_upper")])

#prediction plot

plot(m2, forecast2)
dyplot.prophet(m2, forecast2) 
