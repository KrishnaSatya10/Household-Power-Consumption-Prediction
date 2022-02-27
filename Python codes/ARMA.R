df <- read.csv("housing_data.csv")
df1 <- data.frame(df[3])
na_vec <- which(!complete.cases(df1))
df1_no_na <- df1[-na_vec,]
df2 <- data.frame(df1_no_na)
names(df2)[1] <-paste("Global_reactive_power") 
acf(df2)
AR <- arima(df2,order = c(1,0,0))
AR_fit <- df2$Global_reactive_power - residuals(AR)
ts.plot(df2)
points(AR_fit, type = "l", col = 2, lty = 2)
Predicted_AR <- predict(AR)
Predicted_AR$pred[1]

MA <- arima(df2,order=c(0,0,1))
print(MA)
MA_fit <- df2$Global_reactive_power - resid(MA)
predict_MA <- predict(MA)
predict_MA$pred[1]

cor(AR_fit, MA_fit)
AIC(AR)
AIC(MA)

BIC(AR)
BIC(MA)