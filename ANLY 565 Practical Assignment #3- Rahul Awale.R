# ANLY 580 Time Series and Forecasting 
# Practical Assingment #3
# Value: 150 points
# Deadline: July 25th

#1  Check your working directory
getwd()
#2  Set your working directory to "ANLY 565/RScript". 
setwd("P:/Documents/ANLY 565/RScript")

#checking new working directory
getwd()

#3  Download "Inflation.xls" data file and set the "observation_date" 
#   variable to the date format and the "CPI" variable to the numeric format.
#   The "CPI" variable represents the consumer price index,
#   which idicates the relative prices of a consumer basket. 

library(readxl)

inflation <- read_xls("Inflation.xls")

inflation$observation_date <- as.Date(inflation$observation_date)

inflation$CPI <- as.numeric(inflation$CPI)

str(inflation)

#4  Create two stand alone varibales: "date" and "cpi". 
#   "date" variable should represent the values of the "observation_date" 
#   variable from the "Inflation" data set, while, "cpi" variable should represent 
#   values of the "cpi" variable from the "Inflation" data set.

date <- inflation$observation_date

cpi <- inflation$CPI

#5  Transform "cpi" variable from numeric format to the time series format 
#   by using ts() function. Lable the new varibale as "cpits".  

#checking range for dates

range(date)

cpits<-ts(cpi, start=c(1913,1), end=c(2019,1), frequency=12)

#6  Please construct the following three graphs:
#   1)time series plot, 2) autocorrelation
#   and 3) partial autocorrelation functions for the "cpits" variable. 
#   Based on the signature of these graphs, does the variable appear
#   stationary? Explain

#Answer: No the variable seems to be non-stationary. Based on the plot, the mean value of the time series is not constant and the variance changes over time. 


plot(cpits, main = "CPI from Year 1913 to 2019", xlab = "Year", ylab = "CPI")

acf(cpits)

pacf(cpits)



#7  Use "cpits" variable and window() function to create 2 new variables 
#   called "cpi.pre", "cpi.post". 
#   The "cpi.pre" should include all observations for the period starting from 
#   January of 1990 and up until October 2018.
#   The "cpi.post" should include all observations starting from November 2018.
#   and up until the last month in the dataset.

cpi.pre <- window(cpits, start = c(1990,1), end = c(2018, 10), frequency = 12)


cpi.post <- window(cpits, start = c(2018,11), end = c(2019,1), frequency = 12)

#8  Use time() function and "cpi.pre" variable to create a variable called "Time".
#   Moreover, use "cpi.pre" variable and cycle() function to create 
#   a factor variable titled "Seas".

Time <- time(cpi.pre)

Seas <- factor(cycle(cpi.pre))

#9  Use lm() function to estimate parameter values of a linear regression model 
#   by regressing "Time", and "Seas" on "cpi.pre". 
#   Save these estimates as "cpi.lm".
#   Set the value of the intercept to 0, in order to interpret the 
#   coeficients of the seasonal dummy variables as seasonal intercepts. 
#   (Setting intercept to 0 ensures that for each season there is a unique intercept) 
#   Save these estimates as cpi.lm

cpi.lm <- lm(cpi.pre ~ 0 + Time + Seas)

str(cpi.lm)

#10 Create the following new items: 
#   "new.Time"- sequence of 12 values starting from 2018.75+1/12
#    and each number going up by 1/12
#   "new.Seas"- a vector with the following values c(11,12,1,2,3,4,5,6,7,8,9,10)
#   "new.data"- a data frame that combines the "new.Time" and "new.Seas" variables.

new.Time <- seq(2018.75, len = 12, by = 1/12)

new.Seas <- factor(c(11,12,1,2,3,4,5,6,7,8,9,10))

new.data <- data.frame(Time=new.Time, Seas=new.Seas)

new.data



#11 Use predict() function and cpi.lm model to create a 12 month ahead forecast 
#   of the consumer price index. Save this forecast as "predict.lm"


predict.lm<-predict(cpi.lm, new.data)

predict.lm

#12 Collect residuals from the "cpi.lm" model and save them as "cpi.lm.resid".
#   Moreover, constuct acf and pacf for the "cpi.lm.resid" series. 
#   Is the series stationary?
# -> No
#   Is there autocorrelation in the residual series?
# -> Yes

cpi.lm.resid <- cpi.lm$residuals

acf(cpi.lm.resid)

pacf(cpi.lm.resid)

#13 Based on the AIC, identify the best order of ARMA model 
#   (without the seasonal component) for the cpi.lm.resid time series 
#   and estimate the value of the parameter coefficients. 
#   Please, consider any ARMA model with up to 3 AR and/or MA terms.
#   Save these estimates as resid.best.arma.
#   What is the order of resid.best.arma?
#-> (1,0,2)

best.order <- c(0, 0, 0)

best.aic <- Inf
for (i in 0:3) for (j in 0:3) {
  fit.aic <- AIC(arima(cpi.lm.resid, order = c(i, 0,
                                               j)))
  if (fit.aic < best.aic) {
    best.order <- c(i, 0, j)
    best.arma <- arima(cpi.lm.resid, order = best.order)
    best.aic <- fit.aic
  }}

best.order

resid.best.arma <- best.arma

coef(best.arma)

#14 Use predict() function and resid.best.arma to 
#   create a 12 period ahead forecast of cpi.lm.resid series.
#   Save the forecasted values as resid.best.arma.pred

resid.best.arma.pred <- predict(resid.best.arma, n.ahead = 12)

resid.best.arma.pred$pred

#15 Use ts() function to combine the cpi values forecaseted by cpi.lm model
#   and the residual values forecasted by resid.best.arma.
#   Lable this time series as cpi.pred

cpi.pred <- ts((predict.lm + resid.best.arma.pred$pred),start = c(2018,11), frequency = 12)

#16 Use ts.plot() function to plot cpi.pre and cpi.pred together on one graph.
#   What do you expect will happen to the CPI during the next 12 month?

ts.plot(cbind(cpi.pre, cpi.pred), lty = 1:2)

#17 Please calculate mean absolute percentage error for the cpi.pred
#   forecast for the first three month (Novermber 2018, December 2018, January 2019)
#   How accurate is the model? 
# The model is roughly 99% accurate, as the error rate is only 0.3931%.

mean(abs(cpi.post[1:3] - cpi.pred[1:3])/cpi.post[1:3])*100

#18 What is the forecasted rate of inflation between December 2018 and January 2019?
#   Hint: Inflation = % change in CPI
# The rate of inflation=0.2841%

diff(log(cpi.pred[2:3])) * 100


#19 Policy makers often care more about inflation rather than cpi.
#   Create a new stand alone varible that would represent 
#   the first log difference of the the cpits variable. 
#   Label this variable  "pi", which represents monthly inflation rate in the US.
#   If percentage change is positive there is inflation (prices go up), 
#   and if the percentage change is negative there is deflation (prices fall). 
#   What was the lowest monthly rate of inflation(deflation) recorded in US
#   during the time sample? What about was the highest?

#The lowest rate is a deflation of -3.21% and the highest inflation rate is 5.72%

pi<-(diff(log(cpits)))* 100

summary(pi) 

#20 Please construct the time series plot, the autocorrelation
#   and partial autocorrelation functions for the "pi" variable. 
#   Based on the signature of these graphs, does the variable appear
#   stationary? Explain

#Answer: the variance seems to change over time. Also, the acf and pacf indicate seasonality and trends. The time series is non-stationary.
plot(pi, main = "Inflation rate in the US", xlab = "Time", ylab = "Inflation rate")

acf(pi)

pacf(pi)

#21 Use "pi" variable and window() function to create 2 new variables 
#   called "pi.pre", "pi.post". 
#   The "pi.pre" should include all observations for the period starting from 
#   January of 1990 and up until October 2018.
#   The "pi.post" should include all observations starting from November 2018.
#   and up until the last month in the dataset.

pi.pre <- window(pi, start = c(1990,1), end = c(2018,10), frequency = 12)

pi.post <- window(pi, start = c(2018,11), end = c(2019,1), frequency = 12)


#22 Please create a function that takes a time series as input, 
#   and then uses AIC to identify the best SARIMA model. 
#   The function should return the following:
#   - the order of the best SARIMA, 
#   - its AIC
#   - and the estimates of its coefficient values
#   Lable this formula get.best.sarima

get.best.sarima <- function(x.ts, maxord = c(2,2,2,2,2,2))
{
  best.aic <- Inf
  n <- length(x.ts)
  for (p in 0:maxord[2]) for(d in 0:maxord[2]) for(q in 0:maxord[2])
    for (P in 0:maxord[2]) for(D in 0:maxord[2]) for(Q in 0:maxord[2])
    {
      fit <- arima(x.ts, order = c(p,d,q),
                   seas = list(order = c(P,D,Q),
                               frequency(x.ts)), method = "CSS")
      fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
      if (fit.aic < best.aic)
      {
        best.aic <- fit.aic
        best.fit <- fit
        best.model <- c(p,d,q,P,D,Q)
      }
    }
  list(best.aic, best.fit, best.model)
}


#23  By using get.best.sarima() function please identify the best SARIMA model
#    for pi.pre time series. 
#    Please cosider SARIMA(2,2,2,2,2,2) as the maximum order of the model. 
#    Save the results of the get.best.sarima() function as "pi.best.sarima"
#    What is the order of the best SARIMA model?

#The order of the best SARIMA model is (0,0,1,2,1,1).

pi.best.sarima <- get.best.sarima(pi.pre, maxord = c(2,2,2,2,2,2))

pi.best.sarima


#24  Please use predict() function and the best.sarima.pi model to forecast
#    monthly rate of inflation in the US during November 2018, December 2018
#    and January 2019.
#    Save these predictions as pi.sarima.pred

pi.sarima.pred <- predict(pi.best.sarima[[2]], n.ahead=3)

pi.sarima.pred$pred

#25 Please calculate mean absolute percentage error of the best.sarima model.
#   How accurate is the model? 
#The actual inflation rate is 14.14% lower than the predicted rate.
mean(abs(pi.post[1:3] - pi.sarima.pred$pred)/pi.post[1:3])*100

#26 Extract the residual series from the pi.best.sarima model,
#   and save them as sarima.resid.

sarima.resid <- pi.best.sarima[[2]]$residuals

#27 Plot the acf of the sarima.resid series and acf of the residsarima.resid^2 series 
#   What can you conclude based on these graphs?

#from two graphs, we can see there is evidence of serial correlation in both residual and squared values, so there is evidence of volatility and conditional heteroskedastic since the squared residuals are correlated at most lags. Hence, a Garch model should be fitted to the residual series.

acf(sarima.resid)

acf(sarima.resid^2)

#28 Download fGarch package and upload it to the library

library(fGarch)

#29 Use garchFit() function from the fGarch package 
#   to estimate garch(1,1) model of the resid time series. 
#   By doing so you will be able to analyze the volatility of the 
#   inflation, or, in other words,  how stable it is.
#   Make sure to set "include.mean=F" by doing so you suppress mean parameter 
#   from the default ARMA(0,0) model.
#   Save the estimated coefficients as resid.garch
resid.garch <- garchFit(~ garch(1,1), data=sarima.resid, include.mean = F, trace = F)

#30 The main priority of the monetary authority (Federal Reserve)
#   in the United States is to ensure stable value of currency. 
#   Simply put, Fed wants to keep inflation stable (no volatility). 
#   To maintain stability the Fed depends on a number of tools, 
#   and its effectiveness is judged based on the forecasting model of volatility.
#   Please use resid.garch and predict function 
#   to forecast two period ahead inflation volatily, which is measured by 
#   a square of the forecasted standard deviation.
#   How stable will be the currency in February and March of 2019?
#->the inflation volatility will be around 0.1%.
pi_resid_garch <- garchFit(~ garch(1,1), data=pi, include.mean = F, trace = F)


