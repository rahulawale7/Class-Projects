# ANLY 565 Time Series and Forecasting 
# Practical Assingment #2
# Value: 150 points
# Deadline: March 17th, 2021

#1  Check you working directory
getwd()
#2  Set your working directory to "ANLY 580/RScript". 
#   Upload "nlme" library
library(nlme)

setwd("C:/Users/rahul/Documents/ANLY 565/RScript")

#3  Download "trade.xls" data file and set the "date" 
#   variable to the date format and the "trade" variable to
#   the numeric format. The "trade" variable represents 
#   the Ratio of Exports to Imports for China expressed in percentages.

library(readxl)

trade_data <- read_excel("trade.xls")

date<- as.Date(trade_data$date)

trade <- as.numeric(trade_data$trade)

#4  Create two stand alone varibales: "datev" and "tradev". 
#   "datev" variable should represent values of the "date" variable 
#   from the "trade" data set, while, "tradev" variable should represent 
#   values of the "trade" variable from the "trade" data set.

datev<- trade_data$date
tradev<- trade_data$trade

#5  Use the "datev" variable and the range() function to check the time sample
#   covered by the "trade" data set. What time period is covered?
#   What is the frequency of the data?
# -> It covers from 1992-02-01 to 2019-04-01. As for the frequency of the data, it is monthly.
#range
range(datev)

#frequency
head(datev)

#6  Transform "tradev" variable from numeric format to the time series format 
#   by using ts() function. Lable the new varibale as "tradets".  

tradets<-ts(tradev, start= c(1992,1), end= c(2019,4), freq=12)

#7  Plot the time series graph of the "tradets"variable.
#   Please lable all axis correcly, and make sure to lable the graph. 
#   Based on this graph does the Ratio of Exports to Imports for China exhibit a trend? 
#   What about a regular seasonal fluctuation? 
# -> Yes, based on the graph, the ratio of exports to imports for China does exhibit a trend. 
# -> Yes, it also exhibits seasonal fluctuation that can be observed using the decompose function.

plot(tradets, xlab = "time(year)", ylab = "trade ratio(%)", main = "Trade Ratio Trend for China")

plot(decompose(tradets))

#8  Use "tradets" variable and window() function to create 2 new variables 
#   called "tradepre", "tradepost". 
#   The "tradepre" should include all observations for the period 
#   up until December 2018.(Last observation should be December 2018)
#   The "tradepost" should include all observations starting from January 2019.
#   and up until the last month in the dataset.

tradepre<-window(tradets,start= c(1992,1), end=c(2018,12), freq=12)

tradepost<- window(tradets, start= c(2019,1), end=c(2019,4), freq=12)

#9  Estimate autocorrelation function and partial autocorrelation function for 
#   the "tradepre" variable. Does the trade ratio for China exhibit autocorrelation?  
#   What process can explain this time series (white noise, random walk, AR, etc..)?

#autocorelation
acf(tradepre)

#partial autocorrelation
pacf(tradepre)

#10 Estimate AR(q) model for the "tradepre" time series. 
#   Use ar() function (set aic=FALSE) and rely on the corellologram 
#   to determine q, the order of the model. Moreover, use maximum liklehood method.
#   After that, set aic=TRUE and estimate ar() again to see if you have identified 
#   the order correctly.
#   Save the estimates as "trade.ar".

trade.ar<- ar(tradepre,method="mle", aic=FALSE)

#q 
trade.ar$order

#summary
summary(trade.ar)

#autocorrelation
acf(trade.ar$resid[-(1:trade.ar$order)])

# checking again to see results when aic=TRUE 

trade.ar2<- ar(tradepre,method="mle", aic=TRUE)

#q
trade.ar2$order

#summary
summary(trade.ar2)

#autocorrelation 
acf(trade.ar2$resid[-(1:trade.ar2$order)])

#different results when aic=FALSE, q=12 and when aic=TRUE, q=3. 

#11 For each of the AR coeficients estimate 95% confidence interval
#   To find 95% confidence intervals you need to add and subtract 2
#   standard deviations of the coefficient estimates. 
#   Hint you can obtain these standard deviations by applying sqrt()
#   function to the diagonal elements of the asymptotic-theory variance 
#   matrix of the coefficient estimates

#checking
trade.ar2$ar

#ar1
trade.ar2$ar[1]+c(-2,2)*sqrt(trade.ar2$asy.var.coef[1,1])

#ar2
trade.ar2$ar[2]+c(-2,2)*sqrt(trade.ar2$asy.var.coef[2,2])

#ar3
trade.ar2$ar[3]+c(-2,2)*sqrt(trade.ar2$asy.var.coef[3,3])

#12 Extract the residuals from the trade.ar model and estimate 
#   the autocorrelation function. Based on this correlogram would you say 
#   trade.ar model does a good job of explaining the trade ratio in China?
# ->Yes, the trade.ar2 in this case, does a good job of explaining the trade ratio in China.

acf(trade.ar2$resid[-(1:trade.ar2$order)])


#13 Use trade.ar model and predict() function to creat a 4 period ahead forecast
#   of the trade ratio in China. Save these predicted values as "trade.ar.forc"

trade.ar.forc<- predict(trade.ar2, n.ahead = 4)

trade.ar.forc

#14 Use ts.plot() function to plot side-by-side actual values of the trade ratio
#   from January 2019-April 2019 period and their forecasted counterparts. 
#   (tradepost and trade.ar.forc)
#   Please designate red color to represent the actual observed values, 
#   and blue doted lines to represent forecasted values. 
#   How does the ability to predict future trade ratio depends on the 
#   time horizon of the forecast?
#-> Based on the graph, the ability to predict future trade depending on the time horizon does not look good because the actual values are far apart from the forecasted values.
#-> In addition, we can only observe one correct prediction which is at the intersect between the two lines. 

ts.plot(tradepost, trade.ar.forc$pred, lty = c(1,10), col=c("red","blue"))

#15 Please calculate forecast's mean absolute percentage error 
#   for the trade.ar.forc forecasting model. Why is it important to calculate 
#   mean absolute percentage error rather than mean percentage error?
#

#->Mean Absolute error percentage is more reliable compared to Mean Percentage error because Mean Absolute error is the ratio of the residual over the actual.
#  Therefore, it is less effected by outliers. In addition, unlike in Mean Percentage error, positive and negative does not cancel each other there it is also more accurate.

#forecast's mean absolute error percentage
mean(abs((tradepost - trade.ar.forc$pred)/tradepost * 100))

#16 Use time() function and tradepre variable to create a variable called "Time".

Time <- time(tradepre)

#17 Estimate linear regression model by regressing "Time" on "tradepre" variable.
#   Save this regression model as "trade.lmt". 
#   By using confint() function calculate 95% confidence intervals for the estimated 
#   model coeficients.
#   What can you conclude based on the estimates of the model coeficients?
#   What is the direction of the time trend?

#->Zero is not contained in the intervals, which implies that the estimates are likely significant.In addition, we have narrow confident interval for the slope and the residuals series is autocorrelated with shorter lags.  
# Lastly, the direction of the time trend is positive as we have a positive cofficient of time. 

trade.lmt <- lm(tradepre ~ Time)

coef(trade.lmt)

summary(trade.lmt)

acf(resid(lm(tradepre ~ Time))) 

#18 By visually inspecting a time series plot of the "tradepre" variable, 
#   and given the seasonal nature of the trade relationships it is reasonable to assume 
#   that there are regular seasonal fluctuations in the trade ratio for China. 
#   Use "tradepre" variable and cycle() function to create a factor variable titled "Seas".

plot.ts(tradepre)

Seas<- factor(cycle(tradepre))
str(Seas)


#19 Use lm() function to estimate linear regression model by regressing 
#   "Time" and "Seas" on "tradepre". Save this regression model as "trade.lmts".
#   Set the value of the intercept to 0, in order to interpret the 
#   coeficients of the seasonal dummy variables as seasonal intercepts. 
#   (Setting intercept to 0 ensures that for each season there is a unique intercept)
#   What can you conclude based on the estimates of the model coeficients?
#   What is the direction of the time trend? Is there a seasonal component?
#   During which month should you expect the trade ratio to be the largest?

#->First the estimates are significant because zero is not contained in the intervals.
#->The direction of the time trend is positive and all season are negative
#->The trade ratio expects to be the largest in December.

trade.lmts <- lm(tradepre ~ 0 + Time + Seas)

coef(trade.lmts)

summary(trade.lmts)

confint(trade.lmts)

#20 Extract the residual series from the "trade.lmts" model and save them as 
#   "trade.lmts.resid". Then, estimate autocorrelation function to check the 
#   goodness of the fit. What is the value of autocorrelation at lag 1?
#   What can you conclude based on the correlogram of the residual series

#-> The value of aurocorrelation at lag 1 is 0.664
# Based on the correlogram the residual series are positively correlated.

trade.lmts.resid <- trade.lmts$residuals

acf(trade.lmts.resid)

pacf(trade.lmts.resid)

acf(trade.lmts.resid)[1]

#21 Fit linear model by regressing "Time" and "Seas" on "tradepre"
#   by uzing generalized least squares (gls() fucntion).
#   Set the value of the intercept to 0, in order to interpret the 
#   coeficients of the seasonal dummy variables as seasonal intercepts.
#   Save this model's estimates as "trade.gls".

trade.gls <- gls(tradepre ~ 0 + Time + Seas)

coef(trade.gls)


#22 Compute Akaike's An Information Criterion for "trade.lmts" and "trade.gls".
#   Which model performs better?
#-> GLS performs better because it has a smaller AIC.

AIC(trade.lmts)

AIC(trade.gls)

#23 Create the following new variables: 
#   "new.Time"- sequence of 4 values starting from 2019 and each number going up by 1/12
#   "alpha" - assumes value of the Time coeficient from the trade.gls model
#   "beta" - takes on values of the first, second, third, and fourth seasonal coeficients 
#            from the trade.gls model.

new.Time <- seq(2019, len = 4, by = 1/12)

alpha <- coef(trade.gls)[1]

beta <- coef(trade.gls)[2:5]


#24 By using the forecasting equation of x_(t+1)<-0+alpha*Time_(t+1)+beta
#   create a 4 period ahead forecast of the trade ratio for China. 
#   Lable this forecast as "trade.gls.forc"

trade.gls.forc <- (0 + alpha * new.Time + beta)[1:4]

trade.gls.forc

#25 Use ts.plot() function to plot side-by-side actual values of the trade ratio
#   from January 2019-April 2019 period and their forecasted counterparts. 
#   (tradepost and trade.gls.forecast)
#   Please designate red color to represent the actual observed values, 
#   and blue doted lines to represent forecasted values.

ts.plot(tradepost, trade.gls.forc, lty = c(1,10), col=c("red","blue"))

#26 Please calculate forecast mean absolute percentage error 
#   for the "trade.gls.forc" forecasting model. Based on the 
#   forecast's mean absolute percentage error, which of the two models, 
#   "trade.ar.forc" and trade.gls.forc" performs better?
#-> The trade.ar.forc performs better because it has a smaller mean absolute percentage error.


mean(abs((tradepost - trade.gls.forc)/tradepost * 100))

#27 Create a variable called tradepreL, that represents the first lagged value
#   of the "tradepre" variable. For example tradepreL_t=tradepre_(t-1).
#   Moreover, transform "tradepreL" variable into a time series object by using ts().
#   It should cover the same time period as "tradepre".

tradepreL <- tradepre
for (t in c(2:length(tradepre))) {
  tradepreL[t] <- tradepre[t-1]
}
tradepreL <- ts(tradepreL)


#28 Use lm() function to estimate linear regression model by regressing 
#   "tradepreL", "Time" and "Seas" on "tradepre". 
#   Set the value of the intercept to 0, in order to interpret the 
#   coeficients of the seasonal dummy variables as seasonal intercepts.
#   Save this regression model as "trade.ar.lmts".

trade.ar.lmts <- lm(tradepre ~ 0 + tradepreL + Time + Seas)

coef(trade.ar.lmts)


#29  By using new.Time variable, and the following forecasting equation 
#    x_(t+1)<-0+alpha1*x_t+alpha2*Time_(t+1)+beta 
#    create the following new variables:
#   "alpha1" - assumes value of the tradepreL coefiencient from the trade.ar.lmts model 
#   "alpha2" - assumes value of the Time coeficient from the trade.ar.lmts model
#   "beta1" - takes on values of the first seasonal coeficient from the trade.ar.lmts.
#   "beta2" - takes on values of the second seasonal coeficient from the trade.ar.lmts.
#   "beta3" - takes on values of the third seasonal coeficient from the trade.ar.lmts.
#   "beta4" - takes on values of the fourth seasonal coeficient from the trade.ar.lmts.
#   "forc20191" - takes on the forecasted value of the trade ratio for January 2019
#   "forc20192" - takes on the forecasted value of the trade ratio for February 2019
#   "forc20193" - takes on the forecasted value of the trade ratio for March 2019
#   "forc20194" - takes on the forecasted value of the trade ratio for April 2019
#   "trade.ar.lmts.forc" a vector of four predicted trade ratios.

alpha1 <- coef(trade.ar.lmts)[1]

alpha2 <- coef(trade.ar.lmts)[2]

beta1 <- coef(trade.ar.lmts)[3]

beta2 <- coef(trade.ar.lmts)[4]

beta3 <- coef(trade.ar.lmts)[5]

beta4 <- coef(trade.ar.lmts)[5]

forc20191 <- 0 + alpha1*tradepre[length(tradepre)] + alpha2*new.Time[1] + beta1

forc20192 <- 0 +alpha1*forc20191 + alpha2*new.Time[2] +beta2

forc20193 <- 0 +alpha1*forc20192 + alpha2*new.Time[3] +beta3

forc20194 <- 0 +alpha1*forc20193 + alpha2*new.Time[4] +beta4

trade.ar.lmts.forc <- c(forc20191,forc20192,forc20193,forc20194)



#30 Please calculate forecast mean absolute percentage error 
#   for the trade.ar.lmts.forc forecasting model.
#   Which of the following models would you chose to based on this criteria?
#   Models: trade.ar.forc, trade.gls.forc, and trade.ar.lmts.forc)

#-> Based on this criteria I will choose trade.ar.forc since it has the smallest mean absolute percentage error.

mean(abs((tradepost - trade.ar.lmts.forc)/tradepost * 100))
