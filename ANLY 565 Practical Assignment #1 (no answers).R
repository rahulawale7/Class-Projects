# ANLY 565 Time Series and Forecasting 
# Practical Assignment #1 
# Value: 150 points
# Deadline: Feb 10th, 2021

#1  Check you working directory

getwd()

#2  Set your working directory to "ANLY 580/RScript"

setwd("C:/Users/rahul/Documents/ANLY 565/RScript")

#3  Download goy data set posted on Moodle  and lable it 
#   goy. This data set reperesnets daily prices of gold,
#   oil, and the price of 1 US dollar in terms of Japanese yen.
#   Set the first column in each data set to the date format 
#   and the remaining columns in numerical format.

library(readxl)
goy <- read_excel("~/ANLY 565/RScript/goy.xls")

goy$observation_date <- as.Date(goy$observation_date)

goy$gold <- as.numeric(goy$gold)
goy$oil <- as.numeric(goy$oil)
goy$yen <- as.numeric(goy$yen)
str(goy)
summary(goy)

#4  Create a new data set called "goycc" that contains all complete cases of goy data.
#   Utilize complete.cases function.

goycc <- goy[complete.cases(goy),]
sum(is.na(goycc))
str(goycc)
summary(goycc)

#5  Create a stand alone variable "date" that takes on values of "observation_date"
#   variable from the goycc data set. Set the mode of the varible to character
date<-(goycc$observation_date)
date<-as.character(date)
str(date)
head(date)


#6  Find the range of dates covered in goycc data set by applying range() function
#   to "date" variable. 

range(date)

#7  Create a time series objected called "goyccts" by utilizing goycc dataset and 
#   ts() function. In this dataset please exclude the first column 
#   of the goycc dataset. 

goycc1 <- goycc[,-1]
goyccts <- ts(goycc1, start = c(1971,1), end = c(2019,2), freq = 12)
str(goyccts)

#8  Reasign the value of the yen varible from the goyccts data set
#   by conventing the exchange rate of yen that represents 
#   the price of 1 US Dollar in terms of Japanese yen to represent 
#   the price of 1 Yen in terms of US Dollar. 
#   This way if the number increases it represent appreciation of Yen. 
#   Hint: Reassign the value of yen variable by taking a reciprocal. 

goyccts[,'yen']<- 1/goyccts[,'yen']
head(goyccts)

#9  Plot the time series plot of the three assets. Do you see any trend?
#   Do you see any seasonal component?
plot(goyccts)

# All three assets have a positive trend. As for seasonal component,its very hard to see. It looks like there is some seasonal component on this plot.


#10 Utilize the aggregate function to plot annual prices of the three assets.
#   How does this graph differ from the monthly time series plot?

plot(aggregate(goyccts))

# The aggregate plot is smoother and leaves out the seasonality component. 

#11 Find the average summer price of oil for the entire sample.
summer <- goycc[format.Date(goycc$observation_date, "%m") == '06' | format.Date(goycc$observation_date, "%m") == '07' | format.Date(goycc$observation_date, "%m") == '08',]
summer_mean<-mean(summer$oil)
str(summer_mean)
summer_mean

#12 Find the average winter price of oil for the entire sample.
winter <- goycc[format.Date(goycc$observation_date, "%m") == '12' | format.Date(goycc$observation_date, "%m") == '01' | format.Date(goycc$observation_date, "%m") == '02',]
winter_mean<-mean(winter$oil)
str(winter_mean)
winter_mean

#13 How does the summer price of oil compare to the winter price of oil.
#   Please provide your answer in percentages. 

difference_perct<-(summer_mean - winter_mean)/winter_mean
#the difference in percentage:
difference_perct*100



#In addition winter and summer ratio are as follows:

winter.ratio <- mean(winter$oil)/mean(goycc$oil)
winter.ratio*100 #for percent

summer.ratio <- mean(summer$oil)/mean(goycc$oil)
summer.ratio*100 #for percent



#14 Use window() function to create three stand alone variables 
#   "gold", "oil", and "yen" that take on values of the "gold", "oil", and "yen" 
#   variables from the goyccts dataset starting from January of 2005

gold <- window(goyccts[,'gold'], start=c(2005,1))
oil <- window(goyccts[,'oil'], start=c(2005,1))
yen <- window(goyccts[,'yen'], start = c(2005,1))

#15 Use plot() and decompose() functions to generate three graphs that would depict
#   the observed values, trends, seasonal, and random components for "gold"
#   "oil" and "yen" variables. Would you choose multiplicative or 
#   additive decomposition model for each of the variables?

#gold

#I would choose addiditve decomposition model for gold

#plotting gold
plot(gold)

#plotting decompose of gold
decompose_gold<- decompose(gold, type = 'additive')
plot(decompose_gold)

#oil

#I would choose addiditve decomposition model for oil

#plotting oil
plot(oil)

#plotting decompose of oil
decompose_oil<- decompose(oil, type = 'additive')
plot(decompose_oil)

#yen

#I would choose addiditve decomposition model for oil

#plotting yen
plot(yen)

#plotting decompose of oil
decompose_yen<- decompose(yen, type = 'additive')
plot(decompose_yen)

#The reason for using additive decomposition model is because seasonal component does not increase with the trend for all three assets.

#16 For each of the variables extract the random component and save 
#   them as "goldrand", "oilrand", and "yenrand". Moreover, use na.omit()
#   function to deal with the missing values.

#gold

goldrand<-na.omit(decompose_gold$random)
str(goldrand)
goldrand
#oil
oilrand<-na.omit(decompose_oil$random)
str(oilrand)
oilrand
#yen
yenrand<-na.omit(decompose_yen$random)
str(yenrand)
yenrand

#17 For the random component of each of the assets, please estimate 
#   autocorrelation function.Does any of the assets exhibit autocorrelation?
#   If yes, to what degree?
#   Keep in mind there are missing values. 

#gold 
acf(goldrand)

#gold exhibits significant autocorrelation. Also lag value is less than 0.7

#oil
acf(oilrand)

#oil exhibits significant autocorrelation. Also lag value less than 0.7 and lag value= 1.6

#yen
acf(yenrand)

#yen exhibits significant autocorrelation. Also lag value = 0.6,1.1 and 1.6

#18 For all possible pairs of assets please estimate cross-correlation function 
#   Do any of the variable lead or precede each other?
#   Could you use any of the varibales to predict values of other variables?
#   Make sure to use detranded and seasonally adjusted variables. 
#   ("goldrand", "oilrand", and "yenrand")

#gold and oil

ts.plot(goldrand, oilrand, col=c("gold","black"))

ccf(goldrand,oilrand)

#oil and yen 

ts.plot(oilrand, yenrand, col=c("black","green"))
ccf(oilrand, yenrand)

#yen and gold 

ts.plot(yenrand, goldrand, col=c("green","gold"))
ccf(yenrand, goldrand)

#For this instance, leading variables is changing over time periods. Therefore we cannot use the asset variables to predict each other. Also none of the variables lead or precede each other.

#19 Based on the time series plot of gold, oil, and yen prices, 
#   there appears to be no systematic trends or seasonal effects. 
#   Therefore, it is reasonable to use exponential smoothing for these time series.
#   Estimate alpha, the smoothing parameter for gold, oil and yen. 
#   What does the value of alpha tell you tell you about the behavior of the mean? 
#   What is the estimated value of the mean for each asset?

#Using Holt Winter Function with beta and gamma set to 0

#Gold

hw.gold<-HoltWinters(gold, beta=0,gamma=0);hw.gold

#mean for gold=1317.813562

#aplha for gold= 0.9999196 which means there is little smoothing

#Oil

hw.oil<-HoltWinters(oil, beta=0, gamma=0);hw.oil

#mean for oil=60.4241433

#aplha for oil= 0.9999373 which means there is little smoothing

#Yen

hw.yen<-HoltWinters(yen, beta=0, gamma=0);hw.yen

#mean for yen=0.009188601

#aplha for yen= 0.9999335 which means there is little smoothing

#20 Use plot() function to generate three graphs that depict observed 
#   and exponentially smoothed values for each asset.

#Plotting graphs for all three that depict observed and exponentially smoothed values for each asset:

#Gold
plot(hw.gold)

#Oil
plot(hw.oil)

#Yen
plot(hw.yen)


#21 Use window() function to create 3 new variables called 
#   "goldpre", "oilpre", and "yenpre" that covers the period from January 2005, 
#   until August 2018. 

goldpre<-window(gold,start=c(2005,1), end=c(2018,8))

oilpre<-window(oil,start=c(2005,1), end=c(2018,8))

yenpre<-window(yen,start=c(2005,1), end=c(2018,8))

#22 Use window() function to create 3 new variables called 
#   goldpost, oilpost, and yenpost that covers the period from September 2018, 
#   until February 2019.

goldpost<-window(gold, start=c(2018,9), end=c(2019,2))

oilpost<-window(oil, start=c(2018,9), end=c(2019,2))

yenpost<-window(yen, start=c(2018,9), end=c(2019,2))


#23 Estimate HoltWinters filter model for each asset, while using only only pre data.
#   Save each of these estimates as "gold.hw", "oil.hw", and "yen.hw".

gold.hw<- HoltWinters(goldpre, seasonal="additive")

gold.hw

oil.hw<- HoltWinters(oilpre, seasonal="additive")

oil.hw

yen.hw<- HoltWinters(yenpre, seasonal="additive")

yen.hw

#24 Use HoltWinters filter estimates generated in#23 and predict() function 
#   to create a 6 month ahead forecast of the gold, oil, and yen prices. 
#   Save these forcasted values as "goldforc", "oilforc", and "yenforc".

goldforc<-predict(gold.hw, n.head=6)

oilforc<-predict(oil.hw, n.head=6)

yenforc<-predict(yen.hw, n.head=6)

#25 Use ts.plot() function to plot side-by-side post sample prices 
#   ("goldpost", "oilpost","yenpost") and their forecasted counterparts.
#   Please designate red color to represent the actual prices, 
#   and blue doted lines to represent forecasted values. 

par(mfrow=c(3,1))

goldplot<-ts.plot(goldpost, goldforc, lty = 1:2, col = c('red','blue'), ylim = c(1050,1320))

oilplot<-ts.plot(oilpost, oilforc, lty = 1:2, col = c('red','blue'))

yenplot<-ts.plot(yenpost, yenforc, lty = 1:2, col = c('red','blue'))

              
#26 Please calculate forecast mean percentage error for each assets forecasting model. 
#   Which asset's forecasting model has the lowest mean percentage error?

gold_per <- mean(((goldpost - goldforc)/goldpost)*100); gold_per

oil_per <- mean(((oilpost - oilforc)/oilpost)*100); oil_per

yen_per <- mean(((yenpost - yenforc)/yenpost)*100); yen_per

#27 Use gold, oil, and yen variables to estimate HoltWinters model
#   for each asset. Save these estimates as "goldc.hw", "oilc.hw", and "yenc.hw".

goldc.hw <- HoltWinters(gold, seasonal = 'additive')
oilc.hw <- HoltWinters(oil, seasonal = 'additive')
yenc.hw <- HoltWinters(yen, seasonal = 'additive')

#28 Use "goldc.hw", "oilc.hw", and "yenc.hw" models to create an out-of-sample
#   forecasts to predict the prices of each of the assets for the rest of the 2019.
#   Save these forecasts as "goldforcos", "oilforcos", "yenforcos".
#   What is the forecasted price of Gold for November 2019? 

goldforcoc <-predict(goldc.hw, n.ahead = 10)
oilforcoc <- predict(oilc.hw, n.ahead = 10)
yenforcoc <- predict(yenc.hw, n.ahead = 10)

goldforcoc_10 <- goldforcoc['Nov']

#Therefore forecasted price of gold for November,2019 = 1276.936 yen.

#29 Create time series plots for each asset, that combines the actual price data
#   of each asset and their out-of-sample forecasted values.
#   Please designate red color to represent the actual prices, 
#   and blue doted lines to represent forecasted values.
#   What do you think will happen to the price of each asset by the end of the year?

par(mfrow = c(1,3))
goldplot2<- ts.plot(gold, goldforcoc, lty = 1:4, col = c('red','blue'))
oilplot2<- ts.plot(oil, oilforcoc,lty = 1:4, col = c('red','blue'))
yenplot2<- ts.plot(yen, yenforcoc,lty = 1:4, col = c('red','blue'))

#At the end of the year, the price for all three assets will drop

#30 Please calculate percentage between the price of each asset in 
#   February 2019 and their forecasted December 2019 prices. 
#   Which asset promises the highest rate of return? 

gold_feb <- window(gold, start = c(2019,2))[[1]]
gold_dec <- goldforcoc[[10]]
gold_perc <- gold_feb/gold_dec; gold_perc 

# gold decrease by 4.0%.

oil_feb <- window(oil, start = c(2019,2))[[1]]
oil_dec <- oilforcoc[[10]]
oil_perc <- oil_feb/oil_dec; oil_perc 

# oil increase by 5.26%.

yen_feb <- window(yen, start = c(2019,2))[[1]]
yen_dec <- yenforcoc[[10]]
yen_perc <- yen_feb/yen_dec; yen_perc 

# yen increase by 0.17%.

#Therefore the asset that promises the highest rate of return is the yen. 


