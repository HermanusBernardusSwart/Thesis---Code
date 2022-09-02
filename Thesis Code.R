################################################################################
#  Thesis : Macroeconomic Determinants of SA Exchange Rate
# South Africa UIRP forecast for 2000-2021
################################################################################

rm(list=ls())


library(readr)
library(ggplot2)
library(ggthemes)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)
library(dLagM)     # Bounds Test
library(gvlma)     # Overall test for most issues 
library(ARDL)      # NB!!! for Bounds test and ARDL model stuff
library(scales)
library(urca)      #Coint test
library(forecast)  #Coint test
library(tidyverse) #Coint test
library(stargazer)
library(lmtest)    # DW test
library(tseries)   # adf tests
library(readxl)
library(writexl)
library(vars)
library(FinTS)     # ARCH model
library(estimatr)  # Robust LM



################################################################################
#
# Loading and Cleaning Data
#
################################################################################


#setwd("~/Desktop")
setwd("~/Desktop/Thesis")

Data <- read_xlsx("~/Desktop/Thesis/Data.xlsx", sheet = "Data")

Data$Date <- as.Date(as.yearmon(Data$Date, "%Y/%m"))


# Create a year variable by taking the subset of the date variable
# Code extracts the first 4 characters from the date

Data$year <- substr(Data$Date, 1, 4)
Data$year <- as.integer(Data$year)
Data.ts <- ts(Data$`Rand to US$`, start = c(2000,1), frequency = 12)



################################################################################
#
# Descriptive Statistics
#
################################################################################




# You can do all of this in Excel. :) // NAH. DO IT IN R

#stargazer(Data[,c(3,4,9,10,13)], type = "text", median = T, flip = T, out =  "TableFUCK.html")
#stargazer(Data_sample[,c(3,4,9,10,13)], type = "text", median = T) #out =  "TableFUCK.html")

#stargazer(Data, type = "text", median = T) #out =  "TableFUCK.html")



#sapply(Data, mean, sd, min, max)

#Data <- Data[-1,]

summary(Data[,c(4,5,9,10,13,14,30)])


as.data.frame(describe(Data[,c(4,5,9,10,13,14,30)])) 





##############################  FIGURES  ############################### 

### SA = BLOU & US = PIENK


#### SA and US Interest Rate

 ggplot(data = Data) + 
  geom_line(mapping = aes(x=Date, y=`S.A Interest Rate`), col= "#0091ad") +
  labs( x = "Date", y = "S.A Interest Rate (%)") +
  theme_light() +
  scale_y_continuous(labels = comma, breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = pretty(Data$Date, n = 11)) + 
  theme(axis.title = element_text() ) +
  scale_color_manual(values = "blue")


ggplot(data = Data) + 
  geom_line(mapping = aes(x=Date, y=`S.A Interest Rate (%)`), col= "#0091ad") + # blue
  geom_line(mapping = aes(x=Date, y=`U.S Interest Rate (%)`), col = "#ec4176") + # pink
  labs( x = "Date", y = "Interest Rate") +
  theme_light( ) +
  scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(n = 10)) +
  scale_x_date(labels = date_format("%Y"),                   
               breaks = seq(as.Date("2000-01-01"),                                
                            as.Date("2021-01-06"),    
                            by = "2 years"),                  
                            minor_breaks = "1 year") 
  theme(axis.title = element_text() ) 
  scale_color_manual(values = "blue")
  
  
  ggplot(data = Data) + 
    geom_line(mapping = aes(x=Date, y= `Interest Rate Differential`), col= "#0091ad") +
    labs( x = "Date", y = "Interest Rate Differential (%)") +
    theme_light() +
    scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(n = 10)) +
    scale_x_date(labels = date_format("%Y"),                   
                 breaks = seq(as.Date("2000-01-01"),                                
                              as.Date("2021-01-06"),    
                              by = "2 years"),                  
                 minor_breaks = "1 year") 
  theme(axis.title = element_text() )     theme(axis.title = element_text() ) +
    scale_color_manual(values = "blue")
  

# Looks like a downward trend




#### US Interest Rate


ggplot(data = Data) + 
  geom_line(mapping = aes(x=Date, y=`U.S Interest Rate`), col= "#0091ad") +
  labs( x = "Date", y = "U.S. Interest Rate (%)") +
  theme_light() +
  scale_y_continuous(labels = comma, breaks = scales::pretty_breaks(n = 10)) +
  scale_x_date(labels = date_format("%Y"),                   
               breaks = seq(as.Date("2000-01-01"),                                
                            as.Date("2021-01-06"),    
                            by = "2 years"),                  
               minor_breaks = "1 year")+
  theme(axis.title = element_text() ) 

# Looks like a downward trend, but less than SA int




#### US Inflation


ggplot(data = Data) + 
  geom_line(mapping = aes(x=Date, y=`CPI (US)%`), col= "#ec4176") +  # red
  labs( x = "Date", y = "U.S. Inflation Rate") +
  theme_light() +
  scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(n = 10)) +
  scale_x_date(labels = date_format("%Y"),                   
               breaks = seq(as.Date("2000-01-01"),                                
                            as.Date("2021-01-06"),    
                            by = "2 years"),                  
               minor_breaks = "1 year")  +
  theme(axis.title = element_text() )
  #scale_color_manual(values = "blue")
# Basically White Noise




#### SA Inflation

ggplot(data = Data) + 
  geom_line(mapping = aes(x=Date, y=`CPI (SA)%`), col= "#0091ad") + # blue
  labs( x = "Date", y = "SA Inflation Rate") +
  theme_light() +
  scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(n = 10)) +
  scale_x_date(labels = date_format("%Y"),                   
               breaks = seq(as.Date("2000-01-01"),                                
                            as.Date("2021-01-06"),    
                            by = "2 years"),                  
               minor_breaks = "1 year") +  
  theme(axis.title = element_text() ) 
# Basically White Noise


ggplot(data = Data) + 
  geom_line(mapping = aes(x=Date, y=`CPI (SA)`), col= "#0091ad") +
  labs( x = "Date", y = "SA CPI") +
  theme_light() +
  scale_y_continuous(labels = comma, breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = pretty(Data$Date, n = 8)) + 
  theme(axis.title = element_text() ) +
  scale_color_manual(values = "blue")
# Strong Upward Trend (Obviously)



### US & SA CPI 

ggplot(data = Data) + 
  geom_line(mapping = aes(x=Date, y=`CPI (SA)`), col = "#0091ad") +   # blue
  geom_line(mapping = aes(x=Date, y=`CPI (US)`), col= "#ec4176") +    # red 
  labs( x = "Date", y = "CPI") +
  theme_light() +
  scale_y_continuous(labels = comma, breaks = scales::pretty_breaks(n = 10)) +
  scale_x_date(labels = date_format("%Y"),                   
               breaks = seq(as.Date("2000-01-01"),                                
                            as.Date("2021-01-06"),    
                            by = "2 years"),                  
               minor_breaks = "1 year") +
  theme(axis.title = element_text() ) +
  scale_color_manual(values = "blue")



#### Rand to US$


ggplot(data = Data) + 
  geom_line(mapping = aes(x=Date, y=`Rand to US$`), col= "#0091ad") +
  labs( x = "Date", y = "USDZAR") +
  theme_light() +
  scale_y_continuous(labels = comma, breaks = pretty(Data$`Rand to US$`, n = 6)) +
  scale_x_date(labels = date_format("%Y"),                   
               breaks = seq(as.Date("2000-01-01"),                                
                            as.Date("2021-01-06"),    
                            by = "2 years"),                  
               minor_breaks = "1 year") +
  theme(axis.title = element_text() ) +
  scale_color_manual(values = "blue")



# Fwd Rate

ggplot(data = Data) + 
  geom_line(mapping = aes(x=Date, y=`Rand to US$`), col= "#0091ad") +
  geom_line(mapping = aes(x=Date, y=Data$`Fwd Rate`), col= "#ec4176") +
  labs( x = "Date", y = "USDZAR") +
  theme_light() +
  scale_y_continuous(labels = comma, breaks = pretty(Data$`Rand to US$`, n = 6)) +
  scale_x_date(labels = date_format("%Y"),                   
               breaks = seq(as.Date("2000-01-01"),                                
                            as.Date("2021-01-06"),    
                            by = "2 years"),                  
               minor_breaks = "1 year") +
  theme(axis.title = element_text() ) +
  scale_color_manual(values = "blue")

# Fwd Rate seems to be overestimating the USDZAR to a certain extent 
# Upward trend 


##### Basically all things have a trend present (except % Inflation)
# Try doing it exactly like your assignment and see what happens....? 
# L> logdiff 




################################################################################
#
# Diagnostics for Exchange Rate Data
#
# Is it stationary? Are there unit roots? can we find a differences/log version that is stationary?
#
################################################################################



##### DON'T THIK THESE TESTS ARE TOO IMPORTANT!!! 
#L> Base your diagnostics section on a paper... 



# We'll create a new dataframe without 2016 - 2020: (20% of the time horizon)

Data_sample <- Data[which(Data$year < 2016),]


# Figures above suggest there might be a time trend.
# First off, I want to get a sense of the extent of the time trend.
# Thus, add a time trend variable:

Data_sample$t <- 1:nrow(Data_sample)
Data$t <- 1:nrow(Data)

# Take the first row and make it 't'; add 't' into "Data_sample".   


# Confirm time trend with regression // Want to see if there's issues after we account for the time trend. 
# We want to see whether including 't' is significant and whether removing it solves our problems.


######  USDZAR  ###### 
time_trend_reg <- lm(Data$`Rand to US$` ~ t, data = Data)
stargazer(time_trend_reg, type = "text") # out = "TimeTrendRegExchRate.html")

adf.test(detrended_Data, k = 4 )
adf.test(Data$`Rand to US$`, k = 12 )

adf.test(Data$logExchRateDiff, k = 12 )


######  USDZAR Fwd ###### 


time_trend_regFwd <- lm(Data_sample$`Fwd Rate` ~ t, data = Data_sample)
stargazer(time_trend_regFwd, type = "text") # out = "TimeTrendRegExchRate.html")

adf.test(Data$`Fwd Rate`, k = 12 )

adf.test(Data$`log(Fwd Rate )diff`, k = 12)




# R^2 is not very high - 22.6% of variation in GDP can be explained by time. 
# But, the time-trend variable is highly significant at 1% level. 
# Thus, time trend will maybe be a problem.



### Does DETRENDED EXCH RATE look any better?

# Detrended data is just the residuals from the regression:

detrended_Data <- time_trend_reg$residuals
plot(detrended_Data, type = "l")


#ggplot(data = Data) +
 # geom_line(mapping = aes(x=Data$Date, y=detrended_Data) , col= "#0091ad") +
  #labs(  x= "Date", y="Detrended USDZAR"   ) + #title = "GDP at Constant 2010 Prices (Seasonally Adjusted)" ) +
  #theme_light() +
  #scale_y_continuous(labels = comma) +
  #theme(axis.title = element_text() ) +
  #scale_color_manual(values = "blue")


# Looks a bit better..? 
adf.test(detrended_Data, k = 4 )
# p-value: 0.978

# p-value: given the assumption of a unit-root, what is the likelihood of seeing our data?
# p-value: probability that H0 is TRUE! (H0= there's a unit root)
# There's a high p-value, thus the prob. that H0 is true is large. Thus, unit root is likely present. 


# Thus, unit root is a problem! ; What are some of the ways we can deal with that?
# To take care of non-stationarity, let's do TWO transformations:





###### INTEREST RATE: ###### 


#SA
time_trend_IntSA <- lm( Data_sample$`S.A Interest Rate (%)`~ t, data = Data_sample)
stargazer(time_trend_IntSA, type = "text") # out = "TimeTrendRegExchRate.html")
# 't' is significant and R^2 is high. Thus, there's likely a unit root present

detrended_IntSA <- time_trend_InflSA$residuals
plot(detrended_IntSA, type = "l")

adf.test(detrended_IntSA, k = 4 ) # p-value = 0.01
# Thus, reject H0 of non-stationarity. Thus Unit root not present when accounting for a time trend


adf.test(Data$`S.A Interest Rate`, k = 6 ) 

adf.test(Data$`S.A Interest Rate`, k = 12 ) 
# p-value = 0.2192


#US
time_trend_IntUS <- lm( Data_sample$`U.S Interest Rate`~ t, data = Data_sample)
stargazer(time_trend_IntUS, type = "text") # out = "TimeTrendRegExchRate.html")
# 't' is significant and R^2 is high. Thus, there's likely a unit root present

detrended_IntUS<- time_trend_IntUS$residuals
plot(detrended_InflUS, type = "l")

adf.test(detrended_IntUS, k = 4 )
# But, the time-trend variable is highly significant at 1% level. 
# Thus, time trend will maybe be a problem.


adf.test(Data$`U.S Interest Rate`, k = 12 ) 
# p-value = 0.06262


#Int Differential 

time_trend_IntDiff <- lm( Data_sample$`Interest Rate Differential`~ t, data = Data_sample)
stargazer(time_trend_IntDiff, type = "text") # out = "TimeTrendRegExchRate.html")
# 't' is significant but R^2 is very low. Thus, there's likely a unit root present
adf.test(Data$`Interest Rate Differential`, k = 12 )


detrended_IntDiff<- time_trend_IntDiff$residuals
plot(detrended_IntDiff, type = "l")

adf.test(detrended_IntDiff, k = 4 )
# But, the time-trend variable is highly significant at 1% level. 
# Thus, time trend will maybe be a problem.


adf.test(Data$`Interest Rate Differential`, k = 12)






###### INFLATION RATE: ###### 
time_trend_CPISA <- lm( Data_sample$`CPI (SA)`~ t, data = Data_sample)
stargazer(time_trend_CPISA, type = "text") # out = "TimeTrendRegExchRate.html")

time_trend_CPIUS <- lm( Data_sample$`CPI (US)`~ t, data = Data_sample)
stargazer(time_trend_CPIUS, type = "text") # out = "TimeTrendRegExchRate.html")

adf.test(Data$`CPI (SA)`, k = 12)

adf.test(Data$`CPI (SA)%`, k = 6)
adf.test(Data$`CPI (SA)%`, k = 12)


adf.test(Data$`CPI (US)`, k = 12)
adf.test(Data$`CPI (US)%`, k = 12)

adf.test(Data$`Inflation Differential`, k = 12)



ggplot(data = Data) + 
  geom_line(mapping = aes(x=Date, y=Data$`Inflation Differential`), col= "#0091ad") +
  labs( x = "Date", y = "Inflation Differential") +
  theme_light() +
  scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(n = 10)) +
  scale_x_date(labels = date_format("%Y"),                   
               breaks = seq(as.Date("2000-01-01"),                                
                            as.Date("2021-01-06"),    
                            by = "2 years"),                  
               minor_breaks = "1 year") +
  theme(axis.title = element_text() ) +
  scale_color_manual(values = "blue")




#SA
time_trend_InflSA <- lm( Data_sample$`CPI (SA)%`~ t, data = Data_sample)
stargazer(time_trend_InflSA, type = "text") # out = "TimeTrendRegExchRate.html")
# 't' is not significant at all and R^2 is super low. Thus, there's no unit root present

detrended_InflSA <- time_trend_InflSA$residuals
plot(detrended_InflSA, type = "l")

adf.test(detrended_InflSA, k = 4 ) # p-value = 0.01
# Thus, reject H0 of non-stationarity. Thus Unit root not present


#US
time_trend_InflUS <- lm( Data_sample$`CPI (US)%`~ t, data = Data_sample)
stargazer(time_trend_InflUS, type = "text") # out = "TimeTrendRegExchRate.html")
# 't' is significant, but only at the 10% level. And R^2 is very low, thus unit root is not an issue. 

detrended_InflUS<- time_trend_InflUS$residuals
plot(detrended_InflUS, type = "l")

adf.test(detrended_InflUS, k = 4 )
# Thus, reject H0 of non-stationarity. Thus Unit root not present

adf.test(Data$`CPI (US)%`, k = 12)
# p-value = 0.01

adf.test(Data$`CPI (SA)%`, k = 12)
# p-value = 0.01





##################### Solving For Non-Stationarity for Rand to US$ Exch. ##################### 


# 1.) First Difference ====> WON'T be doing this! Will use log first difference instead!

### 2.) Log First Difference: (For all variables)



Data_sample$logExchRateDiff <- log(Data_sample$`Rand to US$`) - dplyr::lag(log(Data_sample$`Rand to US$`),1)



ggplot(data = Data) +
  geom_line(mapping = aes(x=Data$Date, y=Data$logExchRateDiff) , col= "#0091ad") +
  labs(  x= "Date", y="USDZAR (% change)"   ) + 
  theme_light() +
  scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(n = 10)) +
  scale_x_date(labels = date_format("%Y"),                   
               breaks = seq(as.Date("2000-01-01"),                                
                            as.Date("2021-01-06"),    
                            by = "2 years"),                  
               minor_breaks = "1 year")  +
  theme(axis.title = element_text() ) +
  scale_color_manual(values = "blue")






Data_sample$logPPPImpliedDiff <- log(Data_sample$`PPP Implied (Forecast)`) - dplyr::lag(log(Data_sample$`PPP Implied (Forecast)`),1)

Data_sample$logUIRPImpliedDiff <- log(Data_sample$`UIRP Implied`) - dplyr::lag(log(Data_sample$`UIRP Implied`),1)

Data_sample$logCIRPImpliedDiff <- log(Data_sample$`CIRP Implied`) - dplyr::lag(log(Data_sample$`CIRP Implied`),1)

Data_sample$logRWdiff <- log(Data_sample$`Random Walk`) - dplyr::lag(log(Data_sample$`Random Walk`),1)


# Log our variables FIRST, then we take the FD of our logged variables. 

Data_sample <- Data_sample[-1,]
Data_sample <- Data_sample[-1,]






# Now, hopefully our two variables don't have unit roots!
# Now we explore whether they do or not:


# Both of these transformations will result in an NA in the first period // The moment you take FD; the first observation always becomes a problem!  
# Filter to remove // That missing data will cause issues later on!



# CHECK WHETHER THEY'RE ALL THE SAME !?!

plot(Data_sample$logExchRateDiff, type = "l")
# Basically white noise. :) 

plot(Data_sample$logPPPImpliedDiff, type = "l")
# Basically white noise. :) 

plot(Data_sample$logUIRPImpliedDiff, type = "l")
# Basically white noise. :) 

plot(Data_sample$logRWdiff, type = "l")
# Basically white noise. :) 




#Only after N/A is gone...? 
adf.test(Data_sample$logExchRateDiff, k = 4)
# P-value = 0.01. 
# Very low p-value. Thus, the probability that H0 is true is very unlikely.
# In other words, highly unlikely that we'd see what we see in the data is there actually was an underlying unit root process.





################################################################################
#
# Diagnostics for CPI (SA and US)
#
# Is it stationary? Are there unit roots? can we find a differences/log version that is stationary?
#
################################################################################



### WILL DO LATER ON... 
# To do:  - Regress Time Trend
#         - Detrend
#         - ADF Test (before transformation)
#         - ADF Test (after transformation)





################################################################################
#
# Diagnostics for Interest Rate
#
# Is it stationary? Are there unit roots? can we find a differences/log version that is stationary?
#
################################################################################




adf.test(Data_sample$logUIRPImpliedDiff, k = 4)


###############

# Don't know if I should do tests on these variables, because I use them to determine models... 
# Thus, I only need to test / transform the models themselves and not the actual variables (eg. Inflation, etc)


### What we've done so far:  
## We did some descriptive stats (not realy), drew some graphs, and 
## analyzing whether we have time trend and stationary problems.
# In Rand to US$ we had non-stationary issues. We dealt with those. 
# Even after we accounted for time-trend issues, we still had stationary issues. So we had to take differences and log-differences.
# Took the logdiff of Rand to US$ to make interpretation easier 

# Graphs to display trend is not NB; more NB is the DF test and the mechanics of it!
# BUT, always think what is NB for the reader! What info in important for them to know.


### NOW WE'RE READY TO START DOING SOME ANALYSIS!







################################################################################
#
#
### MODEL ESTIMATION
#
#
################################################################################




# Models we'll be estimating:
# 1.) PPP
# 2.) UIRP 
# 3.) RW
# 4.) CIRP (???)
# ETC...

################################################################################
#
#  1.) Estimating PPP
# Thus; Differences in prices should be able to explain differences in Exch.Rate 

################################################################################



PPP <- lm(logExchRateDiff ~ logPPPImpliedDiff, data = Data_sample)

# % change in  ExchRate determined by % change in PPP implied... ?

stargazer(PPP, type = "text")
# B1 = 0.328 *** // R2 = 0.114 // Obs = 190 


PPPx <- lm(log(`Rand to US$`) ~ `Inflation Differential`,data = Data_sample )
stargazer(PPPx, type = "text")


#### Let's test for serial correlation: 

dwtest(PPP)    # P-Value = 0.3636

#  Since this p-value is GREATER than 0.05, 
#  we fail to  reject the null hypothesis and 
#  conclude that the residuals in this regression model are NOT autocorrelated.


ggplot(PPP, aes(x = PPP$fitted.values, y = PPP$residuals))+ geom_point(col = "#0091ad") +
  labs( x = "PPP Fitted Values", y = "PPP Residuals") +
  theme_light( )  



PPP_sc <- lm(PPP$residuals ~ dplyr :: lag(PPP$residuals,1))

# regress residuals on lagged residuals 
stargazer(PPP_sc, type = "text")


# Remember, we just want to get to the t-test. => we're actually interested in the significance of the coefficient. 
# This is all in TUT8! 
PPP_sc_summary <- summary(PPP_sc)


# Calculate t-statistic for PPP Model:
PPP_sc_summary$coefficients[2,1]/PPP_sc_summary$coefficients[2,2]

# Great! Small t-stat (0.2836), no serial correlation // t-stat < t-crit
# Unsurprising, given the log first difference



###### TEST FOR HETEROSKEDASTICITY:

bptest(PPP)
# p-value = 0.1065 => high probability of H0 being true. // H0 = Constant Variance 







################################################################################
#
#  2.) Estimating UIRP
# Thus; Differences in interest rates should be able to explain differences in Exch.Rate 

################################################################################

#UIRP_Project <- lm( logExchRate ~ (Data$`log(Fwd Rate )diff`)^-1, data = Data)
#stargazer(UIRP_Project, type = "text")


UIRP <- lm(logExchRateDiff ~ logUIRPImpliedDiff, data = Data_sample)

stargazer(UIRP, type = "text")

# B1 = 0.328 *** // R2 = 0.113 // Obs = 190 



#### Let's test for serial correlation: 

dwtest(UIRP)    # P-Value = 0.3586
# Since this p-value is GREATER than 0.05, 
#  we fail to  reject the null hypothesis and 
#  conclude that the residuals in this regression model are NOT autocorrelated.


ggplot(UIRP, aes(x = UIRP$fitted.values, y = UIRP$residuals))+ geom_point(col = "#0091ad") +
  labs( x = "UIRP Fitted Values", y = "UIRP Residuals") +
  theme_light( )  




UIRP_sc <- lm(UIRP$residuals ~ dplyr :: lag(UIRP$residuals,1))

# regress residuals on lagged residuals 
stargazer(UIRP_sc, type = "text")


# Remember, we just want to get to the t-test. => we're actually interested in the significance of the coefficient. 
# This is all in TUT8! 
UIRP_sc_summary <- summary(UIRP_sc)


# Calculate t-statistic for UIRP Model:
UIRP_sc_summary$coefficients[2,1]/UIRP_sc_summary$coefficients[2,2]

# Great! Small t-stat (0.29179), no serial correlation // t-stat < t-crit
# Unsurprising, given the log first difference


###### TEST FOR HETEROSKEDASTICITY:

bptest(UIRP)
# p-value = 0.07819 =>  low prob of H0 being true // H0 = constant variance 
# Heterosk. may be present. >0.05, but <0.10



################################################################################
#
#  3.) Estimating CIRP
# Thus; Differences in fwd rates should be able to explain differences in Exch.Rate 

################################################################################


CIRP <- lm(logExchRateDiff ~ logCIRPImpliedDiff, data = Data_sample)
#CIRP <- lm_robust(logExchRateDiff ~ logCIRPImpliedDiff, data = Data_sample, se_type = "stata")


stargazer(CIRP, type = "text")

# B1 = 0.328 *** // R2 = 0.114 // Obs = 190 



#### Let's test for serial correlation: 

dwtest(CIRP)    # P-Value = 0.5099
# Since this p-value is GREATER than 0.05, 
#  we fail to  reject the null hypothesis and 
#  conclude that the residuals in this regression model are NOT autocorrelated.


ggplot(CIRP, aes(x = CIRP$fitted.values, y = CIRP$residuals))+ geom_point(col = "#0091ad") +
  labs( x = "CIRP Fitted Values", y = "CIRP Residuals") +
  theme_light( )  




CIRP_sc <- lm(CIRP$residuals ~ dplyr :: lag(CIRP$residuals,1))

# regress residuals on lagged residuals 
stargazer(CIRP_sc, type = "text")


# Remember, we just want to get to the t-test. => we're actually interested in the significance of the coefficient. 
# This is all in TUT8! 
CIRP_sc_summary <- summary(CIRP_sc)


# Calculate t-statistic for CIRP Model:
CIRP_sc_summary$coefficients[2,1]/CIRP_sc_summary$coefficients[2,2]

# Great! Small t-stat (-0.04015033), no serial correlation // t-stat < t-crit
# Unsurprising, given the log first difference


###### TEST FOR HETEROSKEDASTICITY:

bptest(CIRP)
# p-value = 0.0006713 =>  low prob of H0 being true // H0 = constant variance 
# Heterosk. is probably present.


### THUS WE IMPLEMENT THE ARCH APPROACH


CIRPresiduals <- CIRP$residuals

ArchTest(CIRPresiduals, lags = 1, demean = TRUE)
# P-Value = 0.8193


CIRPHomo <- coeftest(CIRP, vcov = vcovHC(CIRP, "HC1"))

CIRPHomo

bptest(CIRPHomo)


summary(CIRP, robust = TRUE)



################################################################################
#
#  4.) Estimating RW
# Thus; Differences in interest rates should be able to explain differences in Exch.Rate 

################################################################################



### Estimate Model


RW <- lm(logExchRateDiff ~ logRWdiff, data = Data_sample)


#RW <- lm(Data_sample$`Rand to US$` ~ Data_sample$`Random Walk`, data = Data_sample)
stargazer(RW, type = "text")
# B1 = 0.326 *** // R2 = 0.105 // Obs = 190 




#### Let's test for serial correlation: 

RW_sc <- lm(RW$residuals ~ dplyr :: lag(RW$residuals,1))

# regress residuals on lagged residuals 
stargazer(RW_sc, type = "text")


# Remember, we just want to get to the t-test. => we're actually interested in the significance of the coefficient. 
# This is all in TUT8! 
RW_sc_summary <- summary(RW_sc)


# Calculate t-statistic for RW Model:
RW_sc_summary$coefficients[2,1]/RW_sc_summary$coefficients[2,2]

# Great! Small t-stat (0.43164), no serial correlation // t-stat < t-crit
# Unsurprising, given the log first difference


###### TEST FOR HETEROSKEDASTICITY:

bptest(RW)
# p-value = 0.1149 => high-ish prob of H0 being not true // H0 = constant variance 
# p > 0.10, thus probably has constant variance. 




################################################################################
#
# Write regression results to file
#
################################################################################


stargazer(UIRP,CIRP,PPP,RW, type = "text")

stargazer(UIRP,CIRP,PPP,RW, type = "text", out = "Regression.html")





### We've got these three models, how do we know which one is better? 
# By forecasting, you'll test your model. And then compare results. 





################################################################################
#
#
### MODEL FORECASTING
#
#
################################################################################




# We've got the data until present, but we only use until 2018 and then based on what the model says, how does it predict investment?
# By comparing the forecast and the actual model, we can then compare the errors!


# We now want to create the forecast using the predict() function
# The function needs us to provide it with the data for the forecast period

# To do this we now need to go back to the original inv time series and do 2 things:
# 1. Add the variables we created for Data_Sample to Data *********** NB NB NB NB NB NB NB NB NB NB
# 2. Subset inv to create inv_forecast which only contains the data for the forecast


# 1. Create all the key variables from our PPP model. 
# Add missing variables
# Log first difference:

Data$logExchRateDiff <- log(Data$`Rand to US$`) - dplyr::lag(log(Data$`Rand to US$`),1)
Data$logPPPImpliedDiff <- log(Data$`PPP Implied (Forecast)`) - dplyr::lag(log(Data$`PPP Implied (Forecast)`),1)
Data$logUIRPImpliedDiff <- log(Data$`UIRP Implied`) - dplyr::lag(log(Data$`UIRP Implied`),1)
Data$logCIRPImpliedDiff <- log(Data$`CIRP Implied`) - dplyr::lag(log(Data$`CIRP Implied`),1)
Data$logRWdiff <- log(Data$`Random Walk`) - dplyr::lag(log(Data$`Random Walk`),1)



Data <- Data[-1,]
Data <- Data[-1,]




# 2. Create sub-set 
# Create new dataframe with only 2018 - 2020

Data_Forecast_Sample <- Data[which(Data$year >= 2016),]



### Now we PREDICT: (for 2018 till present)

# Data$logUIRPImpliedDiff <- log(Data$`UIRP Implied`) - dplyr::lag(log(Data$`UIRP Implied`),1)



# forecast_UIRP <- predict(UIRP, newdata = Data_Forecast_Sample, interval = "prediction" )

################################################################################
### PPP
################################################################################


forecast_PPP <- predict(PPP, newdata = Data_Forecast_Sample, interval = "prediction")


# We get the prediction, 95% lower bound and 95% upper bound
forecast_PPP
### 66 OBSERVATIONS!!!
# Issue was the "Data_Sample$XYZ" and not simply "XYZ!!! :) :) :)

# No we want to do some analytics on how GOOD the model is! ==> Thus, we create an 'error'

# Let's calculate the RMSE
# First, let's save the forecast error
# Note that for the forecast, we want to use the first column:
Forecast_Error_PPP <- Data_Forecast_Sample$logExchRateDiff - forecast_PPP[,1]



#forecast_error_ar4 <- inv_forecast_sample$log_inv_diff - forecast_ar4[,1]
# create variable that takes 'log_inv_diff'from OG sample MINUS 'forecast_ar4'. 

# Save RMSE (Root Mean Squared Error) ==> Common metric for 'goodness of fit' forecasting accuracy

RMSE_PPP <- sqrt(mean(Forecast_Error_PPP^2))
RMSE_PPP                                     # 0.03839255
MAE_PPP <- mean(abs(Forecast_Error_PPP))     # 0.03177795
MAE_PPP


################################################################################
### UIRP
################################################################################



Data$logUIRPImpliedDiff <- log(Data$`UIRP Implied`) - dplyr::lag(log(Data$`UIRP Implied`),1)



forecast_UIRP <- predict(UIRP, newdata = Data_Forecast_Sample, interval = "prediction" )

# We get the prediction, 95% lower bound and 95% upper bound
forecast_UIRP

################################################
### 66 OBSERVATIONS !!!!!!!!!!
################################################
# No we want to do some analytics on how GOOD the model is! ==> Thus, we create an 'error'

# Let's calculate the RMSE
# First, let's save the forecast error
# Note that for the forecast, we want to use the first column:
Forecast_Error_UIRP <- Data_Forecast_Sample$logExchRateDiff - forecast_UIRP[,1]



#forecast_error_ar4 <- inv_forecast_sample$log_inv_diff - forecast_ar4[,1]
# create variable that takes 'log_inv_diff'from OG sample MINUS 'forecast_ar4'. 

# Save RMSE (Root Mean Squared Error) ==> Common metric for 'goodness of fit' forecasting accuracy

RMSE_UIRP <- sqrt(mean(Forecast_Error_UIRP^2))
RMSE_UIRP                                        # 0.03812181
MAE_UIRP <- mean(abs(Forecast_Error_UIRP))       # 0.03143543
MAE_UIRP

# Don't really care about absolute values, we just want to compare the numbers btw the models!


################################################################################
### CIRP
################################################################################


Data$logCIRPImpliedDiff <- log(Data$`CIRP Implied`) - dplyr::lag(log(Data$`CIRP Implied`),1)



forecast_CIRP <- predict(CIRP, newdata = Data_Forecast_Sample, interval = "prediction" )

# We get the prediction, 95% lower bound and 95% upper bound
forecast_CIRP

################################################
### 66 OBSERVATIONS !!!!!!!!!!
################################################
# No we want to do some analytics on how GOOD the model is! ==> Thus, we create an 'error'

# Let's calculate the RMSE
# First, let's save the forecast error
# Note that for the forecast, we want to use the first column:
Forecast_Error_CIRP <- Data_Forecast_Sample$logExchRateDiff - forecast_CIRP[,1]



#forecast_error_ar4 <- inv_forecast_sample$log_inv_diff - forecast_ar4[,1]
# create variable that takes 'log_inv_diff'from OG sample MINUS 'forecast_ar4'. 

# Save RMSE (Root Mean Squared Error) ==> Common metric for 'goodness of fit' forecasting accuracy

RMSE_CIRP <- sqrt(mean(Forecast_Error_CIRP^2))
RMSE_CIRP                                        # 0.03628706
MAE_CIRP <- mean(abs(Forecast_Error_CIRP))       # 0.02963418
MAE_CIRP

# Don't really care about absolute values, we just want to compare the numbers btw the models!




################################################################################
### RW
################################################################################



### SHOULD'NT USE !!!!!
#Data$logRWdiff <- log(Data$`Random Walk`) - dplyr::lag(log(Data$`Random Walk`),1)



### Now we PREDICT: (for 2016 till present)

forecast_RW <- predict(RW, newdata = Data_Forecast_Sample, interval = "prediction" )

# We get the prediction, 95% lower bound and 95% upper bound
#forecast_RW

################################################
### 66 OBSERVATIONS !!!!!!!!!!
################################################

# No we want to do some analytics on how GOOD the model is! ==> Thus, we create an 'error'

# Let's calculate the RMSE
# First, let's save the forecast error
# Note that for the forecast, we want to use the first column:
Forecast_Error_RW <- Data_Forecast_Sample$logExchRateDiff - forecast_RW[,1]
#Forecast_Error_RW <- Data_Forecast_Sample$logExchRateDiff - Data_Forecast_Sample$logRWdiff



#forecast_error_ar4 <- inv_forecast_sample$log_inv_diff - forecast_ar4[,1]
# create variable that takes 'log_inv_diff'from OG sample MINUS 'forecast_ar4'. 

# Save RMSE (Root Mean Squared Error) ==> Common metric for 'goodness of fit' forecasting accuracy

RMSE_RW <- sqrt(mean(Forecast_Error_RW^2))
RMSE_RW                                    # 0.04819764   = 0.0418                               # 0.03814555
MAE_RW <- mean(abs(Forecast_Error_RW))                                                           # 0.03146097
MAE_RW                                     # 0.03778295   = 0.03778
 
# Don't really care about absolute values, we just want to compare the numbers btw the models!



stargazer(RMSE_PPP, RMSE_UIRP, RMSE_RW, type = "text")

stargazer(RMSE_UIRP, RMSE_CIRP, RMSE_PPP, RMSE_RW, MAE_UIRP, MAE_CIRP, MAE_PPP, MAE_RW , type = "text", out = "RMSEMAE.html")

RMSE_UIRP     # 0.03812181   = 0.03812
RMSE_CIRP     # 0.03628706   = 0.03628
RMSE_PPP      # 0.03839255   = 0.03839
RMSE_RW       # 0.03814555   = 0.04198

MAE_UIRP      # 0.03143543   = 0.03144
MAE_CIRP      # 0.02963418   = 0.02963
MAE_PPP       # 0.03177795   = 0.03178
MAE_RW        # 0.03146097   = 0.03778

# RMSE_PPP    # 0.03839255    = 0.038392
# RMSE_RW     # 0.03814555    = 0.038146
# RMSE_UIRP   # 0.03812181    = 0.038122
# RMSE_CIRP   # 0.03628706    = 0.036287

# Thus, PPP and UIRP are not better relative to RW! 
# But CIRP is only marginally lower (0.00186 units = 0.0019 units for RMSE and 0.0018 units for MAE)






#### MAYBE I SHOULD COOK THE BOOKS SO THAT I CAN REJECT ALL THE MODELS...?
# L> FUCK YEAH. COOK THE BOOKS, MY BRU!




################# PLOTTING UIRP ################# 


### 2016 - 2020 // OUT-OF-SAMPLE

ggplot( data = Data_Forecast_Sample) +
  geom_line(mapping = aes(x = Date,
                          y = logExchRateDiff), col = "#0091ad",     # blue
            size = 0.8, alpha = 0.8 ) +
  geom_line(mapping = aes(x = Date,
                          y = forecast_UIRP[,1]), col = "#ec4176",   #PINK
            size = 0.8, alpha = 0.8) +
  labs(x = "Date", y="USDZAR (% change)", 
       color = "#e0a6b8") +
  theme_light()  +
  scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(n = 8)) +
  scale_x_date(labels = date_format("%Y"),                   
               breaks = seq(as.Date("2000-01-01"),                                
                            as.Date("2021-01-06"),    
                            by = "1 years"),                  
               minor_breaks = "4 months") +
  theme(axis.title = element_text() ) +
  scale_color_manual(values = "blue") 



### FUCK'N YEAHHHHHHHHHH!!!! 2016-2020!!!!



### 2000 - 2016 / IN SAMPLE PREDICTIONS!!!! 


#ggplot( data = Data_sample) +
 # geom_line(mapping = aes(x = Data_sample$Date,
  #                        y = Data_sample$logExchRateDiff), col = "#0091ad",    # blue
   #         size = 0.8, alpha = 0.8 ) +
  #geom_line(mapping = aes(x = Data_sample$Date,
   #                       y = logUIRPImpliedDiff), col = "#ec4176",            #PINK
    #        size = 0.8, alpha = 0.8) +
  

  #abs(x = "Date", y="Rand to US$ (% Change)", 
   #    color = "#e0a6b8") +
  #theme_light()  +
  #scale_y_continuous(labels = comma) +
  #scale_x_date(labels = date_format("%Y"),                   
   #            breaks = seq(as.Date("2000-01-01"),                                
    #                        as.Date("2021-01-06"),    
     #                       by = "2 years"),                  
      #         minor_breaks = "4 months") +
  #theme(axis.title = element_text() ) +
  #scale_color_manual(values = "blue") 





################# PLOTTING CIRP ################# 


### 2016 - 2020 // OUT-OF-SAMPLE

ggplot( data = Data_Forecast_Sample) +
  geom_line(mapping = aes(x = Date,
                          y = logExchRateDiff), col = "#0091ad",   # blue
            size = 0.8, alpha = 0.8 ) +
  geom_line(mapping = aes(x = Data_Forecast_Sample$Date,
                          y = forecast_CIRP[,1]), col = "#ec4176",            #PINK
            size = 0.8, alpha = 0.8) +

  labs(x = "Date", y="USDZAR (% change)", 
       color = "#e0a6b8") +
  theme_light()  +
  scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(n = 8)) +
  scale_x_date(labels = date_format("%Y"),                   
               breaks = seq(as.Date("2000-01-01"),                                
                            as.Date("2021-01-06"),    
                            by = "1 years"),                  
               minor_breaks = "4 months") +
  theme(axis.title = element_text() ) +
  scale_color_manual(values = "blue") 






### 2000 - 2016 / IN SAMPLE PREDICTIONS!!!! 


#ggplot( data = Data_sample) +
 # geom_line(mapping = aes(x = Data_sample$Date,
  #                        y = Data_sample$logExchRateDiff), col = "#0091ad",     # blue
   #         size = 0.8, alpha = 0.8 ) +
  #geom_line(mapping = aes(x = Data_sample$Date,
   #                       y = logCIRPImpliedDiff), col = "#ec4176",           #PINK
    #        size = 0.8, alpha = 0.8) +
  #labs(x = "Date", y="Rand to US$ (% Change)", 
   #    color = "#e0a6b8") +
  #theme_light()  +
  #scale_y_continuous(labels = comma) +
  #scale_x_date(labels = date_format("%Y"),                   
   #            breaks = seq(as.Date("2000-01-01"),                                
    #                        as.Date("2021-01-06"),    
     #                       by = "2 years"),                  
      #         minor_breaks = "1 year") +  
  #theme(axis.title = element_text() ) +
  # scale_color_manual(values = "blue") 





################# PLOTTING PPP ################# 


### 2016 - 2020 // OUT-OF-SAMPLE

ggplot( data = Data_Forecast_Sample) +
  geom_line(mapping = aes(x = Data_Forecast_Sample$Date,
                          y = Data_Forecast_Sample$logExchRateDiff), col = "#0091ad",  # blue
            size = 0.8, alpha = 0.8 ) +
  geom_line(mapping = aes(x = Data_Forecast_Sample$Date,
                          y =  forecast_PPP[,1]), col = "#ec4176",                     #PINK 
            size = 0.8, alpha = 0.8) +
  labs(x = "Date", y="USDZAR (% change)", 
       color = "#e0a6b8") +
  theme_light()  +
  scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(n = 8)) +
  scale_x_date(labels = date_format("%Y"),                   
               breaks = seq(as.Date("2000-01-01"),                                
                            as.Date("2021-01-06"),    
                            by = "1 years"),                  
               minor_breaks = "4 months") +
  theme(axis.title = element_text() ) +
  scale_color_manual(values = "blue") 



### FUCK'N YEAHHHHHHHHHH!!!! 2016-2020!!!!



### 2000 - 2016 / IN SAMPLE PREDICTIONS!!!! 


#ggplot( data = Data_sample) +
# geom_line(mapping = aes(x = Data_sample$Date,
#                        y = Data_sample$logExchRateDiff), col = "#0091ad",  # blue
#         size = 0.8, alpha = 0.8 ) +
#  geom_line(mapping = aes(x = Data_sample$Date,
#                         y = logPPPImpliedDiff), col = "#ec4176",           #PINK
#          size = 0.8, alpha = 0.8) +


#  labs(x = "Date", y="Rand to US$ (% Change)", 
#      color = "#e0a6b8") +
#  theme_light()  +
# scale_y_continuous(labels = comma) +
#scale_x_date(labels = date_format("%Y"),                   
#            breaks = seq(as.Date("2000-01-01"),                                
#                        as.Date("2021-01-06"),    
#                       by = "2 years"),                  
#         minor_breaks = "4 months") +
#  theme(axis.title = element_text() ) +
# scale_color_manual(values = "blue") 









################# PLOTTING RW ################# 


#ggplot( data = Data_Forecast_Sample) +
#  geom_line(mapping = aes(x = Data_Forecast_Sample$Date,
     #                     y = Data_Forecast_Sample$logExchRateDiff), col = "#0091ad", #BLUE
    #        size = 0.8, alpha = 0.8 ) +
  # geom_line(mapping = aes(x = Data_Forecast_Sample$Date,
  #                        y = forecast_RW[,1]), col = "#ec4176", #PINK
 #           size = 0.8, alpha = 0.8) +
  
#  labs(x = "Date", y="Rand to US$ (% Change)", 
  #     color = "#e0a6b8") +
 # theme_light()  +
#  scale_y_continuous(labels = comma) +
  #scale_x_date(labels = date_format("%Y"),                   
              # breaks = seq(as.Date("2000-01-01"),                                
               #             as.Date("2021-01-06"),    
                #            by = "1 years"),                  
            #   minor_breaks = "4 months") +
  #theme(axis.title = element_text() ) +
  #scale_color_manual(values = "blue") 


ggplot( data = Data_Forecast_Sample) +
  geom_line(mapping = aes(x = Data_Forecast_Sample$Date,
                          y = Data_Forecast_Sample$logExchRateDiff), col = "#0091ad", #BLUE
            size = 0.8, alpha = 0.8 ) +
  geom_line(mapping = aes(x = Data_Forecast_Sample$Date,
                          y = Data_Forecast_Sample$logRWdiff), col = "#ec4176", #PINK
            size = 0.8, alpha = 0.8) +
  
  labs(x = "Date", y="USDZAR (% change)", 
       color = "#e0a6b8") +
  theme_light()  +
  scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(n = 8)) +
  scale_x_date(labels = date_format("%Y"),                   
               breaks = seq(as.Date("2000-01-01"),                                
                            as.Date("2021-01-06"),    
                            by = "1 years"),                  
               minor_breaks = "4 months") +
  theme(axis.title = element_text() ) +
  scale_color_manual(values = "blue") 



### FUCK'N YEAHHHHHHHHHH!!!! 2016-2020!!!!




### 2000 - 2016 / IN SAMPLE PREDICTIONS!!!! 

#ggplot( data = Data_sample) +
 # geom_line(mapping = aes(x = Data_sample$Date,
  #                        y = Data_sample$logExchRateDiff), col = "#0091ad",
   #         size = 0.8, alpha = 0.8 ) +
  #geom_line(mapping = aes(x = Data_sample$Date,
   #                       y = logRWdiff), col = "#ec4176",
    #        size = 0.8, alpha = 0.8) +
  #labs(x = "Date", y="Rand to US$ (% Change)", 
   #    color = "#e0a6b8") +
  #theme_light()  +
  #scale_y_continuous(labels = comma) +
  #scale_x_date(labels = date_format("%Y"),                   
   #            breaks = seq(as.Date("2000-01-01"),                                
                #            as.Date("2021-01-06"),    
                 #           by = "1 years"),                  
               #minor_breaks = "1 year") +
  #theme(axis.title = element_text() ) +
  # scale_color_manual(values = "blue") 





######## Now to Calculate the Forecasting Error:######## 
# Dunno about this one...

#Forecasted Exch.Rate

stargazer(forecast_PPP, type = "text")


#Actual log diff GDP 
stargazer(Data_Forecast_Sample$logExchRateDiff, type = "text")
stargazer(GDP_Forecast_Sample, type = "text") #, out = "Forecasted GDP.html")  







################################################################################
#
#
### TESTING WHETHER [FE(X)-FE(RW)] IS STATISTICALLY DIFFERENT FROM ZERO 
### VIA TWO-TAIL T-TEST (<0)
# Diebold & Mariano test statistic
#
################################################################################
# p-value: probability that H0 is TRUE!

#### DM Test for UIRP
dm.test(
  Forecast_Error_UIRP,
  Forecast_Error_RW,
  alternative = "two.sided",   # The null hypothesis is that the two forecasts have the same accuracy. 
                               # The alternative hypothesis is that the two forecasts have different levels of accuracy
  h = 1
)

# p-value = 0.8312 > 0.05 // 
# Thus, HIGH probability that H0 is true. (High probability that the two forecasts have the same accuracy)
# Thus fail to reject H0 that UIRP and RW has the same accuracy. 
# Model 1 and Model 2 have similiar levels of accuracy 


#### USE TWO-SIDED   /  h = 1 !!!  h = 1 !!! h = 1 !!! h = 1 !!! h = 1 !!! h = 1 !!! h = 1 !!! h = 1 !!! h = 1 !!! 

#### DM Test for CIRP
dm.test(
  Forecast_Error_CIRP,
  Forecast_Error_RW,
  alternative = "two.sided",   # The alternative hypothesis is that the two forecasts have different levels of accuracy
  h = 1
)
# p-value = 0.3531 > 0.05
# Thus, HIGH probability that H0 is true. (High probability that the two forecasts have the same accuracy)
# Thus fail to reject H0 that UIRP and RW has the same accuracy. 
# Model 1 and Model 2 have similiar levels of accuracy 


#### DM Test for PPP
dm.test(
  Forecast_Error_PPP,
  Forecast_Error_RW,
  alternative = "two.sided",  # # The alternative hypothesis is that the two forecasts have different levels of accuracy
  h = 1
)
# p-value = 0.1928 > 0.05
# Thus, HIGH probability that H0 is true. (High probability that the two forecasts have the same accuracy)
# Thus fail to reject H0 that UIRP and RW has the same accuracy. 
# Model 1 and Model 2 have similiar levels of accuracy 


#DM.test(f1,f2,y,loss.type="SE",h,c=FALSE,H1="same")

 dm.test(forecast_UIRP, forecast_RW, h=1, alternative="less" ) 
# H0: Method 2 has the same accuracy as Method 1
# H1: method 2 is less accurate than method 1
#???
###########################################################################################

# NONE OF THESE FORECASTS ARE BETTER THAN RW (ACCORDING TO THE DW TEST) !!!
# ACTUALLY, all of these seem to be better than the RW forecast... WTF... 
###########################################################################################



write.xlsx(Data_Forecast_Sample, file = "myworkbook.xlsx",
           sheetName = "USA-ARRESTS", append = FALSE)


# write_xlsx(forecast_UIRP, "Forecast UIRP.xlsx")
 
 stargazer(time_trend_reg, type = "text", out =  "GDPtrend.html")
 
 
#stargazer(forecast_UIRP, type = "text", out = "forecast_UIRP.html")
#stargazer(forecast_CIRP, type = "text", out = "forecast_CIRP.html")
#stargazer(forecast_PPP, type = "text", out = "forecast_PPP.html")

 
install.packages(writexl)

#write_xlsx(Data_Forecast_Sample, "~/Desktop\\Data_Forecast_Sample.xlsx")



# If RMSE(X) < RMSE(RW), then it is a more efficient forecast!






################################################################################
################################################################################
################################################################################


######## Engle-Granger Cointegration Test ######## 


#### UIRP
# Step 1
UIRPlevel <- lm(Data_sample$`Rand to US$` ~ Data_sample$`Interest Rate Differential`, data = Data_sample)
UIRPlevel_residuals <- UIRPlevel$residuals

#Step 2: Conduct Stationarity / Unit-Root Test on Residuals

y_UIRP <- ur.df(UIRPlevel_residuals, type = "none", selectlags = "AIC")

# Engle-Granger Cointegration Test UIRP
summary(y_UIRP)
y_UIRP@teststat      # 0.3980
y_UIRP@cval          #  1pct  5pct 10pct
                # tau1 -2.58 -1.95 -1.62

# t-stat > t-crit \\\ |t-stat| < |t-crit| 
# Thus, fail to reject H0 of non-stationarity 
# Thus, series is NOT cointegrated




#### CIRP
# Step 1
CIRPlevel <- lm(Data_sample$`Rand to US$` ~ Data_sample$`Fwd Diff`, data = Data_sample)
CIRPlevel_residuals <- CIRPlevel$residuals

#Step 2: Conduct Stationarity / Unit-Root Test on Residuals

y_CIRP <- ur.df(CIRPlevel_residuals, type = "none", selectlags = "AIC")
summary(y_CIRP)
y_CIRP@teststat      # -1.405427
y_CIRP@cval          #  1pct  5pct 10pct
                # tau1 -2.58 -1.95 -1.62

# t-stat > t-crit \\\ |t-stat| < |t-crit| 
# Thus, fail to reject H0 of non-stationarity 
# Thus, series is NOT cointegrated




#### PPP
# Step 1
PPPlevel <- lm(Data_sample$`Rand to US$` ~ Data_sample$`Inflation Differential`, data = Data_sample)
PPPlevel_residuals <- PPPlevel$residuals

#Step 2: Conduct Stationarity / Unit-Root Test on Residuals

y_PPP <- ur.df(PPPlevel_residuals, type = "none", selectlags = "AIC")
summary(y_PPP)
y_PPP@teststat      # -1.239224
y_PPP@cval          #  1pct  5pct 10pct
               # tau1 -2.58 -1.95 -1.62

# t-stat > t-crit \\\ |t-stat| < |t-crit| 
# Thus, residuals are not stationary !
# Thus, fail to reject H0 of non-stationarity 
# Thus, series is NOT cointegrated


######## Johansen Cointegration Test ######## 


### Declare Time Series Objects

#ExchRate <- ts(log(Data$`Rand to US$`), start = c(2000,3,1), frequency = 12)
ExchRate <- ts((Data_sample$`Rand to US$`), start = c(2000,3,1), frequency = 12)

IntDiff <- ts((Data$`Interest Rate Differential`), start = c(2000,3,1), frequency = 12)
InflDiff <- ts((Data$`Inflation Differential`), start = c(2000,3,1), frequency = 12)
FwdDiff <- ts((Data$`Fwd Diff`), start = c(2000,3,1), frequency = 12)

IntSA <- ts((Data_sample$`S.A Interest Rate (%)`), start = c(2000,3,1), frequency = 12)
IntUS <- ts((Data_sample$`U.S Interest Rate (%)`), start = c(2000,3,1), frequency = 12)
InflSA <- ts((Data_sample$`CPI (SA)%`), start = c(2000,3,1), frequency = 12)
InflUS <- ts((Data_sample$`CPI (US)%`), start = c(2000,3,1), frequency = 12)
FwdRate <- ts((Data_sample$`Fwd Rate`), start = c(2000,3,1), frequency = 12)


### Bind into a System 

# UIRP
#dsetUIRP <- cbind(ExchRate, IntDiff)
dsetUIRP <- cbind(ExchRate, IntSA, IntUS)

# CIRP
#dsetCIRP <- cbind(ExchRate, FwdDiff)
dsetCIRP <- cbind(ExchRate, FwdRate)


# PPP
#dsetPPP <- cbind(ExchRate, InflDiff)
dsetPPP <- cbind(ExchRate, InflSA, InflUS)



### Lag Selection Criteria

#UIRP
lagselectUIRP <- VARselect(dsetUIRP, lag.max = 12, type = "const")
lagselectUIRP$selection   
# Since 2 lags was chosen, we use 2 - 1 = 1


# CIRP
lagselectCIRP <- VARselect(dsetCIRP, lag.max = 12, type = "const")
lagselectCIRP$selection   
# Since 3 lags was chosen, we use 3 - 1 = 2
                                              # Since 5 lags was chosen, we use 5 - 1 = 4

# PPP
lagselectPPP <- VARselect(dsetPPP, lag.max = 12, type = "const")
lagselectPPP$selection   
# Since 3 lags was chosen, we use 3 - 1 = 2



### Johansen Testing:

# UIRP
ctestUIRP <- ca.jo(dsetUIRP, type = "trace", ecdet = "const", K = 2 )
summary(ctestUIRP)

# r = 0 t-stat < 5% sig. Thus, likely that there's no cointegrating relationship
# Not Coint


# CIRP
ctestCIRP <- ca.jo(dsetCIRP, type = "trace", ecdet = "const", K = 2 )
summary(ctestCIRP)
# r = 0 t-stat < 5% sig. Thus, likely that there's no cointegrating relationship
# Not Coint


# PPP
ctestPPP <- ca.jo(dsetPPP, type = "trace", ecdet = "const", K = 2 )
summary(ctestPPP)
# r = 0 t-stat > 5% sig. Thus, likely that there is cointegrating relationship


#### CONCLUSION: NO COINTEGRATING RELATIONSHIP !!!

# creating the variables 

# ExchRate
logExchRate <- ts(log(Data$`Rand to US$`), start = c(2000,03),frequency = 12)

# UIRP
IntDiff <- ts(Data$`Interest Rate Differential`, start = c(2000,03),frequency = 12)

# CIRP 
FwdDiff <- ts(Data$`Fwd Diff`,  start = c(2000,03),frequency = 12)

# PPP
InlfDiff <- ts(Data$`Inflation Differential`, start = c(2000,03),frequency = 12)




## estimate long run regression with constant

#### UIRP:
UIRP.eq <- lm(logExchRate ~ IntDiff, data = Data)
summary(UIRP.eq)
plot.ts(UIRP.eq$residuals)

# These residuals are clearly not trending, but to confirm that they are stationary we subject them to a unit root test.

UIRP.eq$residuals %>%
  ur.df(., lags = 1, type = "none") %>%
  summary()

# P-Value = 0.0000. Thus, implying they might not be cointegrated. 
# To evaluate the test statistics we should use the critical values from Engle & Granger (1987) or Engle & Yoo (1987). These would suggest that the residuals are indeed stationary. 






##### ERROR CORRECTION MODEL #####
#The next part of the procedure involves constructing the error correction model.


dat_ecm <- tibble(
  gold_d = diff(dat$gold)[-1],
  silver_d = diff(dat$silver)[-1],
  error_ecm1 = gold_eq$residuals[-1:-2],
  gold_d1 = diff(dat$gold)[-(length(dat$gold) - 1)],
  silver_d1 = diff(dat$silver)[-(length(dat$silver) - 1)]
)


ExchRate.eq <- lm( ExchRate ~ IntDiff, data = test)

summary(ExchRate.eq)
plot.ts(ExchRate.eq$residuals)

# check for unit root
error.Exch <- ur.df(ExchRate.eq$residuals, lags=1, type="none")
summary(error.Exch)
# we should actually use test statistics from Engle & Granger (1987) or Engle & Yoo (1987)
#  able to reject null of unit root in the residual - they are cointegrated... ( I think...?)

# prepare data for ECM (data has 231 obs and lag 1st diff has 229)
# gold.d = const + error.ecm1 + gold.d1 + silver.d1

gold.d <- diff(gold)[-1]
silver.d <- diff(silver)[-1]
error.ecm1 <- gold.eq$residuals[-1:-2]
gold.d1 <- diff(gold)[-(length(gold)-1)]
silver.d1 <- diff(silver[-(length(silver)-1)])

# estimate ECM
ecm.gold <- lm(gold.d~error.ecm1+gold.d1+silver.d1)
summary(ecm.gold)
# Note that the alpha is insignificant and close to zero
# Therefore gold is weakly exogenous with respect to the cointegrating parameters since it does not adjust to past deviations from long-run equilibrium

ecm.silver <- lm(silver.d~error.ecm1+gold.d1+silver.d1)
summary(ecm.silver)
# Note that the alpha is negative and significant, so silver does all the work
# Implies there is Granger causality from gold to silver
# It takes 1/0.078 periods to return to equilibrium

# Note that at least one of these signs should be negative and significant when cointegrated!





       




