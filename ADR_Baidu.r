library(forecast)
#library(fpp)
library(quantmod) #Quantitative Financial Modelling
library(xts)
library(TTR) #Technical Trading Rules pkg
library(tseries)

## 1. Baidu
####################### Getting Data ###################################
getSymbols("BIDU", to="2018-02-28")
class(BIDU)
str(BIDU)
head(BIDU)
tail(BIDU)
BIDU$BIDU.Close[1:5]
# dates -> index
dt=index(BIDU); length(dt); dt[1]; dt[length(dt)]

######################### Plotting ######################################
#plot data
chartSeries(BIDU, type = c("auto", "candlesticks", "matchsticks", "bars","line"), 
            subset = NULL,
            show.grid = TRUE, 
            name = NULL,
            log.scale = FALSE,
            TA = 'addVo()',
            TAsep=';',
            line.type = "l",
            bar.type = "ohlc",
            theme = chartTheme("black"))

# plot a subset of the data
chartSeries(BIDU, subset='2015-01-01/2018-01-01', type='line', name ='BIDU.Close', TA=NULL)
chartSeries(BIDU, subset='last 4 months', type='bars', name ='BIDU.Close', TA=NULL)
addBBands() #add Bollinger Bands 
#### BIDU.xts <- BIDU.xts['2015-01-01/2016-01-01']

chartSeries(BIDU, subset='last 4 months', theme="black", TA=NULL) #draw the chart 

addSMA()
addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index

#Simple plotting the Close
BIDU.c=BIDU$BIDU.Close
plot(BIDU.c, main='BIDU.Close')

# add the 50 period simple moving average to panel 1 of the plot
lines(SMA(BIDU.c, n = 50), on=1, col="green")

# add month end points to the chart
points(BIDU.c[endpoints(BIDU.c, on = "months")], col="red", pch=40, on=1)

# add legend to panel 1
addLegend("topright", on=1,legend.names = c("Close", "SMA(50)"),lty=c(1, 1), lwd=c(2, 1),
          col=c("black", "green", "red"))

######################### Returns #############################
#######################################################################
head(BIDU)
BIDU.c=BIDU$BIDU.Close
# simple return
simple.ret = Delt(BIDU.c, type="arithmetic")[-1] #simple return R = P(t)/P(t-1) - 1
# plot the close and add a panel with the simple returns
plot(BIDU.c, main='BIDU.Close')
lines(simple.ret, type="h", on=NA)

# log return
log.ret = Delt(BIDU.c, type="log")[-1]  #log return r = ln(1+R) = ln(P(t)/P(t-1))
# plot the close and add a panel with the simple returns
plot(BIDU.c, main='BIDU.Close')
lines(log.ret, type="h", on=NA)

###############################################################################
##################################################################################
#if we want to use xts format for derived variables:
df.tmp = data.frame(BIDU$BIDU.Volume[-1])
df.tmp$BIDU.lrt <- log.ret
BIDU.xts.lrt = as.xts(df.tmp$BIDU.lrt)
chartSeries(BIDU.xts.lrt, subset='last 4 years', theme="white", TA=NULL, type='line', name ='BIDU.log.ret')
#please read quantmod manual to familiarize yourself with capabilities
addSMA() #simple moving average
addEMA(100) #exponential moving average
addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index    

###############################Distributional properties of returns##############
#############################################################################
BIDU.c.v=as.numeric(BIDU.c);
acf(BIDU.c.v)
plot(BIDU.c.v, type='l')
summary(BIDU.c.v)
log.ret.v=as.numeric(log.ret);
acf(log.ret.v)

########################### Normality tests ###################################
#need additional tools
library(moments)
library(fitdistrplus)
library(metRology)  #has implementation of scaled and shifted t distribution.
library(goftest)
## Perform the Shapiro-Wilk normality test
shapiro.test(log.ret.v)
#Anderson-Darling test 
ad.test(log.ret.v,null='pnorm')
#Cramer-Von Mises Test
cvm.test(log.ret.v, null='pnorm')  

###############################################################################
###################### Normal and log-Normal distributions ######################
mean(BIDU.c); var(BIDU.c); skewness(BIDU.c); kurtosis(BIDU.c)
mean(log.ret); var(log.ret); skewness(log.ret); kurtosis(log.ret)

plot(density(log.ret.v))
plot(density(exp(log.ret.v)))
plot(ecdf(log.ret.v),main="Empirical CDF")

fit <- fitdist(log.ret.v, "norm")
plot(fit, histo = FALSE, demp = TRUE)
gofstat(fit)
#or one by one
cdfcomp(fit, addlegend=FALSE)
denscomp(fit, addlegend=FALSE,breaks=100)
ppcomp(fit, addlegend=FALSE)
qqcomp(fit, addlegend=FALSE)          

######################## Check stationarity ##################################
### Rejecting the null hypothesis suggests that a time series is stationary
adf.test(BIDU.c);#- p=0.2275  # non-stationary
kpss.test(BIDU.c)  # p << 0.01 #Clearly non-stationary!!!!!!!!!!!!!          

log.ret = Delt(BIDU.c, type="log")[-1]  
adf.test(log.ret);#- p<<0.01
kpss.test(log.ret)  # p > 0.1 #Clearly stationary!!!!!!!!!!!!

#chartSeries(BIDU)
chartSeries(log.ret)          

###################### Is the TS really stationary? ####################################
log.ret.1 <- log.ret['2007-01-04/2012-01-03']
mean(log.ret.1); var(log.ret.1)
log.ret.2 <- log.ret['2012-01-04/2018-01-03']
mean(log.ret.2); var(log.ret.2)
                     
#mu2 > 5*mu1;  var2 < 0.25*var1 
#Conclusion the TS of log-returns of BIDU is NOn-Stationary!!!!!!!!!!!!!!!
###########################################################################
################# Check serial correlation vs. i.i.d. ####################
############### Dynamic trend / Seasonality #################################
#############################################################################################
acf(log.ret)  # anticorrelation & 7 lags memory !!!!
acf(log.ret.1) #strong anticorrelation & 7 lags memory
acf(log.ret.2) #very (5% conf interval) small anticorrelation at lag 4
#Conclusion - the market's regime has changed
#Box-Pierce test for serial correlation
Box.test(log.ret)   #p-value = 0.1701 Conclusion - there is NO serial correlation !!!!!
Box.test(log.ret.1)  #p-value = 0.3667 Conclusion - there is NO serial correlation
Box.test(log.ret.2)  #p-value = 0.3727 - there is no serial correlation !!!!

############################# Modelling Patterns ##############################
# Plot daily closing prices for BIDU (BIDU & AMZN)
getSymbols("BIDU", src="google")
plot(Cl(BIDU))
getSymbols("AMZN", src="google")
plot(Cl(AMZN))
lines(Cl(BIDU))

# Create differenced log returns 
# and plot their values and correlogram
bidurt = diff(log(Cl(BIDU)))[-1]
head(bidurt); tail(bidurt)
plot(bidurt)
plot(density(bidurt))
acf(bidurt, na.action=na.omit)
wilcox.test(as.numeric(bidurt))
adf.test(bidurt) #-stationary p-value = 0.01
kpss.test(bidurt)
Box.test(bidurt) #- p-value = 0.2013 - no Strong serial correlation???

amznrt = diff(log(Cl(AMZN)))[-1]
head(amznrt); tail(amznrt)
plot(amznrt)
plot(density(amznrt))
acf(amznrt, na.action=na.omit)
wilcox.test(as.numeric(amznrt))  #not Normal
adf.test(amznrt) #-stationary
kpss.test(amznrt)
Box.test(amznrt) #- p-value = 0.6298 - No serial correlation !!!!

# Plot the correlogram
acf(abs(bidurt))
acf(abs(amznrt))

#fit to normal
fit_amzn <- fitdist(as.numeric(amznrt), "norm")
gofstat(fit_amzn)

fit_bidu <- fitdist(as.numeric(bidurt), "norm")
gofstat(fit_bidu)

#summary(fit)
plot(fit_amzn, histo = FALSE, demp = TRUE)
kurtosis(as.numeric(amznrt))

plot(fit_bidu, histo = FALSE, demp = TRUE)
kurtosis(as.numeric(bidurt))

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
library("metRology") #--#has implementation of the non-standard t-distribution
fit_amzn <- fitdist(as.numeric(amznrt),"t.scaled",
                    start=list(df=3,mean=mean(as.numeric(amznrt)),sd=sd(as.numeric(amznrt))))
plot(fit_amzn, histo = FALSE, demp = TRUE)
fit_amzn$estimate                    

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
library("metRology") #--#has implementation of the non-standard t-distribution
fit_bidu <- fitdist(as.numeric(bidurt),"t.scaled",
                    start=list(df=3,mean=mean(as.numeric(bidurt)),sd=sd(as.numeric(bidurt))))
plot(fit_bidu, histo = FALSE, demp = TRUE)
fit_bidu$estimate 

###################################################################################
########################### MA(q) process #######################################
# Fit an ARIMA(0, 0, 1) model (i.e. MA(1) ) 
# and plot the correlogram of the residuals
bidurt.ma <- arima(bidurt, order=c(0, 0, 1))
acf(bidurt.ma$res[-1])

# Fit an ARIMA(0, 0, 2) model (i.e. MA(2) ) 
# and plot the correlogram of the residuals
bidurt.ma <- arima(bidurt, order=c(0, 0, 2))
acf(bidurt.ma$res[-1]) 
# the distribution of bidu MA[1] AND MA[2] are THE SAME!!!

#######################################################################
################## ARMA(p,q) ###########################################
# Loop over p = 0 to 4, q = 0 to 4 and create each
# ARMA(p,q) model, then fit to the previous amznrt 
# returns, using the AIC to find the best fit
bidufinal.aic <- Inf
bidufinal.order <- c(0,0,0)
for (i in 0:4) for (j in 0:4) {
        biducurrent.aic <- AIC(arima(bidurt, order=c(i, 0, j)))
        if (biducurrent.aic < bidufinal.aic) {
                bidufinal.aic <- biducurrent.aic
                bidufinal.order <- c(i, 0, j)
                bidufinal.arma <- arima(bidurt, order=bidufinal.order)
        }
}

# Output the results of the fit
bidufinal.order

# Plot the residuals of the final model
acf(resid(bidufinal.arma), na.action=na.omit)

# Carry out a Ljung-Box test for realisation
# of discrete white noise
Box.test(resid(bidufinal.arma), lag=20, type="Ljung-Box")

plot(density(bidufinal.arma$residuals))

wilcox.test(resid(bidufinal.arma))
adf.test(resid(bidufinal.arma)) #-stationary
kpss.test(resid(bidufinal.arma))

# Plot the correlogram
acf(abs(resid(bidufinal.arma)))

#fit to normal
fit_bidu <- fitdist(as.numeric(resid(bidufinal.arma)), "norm")
gofstat(fit_bidu)
#summary(fit)
plot(fit_bidu, histo = FALSE, demp = TRUE)

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit_bidu <- fitdist(as.numeric(resid(bidufinal.arma)),"t.scaled",
                    start=list(df=3,mean=mean(as.numeric(resid(bidufinal.arma))),sd=sd(as.numeric(resid(bidufinal.arma)))))
plot(fit_bidu, histo = FALSE, demp = TRUE)
fit_bidu$estimate 

library(PerformanceAnalytics)  #great pkg
#######################################################################
################## ARIMA ###########################################
getSymbols("BIDU", src="google")
head(BIDU); tail(BIDU); str(BIDU)
BIDU.c=BIDU$BIDU.Close
log.ret = Delt(BIDU.c, type="log")[-1]  #log return r = ln(1+R) = ln(P(t)/P(t-1))
# plot the close and add a panel with the simple returns
plot(BIDU.c, main='BIDU.Close')
plot(log.ret, type="l", on=NA)
acf(log.ret)  # correlation in log-returns
pacf(log.ret)
Box.test(log.ret)
#proxy for volatility
plot(abs(log.ret), type="l", on=NA)
acf(abs(log.ret))
Box.test(abs(log.ret))  # p-value < 2.2e-16 NO correlation in volatility?
#########################################################
# Determine the best fitting ARIMA model
bidufinal.aic <- Inf
bidufinal.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4) {
        biducurrent.aic <- AIC(arima(log.ret, order=c(p, d, q)))
        if (biducurrent.aic < bidufinal.aic) {
                bidufinal.aic <- biducurrent.aic
                bidufinal.order <- c(p, d, q)
                bidufinal.arima <- arima(log.ret, order=bidufinal.order)
        }
}

# Output the best ARIMA order
bidufinal.order

# Plot a correlogram of the residuals, calculate 
# the Ljung-Box test and predict the next 25 daily
# values of the series
acf(resid(bidufinal.arima), na.action=na.omit)
Box.test(resid(bidufinal.arima), lag=20, type="Ljung-Box") #no correlations!
plot(resid(bidufinal.arima))
acf(abs(resid(bidufinal.arima)))  #still have some unused information
#try to fit to normal distribution
fit_bidu <- fitdist(as.numeric(resid(bidufinal.arima)), "norm")
plot(fit_bidu, histo = FALSE, demp = TRUE)  # not normal!!!

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit_bidu <- fitdist(as.numeric(resid(bidufinal.arima)),"t.scaled",
                    start=list(df=3,mean=mean(as.numeric(resid(bidufinal.arima))),sd=sd(as.numeric(resid(bidufinal.arima)))))
plot(fit_bidu, histo = FALSE, demp = TRUE)  # clearly fat-tailedl!!!
##############################################################
#use the forecast library to automatically determine ARIMA coefficients
fit_bidu <- auto.arima(log.ret)
fit_bidu
fit_bidu$arima$coef  #forecast pkg yields AR(1)
plot(fit_bidu$residuals)
Acf(fit_bidu$residuals)  # bad fitting
plot(forecast(BIDU.c, h=50)) #bad forcast

############################### GARCH ######################################################
# Fit a GARCH model to resid(bidifinal.arima)
ft.garch <- garch(resid(bidufinal.arima), trace=F)
ft.res <- ft.garch$res[-1]
length(resid(bidufinal.arima))
#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit <- fitdist(ft.res,"t.scaled", start=list(df=3,mean=mean(ft.res),sd=sd(ft.res)))
plot(fit, histo = FALSE, demp = TRUE)

# Plot the residuals and abs residuals
acf(ft.res)
acf(abs(ft.res))  # no correlations/no information left!!!



