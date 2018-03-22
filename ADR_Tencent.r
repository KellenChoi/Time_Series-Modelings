library(forecast)
library(fpp)
library(quantmod) #Quantitative Financial Modelling
library(xts)
library(TTR) #Technical Trading Rules pkg
library(tseries)

## 1. Tencent
####################### Getting Data ###################################
getSymbols("TCEHY", to="2018-02-28")
class(TCEHY)
str(TCEHY)
head(TCEHY)
tail(TCEHY)
TCEHY$TCEHY.Close[1:5]
# dates -> index
dt=index(TCEHY); length(dt); dt[1]; dt[length(dt)]

######################### Plotting ######################################
#plot data
chartSeries(TCEHY, type = c("auto", "candlesticks", "matchsticks", "bars","line"), 
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
chartSeries(TCEHY, subset='2014-01-01/2014-12-31', type='line', name ='TCEHY.Close', TA=NULL)
chartSeries(TCEHY, subset='last 4 months', type='bars', name ='TCEHY.Close', TA=NULL)

#### TCEHY.xts <- TCEHY.xts['2015-01-01/2016-01-01']

chartSeries(TCEHY, subset='last 4 months', theme="black", TA=NULL) #draw the chart 

addSMA()
addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index

#Simple plotting the Close
TCEHY.c=TCEHY$TCEHY.Close
plot(TCEHY.c, main='TCEHY.Close')

# add the 50 period simple moving average to panel 1 of the plot
lines(SMA(TCEHY.c, n = 50), on=1, col="green")

# add month end points to the chart
points(TCEHY.c[endpoints(TCEHY.c, on = "months")], col="red", pch=40, on=1)

# add legend to panel 1
addLegend("topright", on=1,legend.names = c("Close", "SMA(50)"),lty=c(1, 1), lwd=c(2, 1),
          col=c("black", "green", "red"))
          
######################### Returns #############################
#######################################################################
head(TCEHY)
TCEHY.c=TCEHY$TCEHY.Close

# simple return
simple.ret = Delt(TCEHY.c, type="arithmetic")[-1] #simple return R = P(t)/P(t-1) - 1
# plot the close and add a panel with the simple returns
plot(TCEHY.c, main='TCEHY.Close')
lines(simple.ret, type="h", on=NA)
          
# log return
log.ret = Delt(TCEHY.c, type="log")[-1]  #log return r = ln(1+R) = ln(P(t)/P(t-1))
# plot the close and add a panel with the simple returns
plot(TCEHY.c, main='TCEHY.Close')
lines(log.ret, type="h", on=NA)

###############################################################################
##################################################################################
#if we want to use xts format for derived variables:
df.tmp = data.frame(TCEHY$TCEHY.Volume[-1])
df.tmp$TCEHY.lrt <- log.ret
TCEHY.xts.lrt = as.xts(df.tmp$TCEHY.lrt)
chartSeries(TCEHY.xts.lrt, subset='last 4 years', theme="white", TA=NULL, type='line', name ='TCEHY.log.ret')

#please read quantmod manual to familiarize yourself with capabilities
addSMA() #simple moving average
addEMA(100) #exponential moving average
###addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index    

########################### Distributional properties of returns ##############
#############################################################################
TCEHY.c.v=as.numeric(TCEHY.c);
acf(TCEHY.c.v)
plot(TCEHY.c.v, type='l')
summary(TCEHY.c.v)
log.ret.v=as.numeric(log.ret);

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
mean(TCEHY.c); var(TCEHY.c); skewness(TCEHY.c); kurtosis(TCEHY.c)
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
adf.test(TCEHY.c);#- p=0.99 # non-stationary
kpss.test(TCEHY.c)  # p << 0.01 #Clearly non-stationary!!!!!!!!!!!!!          

log.ret = Delt(TCEHY.c, type="log")[-1]  
adf.test(log.ret);#- p<<0.01  # stationary
kpss.test(log.ret)  # p > 0.1 #Clearly stationary!!!!!!!!!!!!

#chartSeries(TCEHY)
chartSeries(log.ret)          

###################### Is the TS really stationary? ####################################
log.ret.1 <- log.ret['2014-09-22/2015-09-21']
mean(log.ret.1); var(log.ret.1)
log.ret.2 <- log.ret['2016-09-22/2017-09-21']
mean(log.ret.2); var(log.ret.2)

#mu2 > 5*mu1;  var2 < 0.25*var1 
#Conclusion the TS of log-returns of BABA is NOn-Stationary!!!!!!!!!!!!!!!
###########################################################################
################# Check serial correlation vs. i.i.d. ####################
############### Dynamic trend / Seasonality #################################
#############################################################################################
acf(log.ret)  
acf(log.ret.1) 
acf(log.ret.2) 
#Conclusion - the market's regime has changed
#Box-Pierce test for serial correlation
Box.test(log.ret)   #p-value = 0.445 Conclusion - there is NO serial correlation !!!!!
Box.test(log.ret.1)  #p-value = 0.4628 Conclusion - there is NO serial correlation
Box.test(log.ret.2)  #p-value = 0.08 - there is no serial correlation !!!!

############################# Modelling Patterns ##############################
# Plot daily closing prices for Alibab (TCEHY & AMZN)
getSymbols("TCEHY", src="google")
plot(Cl(TCEHY))

getSymbols("AMZN", src="google")
plot(Cl(AMZN))

# Create differenced log returns 
# and plot their values and correlogram
tencentrt = diff(log(Cl(TCEHY)))[-1]
head(tencentrt); tail(tencentrt)
plot(tencentrt)
plot(density(tencentrt))
acf(tencentrt, na.action=na.omit)
wilcox.test(as.numeric(tencentrt))
adf.test(tencentrt) #-stationary
kpss.test(tencentrt)
Box.test(tencentrt) 

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
acf(abs(tencentrt))
acf(abs(amznrt))

#fit to normal
fit_amzn <- fitdist(as.numeric(amznrt), "norm")
gofstat(fit_amzn)

fit_tencent <- fitdist(as.numeric(tencentrt), "norm")
gofstat(fit_tencent)

#summary(fit)
plot(fit_amzn, histo = FALSE, demp = TRUE)
kurtosis(as.numeric(amznrt))

plot(fit_tencent, histo = FALSE, demp = TRUE)
kurtosis(as.numeric(tencentrt))

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
library("metRology") #--#has implementation of the non-standard t-distribution
fit_amzn <- fitdist(as.numeric(amznrt),"t.scaled",
                    start=list(df=3,mean=mean(as.numeric(amznrt)),sd=sd(as.numeric(amznrt))))
plot(fit_amzn, histo = FALSE, demp = TRUE)
fit_amzn$estimate                    

fit_tencent <- fitdist(as.numeric(tencentrt),"t.scaled",
                    start=list(df=3,mean=mean(as.numeric(tencentrt)),sd=sd(as.numeric(tencentrt))))
plot(fit_tencent, histo = FALSE, demp = TRUE)
fit_tencent$estimate 
# ---> better fit to T distribution
###################################################################################
########################### MA(q) process #######################################
# Fit an ARIMA(0, 0, 1) model (i.e. MA(1) ) 
# and plot the correlogram of the residuals
tencentrt.ma <- arima(tencentrt, order=c(0, 0, 1))
acf(tencentrt.ma$res[-1])

# Fit an ARIMA(0, 0, 2) model (i.e. MA(2) ) 
# and plot the correlogram of the residuals
tencentrt.ma <- arima(tencentrt, order=c(0, 0, 2))
acf(tencentrt.ma$res[-1]) 
# the distribution of baba MA[1] AND MA[2] are THE SAME!!!

#######################################################################
################## ARMA(p,q) ###########################################
# Loop over p = 0 to 4, q = 0 to 4 and create each
# ARMA(p,q) model, then fit to the previous amznrt 
# returns, using the AIC to find the best fit
tencentfinal.aic <- Inf
tencentfinal.order <- c(0,0,0)
for (i in 0:4) for (j in 0:4) {
        tencentcurrent.aic <- AIC(arima(tencentrt, order=c(i, 0, j)))
        if (tencentcurrent.aic < tencentfinal.aic) {
                tencentfinal.aic <- tencentcurrent.aic
                tencentfinal.order <- c(i, 0, j)
                tencentfinal.arma <- arima(tencentrt, order=tencentfinal.order)
        }
}

# Output the results of the fit
tencentfinal.order  #[1] 3 0 0

# Plot the residuals of the final model
acf(resid(tencentfinal.arma), na.action=na.omit)

# Carry out a Ljung-Box test for realisation
# of discrete white noise
Box.test(resid(tencentfinal.arma), lag=20, type="Ljung-Box")

plot(density(tencentfinal.arma$residuals))

wilcox.test(resid(tencentfinal.arma))
adf.test(resid(tencentfinal.arma)) #-stationary
kpss.test(resid(tencentfinal.arma))

# Plot the correlogram
acf(abs(resid(tencentfinal.arma)))

#fit to normal
fit_tencent <- fitdist(as.numeric(resid(tencentfinal.arma)), "norm")
gofstat(fit_tencent)
#summary(fit)
plot(fit_tencent, histo = FALSE, demp = TRUE)
## ----> fit to normal? rather than T dist??????

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit_tencent <- fitdist(as.numeric(resid(tencentfinal.arma)),"t.scaled",
                    start=list(df=3,mean=mean(as.numeric(resid(tencentfinal.arma))),sd=sd(as.numeric(resid(tencentfinal.arma)))))


library(PerformanceAnalytics)  #great pkg
#######################################################################
################## ARIMA ###########################################
getSymbols("TCEHY", src="google")
head(TCEHY); tail(TCEHY); str(TCEHY)
TCEHY.c=TCEHY$TCEHY.Close
log.ret = Delt(TCEHY.c, type="log")[-1]  #log return r = ln(1+R) = ln(P(t)/P(t-1))
# plot the close and add a panel with the simple returns
plot(TCEHY.c, main='TCEHY.Close')
plot(log.ret, type="l", on=NA)
acf(log.ret)  #no correlation in log-returns
pacf(log.ret)
Box.test(log.ret)
#proxy for volatility
plot(abs(log.ret), type="l", on=NA)
acf(abs(log.ret))
Box.test(abs(log.ret))  # p-value = 0.0009208: NO correlation in volatility?
#########################################################
# Determine the best fitting ARIMA model
tencentfinal.aic <- Inf
tencentfinal.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4) {
        tencentcurrent.aic <- AIC(arima(log.ret, order=c(p, d, q)))
        if (tencentcurrent.aic < tencentfinal.aic) {
                tencentfinal.aic <- tencentcurrent.aic
                tencentfinal.order <- c(p, d, q)
                tencentfinal.arima <- arima(log.ret, order=tencentfinal.order)
        }
}

# Output the best ARIMA order
tencentfinal.order

# Plot a correlogram of the residuals, calculate 
# the Ljung-Box test and predict the next 25 daily
# values of the series
acf(resid(tencentfinal.arima), na.action=na.omit)
Box.test(resid(tencentfinal.arima), lag=20, type="Ljung-Box") #no correlations!
plot(resid(tencentfinal.arima))
acf(abs(resid(tencentfinal.arima)))  #still have some unused information
#try to fit to normal distribution
fit_tencent <- fitdist(as.numeric(resid(tencentfinal.arima)), "norm")
plot(fit_tencent, histo = FALSE, demp = TRUE)  # not normal!!!

#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit_tencent <- fitdist(as.numeric(resid(tencentfinal.arima)),"t.scaled",
                    start=list(df=3,mean=mean(as.numeric(resid(tencentfinal.arima))),sd=sd(as.numeric(resid(tencentfinal.arima)))))
plot(fit_tencent, histo = FALSE, demp = TRUE)  # clearly fat-tailedl!!!
##############################################################
#use the forecast library to automatically determine ARIMA coefficients
fit_tencent <- auto.arima(log.ret)
fit_tencent
fit_tencent$arima$coef  #forecast pkg yields AR(1)
plot(fit_tencent$residuals)
Acf(fit_tencent$residuals)  # bad fitting
plot(forecast(TCEHY.c, h=50)) #bad forcast

############################### GARCH ######################################################
# Fit a GARCH model to resid(tencnetfinal.arima)
ft.garch <- garch(resid(tencentfinal.arima), trace=F)
ft.res <- ft.garch$res[-1]
length(resid(tencentfinal.arima))
#fit to Student's t distribution for 'df' degrees of freedom, shifted by 'mean' and scaled by 'sd'.
fit <- fitdist(ft.res,"t.scaled", start=list(df=3,mean=mean(ft.res),sd=sd(ft.res)))
plot(fit, histo = FALSE, demp = TRUE)

# Plot the residuals and abs residuals
acf(ft.res)
acf(abs(ft.res))  # no correlations/no information left!!!         
