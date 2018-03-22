##### Autocorrelation plot(Correlogram): correlation of the series with itself, lagged by x time units
######## great info for indentifying and interpreting ACF and PACF 

import numpy as np
import pandas as pd
import statsmodels.api as sm

import matplotlib.pyplot as plt
%matplotlib inline

### reading and formatting the data
df = pd.read_csv('monthly-milk-production-pounds-p.csv')
df.columns = ['Month','Milk in pounds per cow']
# Weird last value at bottom causing issues
df.drop(168,axis=0,inplace=True)
df['Month'] = pd.to_datetime(df['Month'])
df.head()  

### differencing
## first differencing
df['Milk First Difference'] = df['Milk in pounds per cow'] - df['Milk in pounds per cow'].shift(1)
df['Milk First Difference'].plot()
## second differencing 
df['Milk Second Difference'] = df['Milk First Difference'] - df['Milk First Difference'].shift(1)
df['Milk Second Difference'].plot()
## seasonal difference
df['Seasonal Difference'] = df['Milk in pounds per cow'] - df['Milk in pounds per cow'].shift(12)
df['Seasonal Difference'].plot()
## seasonal first difference
df['Seasonal First Difference'] = df['Milk First Difference'] - df['Milk First Difference'].shift(12)
df['Seasonal First Difference'].plot()


####### ACF
from statsmodels.graphics.tsaplots import plot_acf,plot_pacf
# Duplicate plots
# Check out: https://stackoverflow.com/questions/21788593/statsmodels-duplicate-charts
# https://github.com/statsmodels/statsmodels/issues/1265
fig_first = plot_acf(df["Milk First Difference"].dropna())
fig_seasonal_first = plot_acf(df["Seasonal First Difference"].dropna())

### by Pandas: only ACF, not PACF but statsmodels recommended
from pandas.plotting import autocorrelation_plot
autocorrelation_plot(df['Seasonal First Difference'].dropna())

####### PACF
result = plot_pacf(df["Seasonal First Difference"].dropna())
## Typically a sharp drop after lag "k" suggests an AR-k model should be used. 
## If there is a gradual decline, it suggests an MA model

## more plotting
fig = plt.figure(figsize=(12,8))
ax1 = fig.add_subplot(211)
fig = sm.graphics.tsa.plot_acf(df['Seasonal First Difference'].iloc[13:], lags=40, ax=ax1)
ax2 = fig.add_subplot(212)
fig = sm.graphics.tsa.plot_pacf(df['Seasonal First Difference'].iloc[13:], lags=40, ax=ax2)

### NOTE:
# Identification of an AR model is often best done with the PACF.
# Identification of an MA model is often best done with the ACF rather than the PACF.


###### Seasonal ARIMA Model
## For non-seasonal data
from statsmodels.tsa.arima_model import ARIMA

# We have seasonal data!
model = sm.tsa.statespace.SARIMAX(df['Milk in pounds per cow'],order=(0,1,0), seasonal_order=(1,1,1,12))
results = model.fit()
print(results.summary())

results.resid.plot()
results.resid.plot(kind='kde')

####### Prediction of future values
df['forecast'] = results.predict(start = 150, end= 168, dynamic= True)  
df[['Milk in pounds per cow','forecast']].plot(figsize=(12,8))

####### Forecasting: requires more time periods so create them with pandas
df.tail()
from pandas.tseries.offsets import DateOffset
future_dates = [df.index[-1] + DateOffset(months=x) for x in range(0,24) ]
future_dates
future_dates_df = pd.DataFrame(index=future_dates[1:],columns=df.columns)
future_df = pd.concat([df,future_dates_df])
future_df.head()
future_df.tail()

