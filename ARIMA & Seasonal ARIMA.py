import numpy as np
import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt
%matplotlib inline

#### Getting the data and formatting 
df = pd.read_csv('monthly-milk-production-pounds-p.csv')
df.head()
df.tail()
df.columns = ['Month','Milk in pounds per cow']
df.head()
# Weird last value at bottom causing issues
df.drop(168,axis=0,inplace=True)
df['Month'] = pd.to_datetime(df['Month'])  ## date index
df.set_index('Month',inplace=True)
df.head()
df.describe().transpose()

#### Visualizing the data
df.plot()
## SMA(12) and STD(12)
timeseries = df['Milk in pounds per cow']
timeseries.rolling(12).mean().plot(label='12 Month Rolling Mean')
timeseries.rolling(12).std().plot(label='12 Month Rolling Std')
timeseries.plot()
plt.legend()
## SMA(12)
timeseries.rolling(12).mean().plot(label='12 Month Rolling Mean')
timeseries.plot()
plt.legend()

#### Decomposition: to see the individual parts
from statsmodels.tsa.seasonal import seasonal_decompose
decomposition = seasonal_decompose(df['Milk in pounds per cow'], freq=12)  
fig = plt.figure()  
fig = decomposition.plot()  
fig.set_size_inches(15, 8)

#### Stationality testing
####### Augmented Dickey-Fuller unit root test: H0 - the time series has a unit root(non-stationary)
from statsmodels.tsa.stattools import adfuller

result = adfuller(df['Milk in pounds per cow'])

print('Augmented Dickey-Fuller Test:')
labels = ['ADF Test Statistic','p-value','#Lags Used','Number of Observations Used']

for value,label in zip(result,labels):
    print(label+' : '+str(value) )
    
if result[1] <= 0.05:
    print("strong evidence against the null hypothesis, reject the null hypothesis. Data has no unit root and is stationary")
else:
    print("weak evidence against null hypothesis, time series has a unit root, indicating it is non-stationary ")

### Storing it in a fnc for later use!!!!
def adf_check(time_series):
    """
    Pass in a time series, returns ADF report
    """
    result = adfuller(time_series)
    print('Augmented Dickey-Fuller Test:')
    labels = ['ADF Test Statistic','p-value','#Lags Used','Number of Observations Used']

    for value,label in zip(result,labels):
        print(label+' : '+str(value) )
    
    if result[1] <= 0.05:
        print("strong evidence against the null hypothesis, reject the null hypothesis. Data has no unit root and is stationary")
    else:
        print("weak evidence against null hypothesis, time series has a unit root, indicating it is non-stationary ")
        
        
#### Seasonal ARIMA: when data is seasonal
### Differencing (differencing on until data is stationary)
### First Diferrencing
df['Milk First Difference'] = df['Milk in pounds per cow'] - df['Milk in pounds per cow'].shift(1)
adf_check(df['Milk First Difference'].dropna())    
df['Milk First Difference'].plot()    ####### p-vlue = 0.03 --> "STATIONARY"

### Second Differencing
# Sometimes it would be necessary to do a second difference 
# This is just for show, we didn't need to do a second difference in our case
df['Milk Second Difference'] = df['Milk First Difference'] - df['Milk First Difference'].shift(1)
adf_check(df['Milk Second Difference'].dropna())
df['Milk Second Difference'].plot()

### Seasonal Difference ---> shift(12)
df['Seasonal Difference'] = df['Milk in pounds per cow'] - df['Milk in pounds per cow'].shift(12)
df['Seasonal Difference'].plot()
# Seasonal Difference by itself was not enough!
adf_check(df['Seasonal Difference'].dropna())   ###### p-value : 0.160798805277, indicating it is non-stationary
# Seasonal First Difference
df['Seasonal First Difference'] = df['Milk First Difference'] - df['Milk First Difference'].shift(12)
df['Seasonal First Difference'].plot()
adf_check(df['Seasonal First Difference'].dropna())   ###### p-value : 1.86542343188e-05, Data has no unit root and is stationary
    
    
    
    
    
