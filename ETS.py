import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
%matplotlib inline

airline = pd.read_csv('airline_passengers.csv',index_col="Month")
airline.head()
airline.plot()

######## ETS: Error-Trends-Seaonality - (for linear, seasonal, constant trend data -> additive model)
##### type of data: additive or multiplicative
### Get data in correct format
airline.dropna(inplace=True)
airline.index = pd.to_datetime(airline.index)

from statsmodels.tsa.seasonal import seasonal_decompose
result = seasonal_decompose(airline['Thousands of Passengers'], model='multiplicative')
result.plot()
