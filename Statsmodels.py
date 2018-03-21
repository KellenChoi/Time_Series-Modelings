import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
%matplotlib inline
import statsmodels.api as sm

######### create dataframe
df = sm.datasets.macrodata.load_pandas().data
print(sm.datasets.macrodata.NOTE)

######### indexing date
index = pd.Index(sm.tsa.datetools.dates_from_range('1959Q1', '2009Q3'))
df.index = index
df.head()

######### plotting 
df['realgdp'].plot()
plt.ylabel("REAL GDP")

######### using Statsmodels to get the trend
########### Hodrick-Prescott Filter(HP filter)
# Tuple unpacking
gdp_cycle, gdp_trend = sm.tsa.filters.hpfilter(df.realgdp)
gdp_cycle
type(gdp_cycle)   ### pandas.core.series.Series
# add gdp_trend to the dataframe to estimate
df["trend"] = gdp_trend
# plotting
df[['trend','realgdp']].plot()
# sub-plotting
df[['trend','realgdp']]["2000-03-31":].plot(figsize=(12,8))
