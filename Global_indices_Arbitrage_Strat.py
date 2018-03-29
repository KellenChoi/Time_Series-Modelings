####### To use information from an earlier time zone to your advantage in a later time zone.
##### Index Country Closing Time (EST) Hours Before S&P Close All Ords Australia 0100 15 Nikkei 225 Japan 0200 14 
##### Hang Seng Hong Kong 0400 12 DAX Germany 1130 4.5 FTSE 100 UK 1130 4.5 NYSE Composite US 1600 0 
##### Dow Jones Industrial Average US 1600 0 S&P 500 US 1600 0

import io
import pandas as pd
from pandas.tools.plotting import autocorrelation_plot
from pandas.tools.plotting import scatter_matrix
import numpy as np

import matplotlib.pyplot as plt

#import gcp
#import gcp.bigquery as bq

import tensorflow as tf
#from tensorflow.contrib import skflow
from sklearn import datasets, metrics

import datetime
from pandas_datareader import data
import fix_yahoo_finance as yf
yf.pdr_override()

# Define the instruments to download. We would like to see Apple, Microsoft and the S&P500 index.
tickers = ['AAPL']

# Define which online source one should use
data_source = 'yahoo'

# We would like all available data from 01/01/2000 until 12/31/2016.
start_date = '2016-12-31'
end_date = '2018-03-27'

# User pandas_reader.data.DataReader to load the desired data. As simple as that.
df = data.get_data_yahoo(tickers, start_date, end_date)
df.head(5
        
%%sql --module market_data_query
SELECT Date, Close FROM $market_data_table

snp = bq.Query(market_data_query, market_data_table=bq.Table('bingo-ml-1:market_data.snp')).to_dataframe().set_index('Date')
nyse = bq.Query(market_data_query, market_data_table=bq.Table('bingo-ml-1:market_data.nyse')).to_dataframe().set_index('Date')
djia = bq.Query(market_data_query, market_data_table=bq.Table('bingo-ml-1:market_data.djia')).to_dataframe().set_index('Date')
nikkei = bq.Query(market_data_query, market_data_table=bq.Table('bingo-ml-1:market_data.nikkei')).to_dataframe().set_index('Date')
hangseng = bq.Query(market_data_query, market_data_table=bq.Table('bingo-ml-1:market_data.hangseng')).to_dataframe().set_index('Date')
ftse = bq.Query(market_data_query, market_data_table=bq.Table('bingo-ml-1:market_data.ftse')).to_dataframe().set_index('Date')
dax = bq.Query(market_data_query, market_data_table=bq.Table('bingo-ml-1:market_data.dax')).to_dataframe().set_index('Date')
aord = bq.Query(market_data_query, market_data_table=bq.Table('bingo-ml-1:market_data.aord')).to_dataframe().set_index('Date')   
        
        
        
        
        
        
        
        
        
        
        
        
        
        
