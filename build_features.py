from __future__ import division
import numpy as np
import pandas as pd
from helper_funcs import *


# read in the texas dataset
data_dir = '/Users/robert/Documents/UMN/air_pollution/data/'
df = import_region(region="houston", filename=data_dir + 'all_texas.csv')


# impute missing values with the average on a particular datetime
impute_vars = ['wind_degrees_compass', 'wind_knots', 'temperature', 'dewpoint', 'pct_humidity', 'ozone', 'so2', 'co', 'no2']
for var in impute_vars:
	print "Pct Missing in", var + ":", round(df[var].isnull().sum()/len(df)*100, 1)
	df[var] = df.groupby("datetime_key")[var].transform(lambda x: x.fillna(x.mean()))
	print "\tReduced to:", round(df[var].isnull().sum()/len(df)*100, 1)

# if anything is still missing, average over the entire day and all locations
impute_vars = ['dewpoint', 'pct_humidity', 'ozone', 'so2', 'co', 'no2']
for var in impute_vars:
	print "Pct Missing in", var + ":", round(df[var].isnull().sum()/len(df)*100, 1)
	df[var] = df.groupby("date_key")[var].transform(lambda x: x.fillna(x.mean()))
	print "\tReduced to:", round(df[var].isnull().sum()/len(df)*100, 1)

# if anything is still missing, average over the entire locations
impute_vars = ['dewpoint', 'pct_humidity', 'so2', 'co', 'no2']
for var in impute_vars:
	print "Pct Missing in", var + ":", round(df[var].isnull().sum()/len(df)*100, 1)
	df[var] = df.groupby("location_key")[var].transform(lambda x: x.fillna(x.mean()))
	print "\tReduced to:", round(df[var].isnull().sum()/len(df)*100, 1)

# any missings left?
for var in df.columns.tolist():
	print "Num Missing in", var + ":", df[var].isnull().sum()


# build out date features
# include both binary representations
# and continuous cyclic variables
df['day_of_week'] = pd.DatetimeIndex(df.Date_Local).dayofweek
df['day_of_week_cycle'] = make_cyclic_feature(df.day_of_week)
df = make_binary_features(df, 'day_of_week')

df['day_of_year_cycle'] = make_cyclic_feature(pd.DatetimeIndex(df.Date_Local).dayofyear)
# no binary, to many features

df['week_of_year_cycle'] = make_cyclic_feature(pd.DatetimeIndex(df.Date_Local).weekofyear)
# no binary, to many features

# df['year'] = pd.DatetimeIndex(df.Date_Local).year
# df = make_binary_features(df, 'year')

df['month'] = pd.DatetimeIndex(df.Date_Local).month
df['month_cycle'] = make_cyclic_feature(df.month)
df = make_binary_features(df, 'month')

df['day_of_month'] = pd.DatetimeIndex(df.Date_Local).day
df['day_of_month_cycle'] = make_cyclic_feature(df.day_of_month)
df = make_binary_features(df, 'day_of_month')

df['quarter'] = pd.DatetimeIndex(df.Date_Local).quarter
df['quarter_cycle'] = make_cyclic_feature(df.quarter)
df = make_binary_features(df, 'quarter')

df['time_cycle'] = make_cyclic_feature(df.Time_Local)
df = make_binary_features(df, 'Time_Local')

# convert wind direction to two cyclic variables
df['wind_direction_sin'] = np.sin(math.pi * df.wind_degrees_compass / 180.0)
df['wind_direction_cos'] = np.cos(math.pi * df.wind_degrees_compass / 180.0)
df['wind_direction_NE'] = np.where(df['wind_degrees_compass'] < 90, 1, 0)
df['wind_direction_SE'] = np.where((df['wind_degrees_compass'] >= 90) & (df['wind_degrees_compass'] < 180), 1, 0)
df['wind_direction_SW'] = np.where((df['wind_degrees_compass'] >= 180) & (df['wind_degrees_compass'] < 270), 1, 0)
df['wind_direction_NW'] = np.where(df['wind_degrees_compass'] >= 270, 1, 0)

lag_vars = ['pm25', 'wind_direction_sin', 'wind_direction_cos', 'wind_knots', 'temperature',
	'dewpoint', 'pct_humidity', 'ozone', 'so2', 'co', 'no2']

for var in lag_vars:
	# lagged features
	print "beginning to add ", var, "lagged features..."
	df = lag_feature(df, var, 1)
	df = lag_feature(df, var, 2)
	df = lag_feature(df, var, 3)
	df = lag_feature(df, var, 4)
	df = lag_feature(df, var, 5)
	# lagged moving averages
	print "\tand now adding the lagged and averaged version"
	df = lag_avg_feature(df, feat_var=var, lag_days=1, window=1, window_units='days')
	df = lag_avg_feature(df, feat_var=var, lag_days=2, window=1, window_units='days')
	df = lag_avg_feature(df, feat_var=var, lag_days=3, window=1, window_units='days')
	df = lag_avg_feature(df, feat_var=var, lag_days=4, window=1, window_units='days')
	df = lag_avg_feature(df, feat_var=var, lag_days=5, window=1, window_units='days')


df.to_csv(data_dir + 'houston_features.csv', index=False)

# df.shape
# df.columns.tolist()
# df.columns.to_series().groupby(df.dtypes).groups
# df.describe()


# TODO: spatial features, pick random locations, compute distance from each station to the locations



