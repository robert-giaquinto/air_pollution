from __future__ import division
import numpy as np
import pandas as pd
from helper_funcs import import_region, make_binary_features, make_cyclic_feature, date_to_key, datetime_to_key

# read in the texas dataset
data_dir = '/Users/robert/Documents/UMN/air_pollution/data/'
df = import_region(region="houston", filename=data_dir + 'all_texas.csv')
df.shape
df.columns.tolist()
df.columns.to_series().groupby(df.dtypes).groups
df.describe()


df.groupby(['County_Code', 'Site_Num']).State_Code.aggregate([len])

test = df.loc[df.datetime_key == 2014093023,]
test.shape


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

df['day'] = pd.DatetimeIndex(df.Date_Local).day
df['day_cycle'] = make_cyclic_feature(df.day)
df = make_binary_features(df, 'day')

df['quarter'] = pd.DatetimeIndex(df.Date_Local).quarter
df['quarter_cycle'] = make_cyclic_feature(df.quarter)
df = make_binary_features(df, 'quarter')


# build lagged versions of a variable
feat_var = 'pm25'
lag_days = 5
# df['lagged_date'] = pd.DatetimeIndex(df.Date_Local) - pd.DateOffset(lag_days)
df['lagged_datetime_key'] = df.datetime_key - lag_days
lagged_data = df[['datetime_key', 'location_key', feat_var]]
df = pd.merge(df, lagged_data, how="left",
	left_on=['lagged_datetime_key', 'location_key'],
	right_on=['datetime_key', 'location_key'],
	suffixes=('', '_lag' + str(lag_days)),
	sort=False)
df.drop(['lagged_datetime_key', 'datetime_key_lag5'], axis=1, inplace=True)


test = date_to_key(df.Date_Local)
test = datetime_to_key(df.Date_Local, df.Time_Local)

test.columns
test.shape
df.shape



