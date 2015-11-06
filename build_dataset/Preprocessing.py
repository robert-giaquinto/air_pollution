import numpy as np
import pandas as pd
import math
from scipy.stats import rankdata
from data_utilities import *


def extract_region(region, filename='all_texas.csv'):
	"""
	My study focuses primarly on the texas region, this is a help function that imports
	:param region:
	:param filename:
	:return:
	"""
	if region == "houston":
		region = {'lat_min': 29.25,
			'lat_max': 30.35,
			'lon_min': -95.43,
			'lon_max': -94.08}
	elif region == "dallas":
		region = {'lat_min': 31.65,
			'lat_max': 33.86,
			'lon_min': -98.57,
			'lon_max': -94.17}
	elif region == "san antonio":
		region = {'lat_min': 28.70,
			'lat_max': 30.48,
			'lon_min': -103.18,
			'lon_max': -97.71}
	else:
		print "only accepting regions: 'houston', 'dallas', 'san antonio'"
		return 0
	df = pd.read_csv(filename, parse_dates=['Date_Local'])
	df = df.loc[(df.Latitude >= region['lat_min']) &
		(df.Latitude <= region['lat_max']) &
		(df.Longitude >= region['lon_min']) &
		(df.Longitude <= region['lon_max']), ]
	# reset the index after dropping rows
	df = df.reset_index(drop=True)
	return df


def make_binary_features(df, feat_name, drop_original=False):
	dummies = pd.get_dummies(df[feat_name].values, prefix=feat_name)
	rval = pd.concat([df, dummies], axis=1)
	if drop_original:
		rval.drop([feat_name], axis=1, inplace=True)
	return rval


def make_cyclic_feature(feat, num_digits=2):
	num_vals = 1.0 * len(pd.unique(feat))
	# make feature cyclic via sin curve
	# rank the values to give equal spacing between points
	# can use number of rounding digits to control number of unique values
	rval = rankdata(np.round(np.sin(((feat - feat.min()) / num_vals) * math.pi), num_digits), method='dense')
	return rval


def lag_feature(df, feat_var, lag_days):
	"""
	Add feature lagged by lag_days and lag_hours as a new feature
	:param df:
	:param feat_var:
	:param lag_days:
	:return:
	"""
	# extract a view of the dataset to later re-merge into the original data
	lagged_data = df[['datetime_key', 'location_key', feat_var]]

	df['lagged_date'] = pd.DatetimeIndex(df.Date_Local) - pd.DateOffset(lag_days)
	df['lagged_key'] = datetime_to_key(df.lagged_date, df.Time_Local)
	df = pd.merge(df, lagged_data, how="left",
		left_on=['lagged_key', 'location_key'],
		right_on=['datetime_key', 'location_key'],
		suffixes=('', '_lag' + str(lag_days)),
		sort=False)
	df.drop(['lagged_key',
		'lagged_date',
		'datetime_key_lag' + str(lag_days)],
		axis=1, inplace=True)
	return df


def lag_avg_feature(df, feat_var, lag_days, window, window_units="days"):
	"""

	:param df:
	:param feat_var:
	:param lag_days: how far back in days to lag
	:param window: how many hours/days to average over
	:return:
	"""
	if window_units == "days":
		key_var = "date_key"
		df['lagged_key'] = date_to_key(pd.DatetimeIndex(df.Date_Local) - pd.DateOffset(lag_days))
	elif window_units == "hours":
		key_var = "datetime_key"
		df['lagged_key'] = datetime_to_key(pd.DatetimeIndex(df.Date_Local) - pd.DateOffset(lag_days), df.Time_Local)
	else:
		print "error"
		return 1

	# aggregate the data to unique values by datetime
	agg_df = df[['location_key', key_var, feat_var]].groupby(['location_key', key_var])[feat_var].agg([np.sum, len]).reset_index()
	agg_df.rename(columns={'sum': 'total', 'len': 'ct'}, inplace=True)

	# compute running totals for each location
	agg_df['rolling_total'] = agg_df.groupby('location_key')['total'].apply(pd.rolling_sum, window)
	agg_df['rolling_ct'] = agg_df.groupby('location_key')['ct'].apply(pd.rolling_sum, window)

	# impute missing data from the data available
	agg_df.loc[agg_df['rolling_total'].isnull(), 'rolling_total'] = agg_df['total'] * window
	agg_df.loc[agg_df['rolling_ct'].isnull(), 'rolling_ct'] = agg_df['ct'] * window

	# combine to sum and count to create final moving average
	agg_df[feat_var] = agg_df.rolling_total / agg_df.rolling_ct
	agg_df.drop(['total', 'ct', 'rolling_total', 'rolling_ct'], axis=1, inplace=True)

	# merge the lagged statistics with original data
	df = pd.merge(df, agg_df, how="left",
		left_on=['lagged_key', 'location_key'],
		right_on=[key_var, 'location_key'],
		suffixes=('', '_lag' + str(lag_days) + '_agg' + str(window)),
		sort=False)
	df.drop(['lagged_key',
		key_var + '_lag' + str(lag_days) + '_agg' + str(window)],
		axis=1, inplace=True)
	return df


# TODO: spatial features, pick random locations, compute distance from each station to the locations
def preprocess_data(df, verbose=False):
	# BASIC CLEAN-UP
	# rename long variables
	new_col_names = {'RH_Dewpoint_degrees_fahrenheit': 'dewpoint', 'RH_Dewpoint_percent_relative_humidity': 'pct_humidity'}
	df.rename(columns=new_col_names, inplace=True)

	# don't allow pm25 to be negative
	df[df.pm25 < 0] = 0

	# IMPUTATION
	# impute missing values with the average on a particular datetime
	impute_vars = ['wind_degrees_compass', 'wind_knots', 'temperature', 'dewpoint', 'pct_humidity', 'ozone', 'so2', 'co', 'no2']
	for var in impute_vars:
		if verbose:
			print "Pct Missing in", var + ":", round(df[var].isnull().sum()/len(df)*100, 1)
		df[var] = df.groupby("datetime_key")[var].transform(lambda x: x.fillna(x.mean()))
		if verbose:
			print "\tReduced to:", round(df[var].isnull().sum()/len(df)*100, 1)

	# if anything is still missing, average over the entire day and all locations
	impute_vars = ['dewpoint', 'pct_humidity', 'ozone', 'so2', 'co', 'no2']
	for var in impute_vars:
		if verbose:
			print "Pct Missing in", var + ":", round(df[var].isnull().sum()/len(df)*100, 1)
		df[var] = df.groupby("date_key")[var].transform(lambda x: x.fillna(x.mean()))
		if verbose:
			print "\tReduced to:", round(df[var].isnull().sum()/len(df)*100, 1)

	# if anything is still missing, average over the entire locations
	impute_vars = ['dewpoint', 'pct_humidity', 'so2', 'co', 'no2']
	for var in impute_vars:
		if verbose:
			print "Pct Missing in", var + ":", round(df[var].isnull().sum()/len(df)*100, 1)
		df[var] = df.groupby("location_key")[var].transform(lambda x: x.fillna(x.mean()))
		if verbose:
			print "\tReduced to:", round(df[var].isnull().sum()/len(df)*100, 1)

	# any missings left?
	if verbose:
		for var in df.columns.tolist():
			print "Num Missing in", var + ":", df[var].isnull().sum()

	# DATE FEATURES
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
		if verbose:
			print "beginning to add ", var, "lagged features..."
		df = lag_feature(df, var, 1)
		df = lag_feature(df, var, 2)
		df = lag_feature(df, var, 3)
		df = lag_feature(df, var, 4)
		df = lag_feature(df, var, 5)
		# lagged moving averages
		if verbose:
			print "\tand now adding the lagged and averaged version"
		df = lag_avg_feature(df, feat_var=var, lag_days=1, window=1, window_units='days')
		df = lag_avg_feature(df, feat_var=var, lag_days=2, window=1, window_units='days')
		df = lag_avg_feature(df, feat_var=var, lag_days=3, window=1, window_units='days')
		df = lag_avg_feature(df, feat_var=var, lag_days=4, window=1, window_units='days')
		df = lag_avg_feature(df, feat_var=var, lag_days=5, window=1, window_units='days')
	return df












