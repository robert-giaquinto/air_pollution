from __future__ import division
import numpy as np
import pandas as pd
import math
from scipy.stats import rankdata


def import_region(region, filename='all_texas.csv'):
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

	# fix dates
	df['Date_Local'] = pd.DatetimeIndex(df.Date_Local).normalize()

	# add in some keys for easily merging dates
	df['datetime_key'] = datetime_to_key(df.Date_Local, df.Time_Local)
	df['date_key'] = date_to_key(df.Date_Local)
	# location key:
	df['location_key'] = set_location_key(df.County_Code, df.Site_Num)

	# rename long variables
	new_col_names = {'RH_Dewpoint_degrees_fahrenheit': 'dewpoint', 'RH_Dewpoint_percent_relative_humidity': 'pct_humidity'}
	df.rename(columns=new_col_names, inplace=True)

	# don't allow pm25 to be negative
	df[df.pm25 < 0] = 0
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

	# merge the lagged statistis with original data
	df = pd.merge(df, agg_df, how="left",
		left_on=['lagged_key', 'location_key'],
		right_on=[key_var, 'location_key'],
		suffixes=('', '_lag' + str(lag_days) + '_agg' + str(window)),
		sort=False)
	df.drop(['lagged_key',
		key_var + '_lag' + str(lag_days) + '_agg' + str(window)],
		axis=1, inplace=True)
	return df


def date_to_key(date):
	rval = ((pd.DatetimeIndex(date).year * 10000) +
		(pd.DatetimeIndex(date).month * 100) +
		pd.DatetimeIndex(date).day)
	return rval


def datetime_to_key(date, time):
	rval = ((pd.DatetimeIndex(date).year * 1000000) +
		(pd.DatetimeIndex(date).month * 10000) +
		(pd.DatetimeIndex(date).day * 100) +
		time)
	return rval


def set_location_key(county, site):
	rval = ((county * 1000000) + site)
	return rval













