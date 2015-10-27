import pandas as pd


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