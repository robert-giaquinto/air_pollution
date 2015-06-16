from __future__ import division
import os
from urllib2 import urlopen, URLError, HTTPError
import zipfile
import numpy as np
import pandas as pd

# A list of all files for download can be found here:
# http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/file_list.csv


def dlfile(url):
	# Open the url
	try:
		f = urlopen(url)
		print "downloading " + url

		# Open our local file for writing
		with open(os.path.basename(url), "wb") as local_file:
			local_file.write(f.read())

	#handle errors
	except HTTPError, e:
		print "HTTP Error:", e.code, url
	except URLError, e:
		print "URL Error:", e.reason, url



# TODO: download the data if it has been updated
def unzip_hourly(file_name, yr_range, keep_region=None):
	"""

	:param file_num:
	:param yr_range:
	:return:
	"""
	dfs = []
	for yr in yr_range:
		base_name = ("hourly_%s_%d" % (file_name, yr))

		# download the data set if needed
		if base_name + ".zip" not in os.listdir(os.curdir):
			# may want always download the 2014 file...
			url = "http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/" + base_name + ".zip"
			dlfile(url)

		# open csv file, clean it up
		csv_file = zipfile.ZipFile(base_name + ".zip")

		# only read in certain variables
		keep_vars = [
			'Date Local', 'Time Local',
			'State Code', 'County Code', 'Site Num',
			'Sample Measurement', 'Units of Measure',
			'Latitude', 'Longitude'
		]

		# read in csv in the zip file using csv reader
		df = pd.read_csv(
			csv_file.open(base_name + ".csv"),
			parse_dates=['Date Local'],
			usecols=keep_vars
		)

		df[['Latitude', 'Longitude']] = df[['Latitude', 'Longitude']].astype(float)
		if keep_region is not None:
			df = df.loc[ (df.Latitude > keep_region['lat_min']) &
				(df.Latitude < keep_region['lat_max']) &
				(df.Longitude > keep_region['lon_min']) &
				(df.Longitude < keep_region['lon_max']), ]

		# clean up columns names
		df.rename(columns=lambda x: x.replace(' ', '_'), inplace=True)

		# PULL OUT TEXAS
		df = df.loc[df.State_Code == 48, ]

		# clean up time stamps
		df.Time_Local = df.Time_Local.str.split(':').str.get(0).astype(int)
		# remove time portion of date
		df.Date_Local = df.Date_Local.apply(lambda x: x.date())

		# merge the second statistic if exists
		# (converting data from long to wide)
		measurements = np.unique(df.Units_of_Measure).tolist()
		if len(measurements) > 1:
			key_vars = ['Date_Local', 'Time_Local', 'State_Code', 'County_Code', 'Site_Num']
			# save all columns and one of the statistics
			first_measure = measurements.pop()
			main_df = df[first_measure == df.Units_of_Measure]
			main_df.rename(columns={'Sample_Measurement': first_measure.replace(' ', '_')}, inplace=True)
			for i in measurements:
				# incrementally add the remaining statistics
				aug_df = df[i == df.Units_of_Measure][key_vars + ['Sample_Measurement']]
				aug_df.rename(columns={'Sample_Measurement': i.replace(' ', '_')}, inplace=True)
				main_df = pd.merge(main_df, aug_df, on=key_vars, how='inner')
				del aug_df
			del df
			df = main_df
			del main_df
		df.drop('Units_of_Measure', axis=1, inplace=True)
		dfs.append(df)

	if len(yr_range) > 1:
		rval = pd.concat(dfs)
	else:
		rval = df
	# rval.set_index(key_vars, inplace=True)
	# rval.sort_index(inplace=True)
	return rval


def merge_sources(yr_range, file_names, keep_region=None):
	key_vars = ['Date_Local', 'Time_Local', 'State_Code', 'County_Code', 'Site_Num']
	for i, elt in enumerate(file_names):
		# recursively add features from the epa's download files
		file_name = elt[0]  # file name
		col_label = elt[1]  # alias in dataframe

		if i == 0:
			# initialize dataframe
			rval = unzip_hourly(file_name=file_name, yr_range=yr_range, keep_region=keep_region)
			rval.rename(columns={'Sample_Measurement': col_label}, inplace=True)
		else:
			# add a new feature to the dataframe
			feature = unzip_hourly(file_name=file_name, yr_range=yr_range, keep_region=keep_region)
			feature.drop(['Latitude', 'Longitude'], axis=1, inplace=True)
			feat_vars = [x for x in feature.columns.tolist() if x not in key_vars]
			feat_names = {k: '_'.join([col_label, k]).replace('_Sample_Measurement', '') for k in feat_vars}
			feature.rename(columns=feat_names, inplace=True)
			rval = pd.merge(rval, feature, on=key_vars, how='left')
	return rval


# import and merge data
file_names = [
	('88502', 'pm25'),
	# ('88101', 'pm25_frm'),
	('81102', 'pm10'),
	('WIND', 'wind'),
	('TEMP', 'temperature'),
	('PRESS', 'pressure'),
	('RH_DP', 'RH_Dewpoint'),
	('44201', 'ozone'),
	('42401', 'so2'),
	('42101', 'co'),
	('42602', 'no2'),
	('HAPS', 'HAPs'),
	('VOCS', 'VOCs')
]

region = {'lat_min': 41.0,
	'lat_max': 49.0,
	'lon_min': -125.0,
	'lon_max': -108.0}

data = merge_sources(yr_range=range(2000, 2015), file_names=file_names)
print "done downloading and merging"

# write to csv file
data.to_csv('texas.csv', index=False)
print "done with everything"

