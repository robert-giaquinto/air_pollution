from __future__ import division
import os
from urllib2 import urlopen, URLError, HTTPError
import zipfile
import pandas as pd
import numpy as np
from data_utilities import *

# A list of all files for download can be found here:
# http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/file_list.csv

def dlfile(url, data_dir):
	# Open the url
	try:
		f = urlopen(url)
		print "downloading " + url

		# Open our local file for writing
		with open(data_dir + os.path.basename(url), "wb") as local_file:
			local_file.write(f.read())

	# handle errors
	except HTTPError, e:
		print "HTTP Error:", e.code, url
	except URLError, e:
		print "URL Error:", e.reason, url


# TODO: download the data if it has been updated
def unzip_hourly(file_info, yr_range, data_dir, keep_region=None, state_num=None, county_num=None, return_agg=False):
	"""
	imprt the data, either from local zip files or the air data website as needed.
	apply some basic preprocessing and subsetting of the dataset
	:param file_num:
	:param yr_range:
	:return:
	"""
	dfs = []
	file_id = file_info[0]
	file_label = file_info[1]
	for yr in yr_range:
		print "\tyear:", yr
		base_name = ("hourly_%s_%d" % (file_id, yr))

		# DOWNLOAD OR IMPORT --------------------------------------------------
		if base_name + ".zip" not in os.listdir(data_dir):
			url = "http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/" + base_name + ".zip"
			dlfile(url, data_dir)

		# open csv file, clean it up
		csv_file = zipfile.ZipFile(data_dir + base_name + ".zip")

		# only read in certain variables
		keep_vars = ['Date Local', 'Time Local', 'State Code', 'County Code', 'Site Num',
			'Sample Measurement', 'Units of Measure', 'Latitude', 'Longitude']

		# read in csv in the zip file using csv reader
		df = pd.read_csv(csv_file.open(base_name + ".csv"),
			parse_dates=['Date Local'],
			usecols=keep_vars)

		# clean up spaces in column names
		df.rename(columns=lambda x: x.replace(' ', '_'), inplace=True)

		# SUBSET RESULTS ------------------------------------------------------
		# keep a specific latitude and longitudinal region?
		df[['Latitude', 'Longitude']] = df[['Latitude', 'Longitude']].astype(float)
		if keep_region is not None:
			df = df.loc[(df.Latitude > keep_region['lat_min']) &
				(df.Latitude < keep_region['lat_max']) &
				(df.Longitude > keep_region['lon_min']) &
				(df.Longitude < keep_region['lon_max']), ]

		# reduce to a specific state or county code?
		if state_num is not None:
			df = df.loc[df.State_Code == state_num, ]
		if county_num is not None:
			df = df.loc[df.County_Code == county_num, ]

		# BASIC PRE-PROCESSING ------------------------------------------------
		# clean up time stamps
		df.Time_Local = df.Time_Local.str.split(':').str.get(0).astype(int)
		# remove time portion of date
		df['Date_Local'] = pd.DatetimeIndex(df.Date_Local).normalize()

		# add in some keys for easily merging dates
		df['datetime_key'] = datetime_to_key(df.Date_Local, df.Time_Local)
		df['date_key'] = date_to_key(df.Date_Local)
		# location key:
		df['location_key'] = set_location_key(df.County_Code, df.Site_Num)

		# aggregate stats if there are dupes (i.e. 2 measurements from same location and time)
		agg_df = df.groupby(['datetime_key', 'location_key', 'Units_of_Measure']).Sample_Measurement.agg([np.mean])
		measurements = np.unique(df.Units_of_Measure).tolist()
		if len(measurements) > 1:
			# pivot on units_of_measure, and treat indices as columns
			# this will transform from long to wide
			agg_df = agg_df.unstack().reset_index()
			# fix names of columns
			level0 = agg_df.columns.get_level_values(0)
			level1 = agg_df.columns.get_level_values(1)
			agg_df.columns = [file_label + '_' + level1[x].replace(' ', '_').lower()
				if level1[x] != ''
				else level0[x]
				for x
				in range(agg_df.shape[1])]
		else:
			# fix the name
			agg_df.reset_index(inplace=True)
			agg_df.rename(columns={"mean": file_label}, inplace=True)
			agg_df.drop('Units_of_Measure', axis=1, inplace=True)

		# merge aggregated measurements to the unique, non-measurement variables
		if return_agg:
			dfs.append(agg_df)
		else:
			uniq_vars = ['State_Code', 'County_Code', 'Site_Num',
			'Latitude', 'Longitude',
			'Date_Local', 'Time_Local',
			'datetime_key', 'date_key', 'location_key']
			uniq_df = df[uniq_vars].drop_duplicates(subset=['datetime_key', 'location_key'])
			df = pd.merge(uniq_df, agg_df, on=['datetime_key', 'location_key'], how='left', sort=False)
			# save the results
			dfs.append(df)

	# return results
	if len(yr_range) > 1:
		# stack all the data frames on top of one-another
		rval = pd.concat(dfs)
	else:
		rval = df
	return rval


def import_data(yr_range, file_names=None, data_dir=os.curdir, keep_region=None, state_num=None, county_num=None):
	"""
	Merge features spread across multiple files into a single data frame
	:param yr_range:
	:param file_names:
	:param data_dir:
	:param keep_region:
	:param state_num:
	:param county_num:
	:return:

	Example of the region subset parameter
	region = {'lat_min': 41.0,
		'lat_max': 49.0,
		'lon_min': -125.0,
		'lon_max': -108.0}

	example of file_names list
	file_names =[('88502', 'pm25'),
		('WIND', 'wind'),
		('TEMP', 'temperature'),
		('RH_DP', 'RH_Dewpoint'),
		('44201', 'ozone'),
		('42401', 'so2'),
		('42101', 'co'),
		('42602', 'no2')]
	"""
	if file_names is None:
		file_names =[('88502', 'pm25'),
			('WIND', 'wind'),
			('TEMP', 'temperature'),
			('RH_DP', 'RH_Dewpoint'),
			('44201', 'ozone'),
			('42401', 'so2'),
			('42101', 'co'),
			('42602', 'no2')]
	key_vars = ['datetime_key', 'location_key']
	for i, file_info in enumerate(file_names):
		print "adding feature:", file_info[1]
		# recursively add features from the epa's download files
		if i == 0:
			# initialize dataframe
			rval = unzip_hourly(file_info=file_info,
				yr_range=yr_range,
				data_dir=data_dir,
				keep_region=keep_region,
				state_num=state_num,
				county_num=county_num)
		else:
			# add a new feature to the dataframe
			feature = unzip_hourly(file_info=file_info,
				yr_range=yr_range,
				data_dir=data_dir,
				keep_region=keep_region,
				state_num=state_num,
				county_num=county_num,
				return_agg=True)
			rval = pd.merge(rval, feature, on=key_vars, how='left', sort=False)
	return rval

