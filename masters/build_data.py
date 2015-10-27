from __future__ import division
import os
from Importing import *
from Preprocessing import *

def build_data(data_dir):
	# IMPORT DATA
	# read the data from air now website or import it from local zip files (if they exist on data_dir
	if 'all_texas.csv' not in os.listdir(data_dir):
		# import and combine features from different files
		raw_data = import_data(yr_range=range(2000, 2015), data_dir=data_dir, state_num=48)
		# write to csv file
		raw_data.to_csv(data_dir + 'all_texas.csv', index=False)
		del raw_data

	# PREPROCESS DATA
	# read in the texas dataset
	# import one region (houston) of all the texas data
	if 'houston_features.csv' in os.listdir(data_dir):
		df = extract_region(region="houston", filename=data_dir + 'all_texas.csv')

		# clean up and make new features
		rval = preprocess_data(df)
		rval.to_csv(data_dir + 'houston_features.csv', index=False)
	else:
		rval = pd.read_csv(data_dir + 'houston_features.csv')

	return rval


