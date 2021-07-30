# File names
volume_filename = 'volume_24hr.csv' # Volume data file name
station_filename = 'stations.csv' # Station data file name

# Station data parameters
station_id_colname = 'STATIONID' # Station ID column name
long_colname = 'LONG' # Longitude column name
lat_colname = 'LAT' # Latitude column name
num_lane_colname ='LRS_NUMBERLANE' # Lane count column name
fc_colname = 'FC' # Functional Class column name

# Volume data parameters
station_id_colname2 = 'STATIONID' # Station ID column name
dir_colname = 'Dir' # Direction column name
date_colname = 'Date' # Date column name
date_format = '%Y-%m-%d' # Date format
date_min = '2018-07-01' # Start Date
date_max = '2018-12-31' # End Date
hour_colnum = 4:27 # Column numbers of hours in volume_24hr.csv

# Plot stations on map
station_radius = 10 # Radius of circle representing stations

# TomTom probe data parameters
tomtom_api_key = ""
tomtom_map_version = "2018.12"
tomtom_Id_colnum = 1 # Column numbers of TomTom Id
tomtom_AvgTt_colnum = 2 # Column numbers of average travel time
tomtom_AvgSp_colnum = 5 # Column numbers of average speed
tomtom_count_colnum = 9 # Column numbers of probe count

# Time zone
time_zone = 'US/Eastern' # Values: "US/Eastern", "US/Central", "US/Mountain", "US/Pacific", "US/Alaska", "US/Hawaii"

# Direcrion code
north_code = 'N' # Code for north
northeast_code = 'NE' # Code for northeast
east_code = 'E' # Code for east
southeast_code = 'SE' # Code for southeast
south_code = 'S' # Code for south
southwest_code = 'SW' # Code for southwest
west_code = 'W' # Code for west
northwest_code = 'NW' # Code for northwest
oneway_code = 'OW' # Code for one-way
both_code = 'BOTH' # Code for both directions

# API key for the weather company
weather_api_key = ''
weather_batch_query = F

# Data exploration
volume_radius_weight = 400 # Radius weight for volume plot. Default values: freeway: 400, off-freeway: 4000
ProbeCount_radius_weight = 500 # Radius weight for probe count plot. Default values: freeway: 500, off-freeway: 5000
PenRate_radius_weight = 20000 # Radius weight for penetration rate plot. Default values: freeway: 20000, off-freeway: 200000
volume_hist_bin = 100 # Volume histogram bin size
volume_break_interval = 1000 # Volume histogram X-axis break interval
probe_hist_bin = 10 # Probe count histogram bin size
prob_break_interval = 100 # Probe count histogram X-axis break interval
snapshot_hours = 500 # random sample size to plot probe count vs. volume

# Create training data
removed_stations = c(846, 206, 390) # Station IDs to be removed from training
nfold = 10 # Number of folds for cross validation
validation_only = T # True: no test data generated
leave_one_out = T # True: leave-one-out validation
test_sample_method = 'one of each' # Sampling method to create test data. Values: 'random', 'stratified', 'one of each'
test_sample_ratio = 0.2 # Test data sampling ratio

#Univariate analysis
y_limits = c(0, 3000) # y-axis limits for univariate plots 
long_bin = 0.1 # Bin size of longitude
lat_bin = 0.1 # Bin size of latitude
probe_bin = 50 # Bin size of probe count


