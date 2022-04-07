# Data Pipeline for Modeling

The scripts fuse multiple data sources and prepare the data for model training. All required R and python package needs to be installed before running scripts. 

## Prepare input data

Two data files specified below are needed for volume estimation. This ground truth data is used for training and should be recorded by continuous and/or short-term traffic counting stations *in-situ*. Note that these data are not always available for all regions, and the data may be collected and provided in disparate formats that need preparation to be ingested in this data pipeline. The specific names of columns can be adjusted in [`DefaultValues.R`](https://github.com/NREL/TrafficVolumeEstimation/blob/master/data_pipeline/DefaultValues.R) The following specifies how these files should be formatted:

1. [`stations.csv`](https://github.com/NREL/TrafficVolumeEstimation/blob/master/data_pipeline/stations.csv). The station data file should contain the following columns. If functional class and number of lanes information can not be found, fill the columns with 0s.

    * Station ID: the unique ID of the volume count location
  
    * Latitude: count station location latitude
  
    * Longitude: count station location longitude
  
    * Functional class: road functional class at the count station location
  
    * Number of lanes: number of lanes at the count station location

1. [`volume_24hr.csv`](https://github.com/NREL/TrafficVolumeEstimation/blob/master/data_pipeline/volume_24hr.csv). The volume counts data file should contain the following columns. 

    * Station ID: the unique ID of the volume count location
    
    * Direction: traffic direction
    
    * Date: data collection date
    
    * Hour of day (0-23): traffic volume of the hour. (e.g.  0 is traffic volume of 0:00-1:00 am, 17 is traffic volume of 5:00-6:00 pm)

## Run the code

After retrieving and preparing the data:

1. Specify the values in [`DefaultValues.R`](https://github.com/NREL/TrafficVolumeEstimation/blob/master/data_pipeline/DefaultValues.R) script

1. Run `Rscript JoinStationsWithVolume.R` to join the station table with the traffic volume table and output the joined data at `volume_24hr.rds`. A data log is created and saved at `data_log.rds`. 

1. Run `Rscript QueryTomtomData.R` to query TomTom network and probe data. The request job IDs and download URLs are generated and saved at `jobs.rds` and `url_zip.rds`. 

1. Run `Rscript DownloadTomtomData.R` to to download data. The data is downloaded at `./data/`

1. Run `Rscript ProcessTomtomData.R` to process TomTom data. The processed TomTom data is saved at `tomtom_data.rds`. 

1. Run `Rscript JoinTomtomWithVolume.R` to join the TomTom data and weather data with volume data. The final data is saved at `final_data.rds`

1. Run `Rscript DataExploration.R` to plot distributions and visualize data. Plots are saved at `./data_exploration`.

1. Run `Rscript CreateTrainingData.R` to create training and test data for modeling. The training and test data are saved as `final_train_data.csv` and `final_test_data.csv`.

1. Run `Rscript UnivariateAnalysis.R` to conduct univariate analysis. Plots are saved at `./univariate_analysis`.

