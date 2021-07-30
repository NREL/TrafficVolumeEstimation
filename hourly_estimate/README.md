# Volume Estimation for a City

The scripts estimate and visualize hourly traffic volume for a whole day. Before running scripts, all required R and python package needs to be installed and copy the `xgb.dat` from `hourly_model` to this folder.

```bash
# Send TomTom API POST request to query TomTom network and probe data for a whole day on date yyyy-mm-dd
# A request job ID is generated and stored at ./job/yyyy-mm-dd_job.rds
Rscript QueryTomTomData.R yyyy-mm-dd 

# Send TomTom API GET request to download data
# It takes a few hours for TomTom to process the POST request before available for downloading. The data is downloaded at ./yyyy-mm-dd/
Rscript DownloadTomTomData.R yyyy-mm-dd 

# Generate input data for volume estimation
# The input data is stored at ./yyyy-mm-dd/daily_data.csv
Rscript CreateInputData.R yyyy-mm-dd 

# Run a pre-trained model to estimate volume
# The estimate volume results is stored at ./yyyy-mm-dd/daily_data_pred.csv
python EstimateTrafficVolume.py yyyy-mm-dd  

# Save estimated results in a shapefile
Rscript SaveResults.R yyyy-mm-dd 

# Generate visualizations for estimated volume
# Needs to specify the hour of day. 
# For example, Rscript visualize.R 2017-02-02 1 8 17 23 visualize traffic volume for 1-2am, 8-9am, 5-6pm and 23-24pm. Visulaizations are stored as ./yyyy-mm-dd/volume_h.html
Rscript Visulaization.R yyyy-mm-dd h h h ... 

 ```

<p align="center">
  <img src="/hourly_estimate/volume.png" )
</p>
