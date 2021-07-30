# AADT Estimation for a City

The scripts estimate and visualize AADT. Before running scripts, all required R and python package needs to be installed and copy the `xgb.dat` from `adt_model` to this folder. 

```bash
# Query and download TomTom data for each day of week and month combination
Rscript multi_dates_tomtom_query.R 

# Generate input data for volume estimation
bash create_input_data.sh 

# Run a pre-trained model to estimate volume
bash estimate_traffic_volume.sh  

# Save estimated results in a shapefile
bash save_results.sh 

# Generate visualizations for estimated volume
Rscript Visulaization.R 

 ```

<p align="center">
  <img src="/aadt_estimate/AADT.png" )
</p>
