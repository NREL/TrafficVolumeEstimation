# Ubiquitous Traffic Volume Estimation

By utilizing information from existing traffic sensors (i.e., continuous count stations and short-term counts), and combining it with vehicle probe data, as well as other relevant information (e.g., weather and road geometry), this project estimates vehicle volumes across the entire road network for any region of interest. This repo contains all the modules of the code involved in the workflow to estimate ubiquitous (24x7x365) hourly traffic volumes, as well as Average Annual Daily Traffic (AADT). Specifically:

- The `data pipeline` module has code required for preparing input data (from multiple sources) for training the machine learning model
- The `hourly_model` module contains the code required for training and testing the hourly volume estimation model
- The `hourly_estimate` module contains the code to apply the model estimated using ‘hourly_model’ for any given region of interest
- The `adt_model` module contains the code required to estimate Average Annual Daily Traffic (AADT) based on Average Daily Traffic (ADT) values
- The `aadt_estimate` module contains the code to apply the model estimated using ‘adt_model’ for any given region of interest
- The `visualize` module contains the code required to visualize the results from the ‘hourly_model’, 'adt_model', hourly_estimate’, or 'aadt_estimate'

## Hourly Volume Estimation

The hourly traffic volume estimation work flow is as follows: Data pipeline -> Train and validate hourly volume models -> Summarize and visualize results -> Estimate hourly volume for the entire network.

The figure below demonstrates hourly volume estimation in Chattanooga, TN.

<p align="center">
  <img src="/hourly_estimate/volume.png" )
</p>


## AADT Estimation
  
The AADT estimation work flow is as follows: Data pipeline -> Train and validate ADT volume models -> Summarize and visualize results -> Visualization -> Estimate AADT for the entire network.
 
The figure below demonstrates AADT volume estimation in Worcester, MA.
  
<p align="center">
  <img src="/aadt_estimate/AADT.png" )
</p>
  
## Authors & Contact
  
The primary author of this codebase is Yi Hou. The principal investigator for this project is Venu Garikapati. For more information or if you have any questions, please contact Chris Hoehne (christopher.hoehne@nrel.gov).
