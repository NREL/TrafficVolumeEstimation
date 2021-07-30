# Visualize Model Results

Use this sub-module to generate visualizations for the model results from XGBoost.

1. Check the parameters in `visualize/viz-params.R`. For instance, the default setting will drop stations that have fewer than 5 observations.

2. Copy output file(s) from the model data pipeline into `visualize/input`. Each file will be visualized separately. If you want to visualize two sets of results in the same figures, bind the files together before running. If the expected columns are missing or named incorrectly, the visualization will not work properly. check `visualize/input/sample_input.csv` for an example of what the data should look like (this file is ignored automatically and is an example of what the output from the data pipeline should look like).

3. Run `visualize/viz-plots.R`. If this is the first time running this script on a machine and some of the required R packages are not installed, they will automatically be installed for you at the start.

The cleaned results data and figures will be output in `visualize/output/[name_of_the_input_file]/` for each input file that was in `visualize/input`

NOTES:

  - Ignores the `FC` variable assumes the functional road class (`FRC` variable) classification for labeling road classes such that 0 = interstates and 6+ = local roads (labeling all FRC above 6 to be local. See [here](https://github.nrel.gov/MBAP/VolumeEstimation/blob/develop/visualize/viz-plots.R#L50) for the specific labeling.
  - Some plots labeling or display of text may not be ideal; in these cases, manual adjustments may be necessary (such as decreasing the font size, e.g. `ggplot2::theme(legend.text = element_text(size = 8)`)
