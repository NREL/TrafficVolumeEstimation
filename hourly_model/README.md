# Hourly Volume Model Training and Test

This notebook build XGBoost model for hourly volume estimation and use bayesian optimization to automatically tune hyperparameters. Before running the notebook, copy `final_train_data.csv` and `final_test_data.csv` from `data_pipeline` to this folder. A few values needs to be specified in the notebook.

1. `validation_only`: `True` if only training data is available, `False` if both training and test data is available. 

1. `n_nodes`: number of computer nodes used for model training. 

1. `trained`: `True` if model is already trained, `False` if model is not trained.

1. `cols_drop`: columns that need to be removed. 

1. `X_name_dict`: specify input data column names that will displayed in the variable importance plot. 

After training and validate the model, run `results.R` to calculate error metrics for hourly volume estimation.