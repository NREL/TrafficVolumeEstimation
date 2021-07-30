import pandas as pd
import numpy as np
import pickle
import sys
from xgboost import XGBRegressor

query_month = sys.argv[1]
query_wkd = sys.argv[2]
folder_name = query_month + '_' + query_wkd

def load_model(path):
    f = open(path, 'rb')
    clf = pickle.load(f)
    f.close()
    return(clf)

# load trained model
xgb = load_model('xgb.dat')

# run the model to estimate volume
daily_data_df = pd.read_csv("./" + folder_name + "/daily_data.csv")
daily_data_df = daily_data_df.loc[daily_data_df.FRC<=1, :]
daily_data = daily_data_df.drop(['Id'], axis=1).values
daily_data_pred = xgb.predict(daily_data)
daily_data_pred[daily_data_pred<0]=0

# output volume estimates
network_pred = pd.DataFrame(np.concatenate((daily_data_df.values, daily_data_pred.reshape(-1, 1)), axis=1),
                              columns = daily_data_df.columns.append(pd.Index(['pred_volume'])))
network_pred.to_csv("./" + folder_name + '/daily_data_pred.csv', index = False)
