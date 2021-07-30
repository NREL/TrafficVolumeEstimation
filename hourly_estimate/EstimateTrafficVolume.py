import pandas as pd
import numpy as np
import pickle
import sys
from xgboost import XGBRegressor

query_date = sys.argv[1]

def load_model(path):
    f = open(path, 'rb')
    clf = pickle.load(f)
    f.close()
    return(clf)

# load trained model
xgb = load_model('xgb.dat')

# run the model to estimate volume
daily_data_df = pd.read_csv("./" + query_date + "/daily_data.csv")
daily_data = daily_data_df.drop(['Id'], axis=1).values
daily_data_pred = xgb.predict(daily_data)
daily_data_pred[daily_data_pred<0]=0

# output volume estimates
network_pred = pd.DataFrame(np.concatenate((daily_data_df.values, daily_data_pred.reshape(-1, 1)), axis=1),
                              columns = daily_data_df.columns.append(pd.Index(['pred_volume'])))
network_pred.to_csv("./" + query_date + '/daily_data_pred.csv', index = False)