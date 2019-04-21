import pandas as pd
import numpy as np

def processEpoch(data):
    
    #Convert epoch time to timestamp

    epoch_time = pd.to_numeric(data['secondsPastEpoch'])
    date_time = epoch_time.astype("datetime64[s]")
    final = pd.concat([data, epoch_time.astype("datetime64[s]")], axis=1)
    final.columns.values[5] = 'timestamp'

    return(final)