import pandas as pd
import numpy as np

def processKlystronData(data):
    
    #This function processes raw data per klystron from epics network
    
    #Extract the column labels from the data
    column_labels = data[data.iloc[:,0].str.contains('labels')]
    column_labels = column_labels.iloc[0].str.rpartition('[').iloc[0,2]
    column_labels = column_labels.replace(']', '')
    column_labels = column_labels.split(',')
    
    #drop the column row
    new_data = data[data.iloc[:,0].str.contains('labels') == False]
    
    #extract each column
    data_list = []
    for i in column_labels:
        target_data = new_data[new_data.iloc[:,0].str.contains(i)]
        target_data = target_data.iloc[0].str.rpartition('[').iloc[0,2]
        target_data = target_data.replace(']', '')
        target_data = target_data.split(',')
        data_list.append(target_data)
    
    #convert from list -> data frame
    data_list = np.column_stack(data_list)
    final = pd.DataFrame(data_list, columns = column_labels)
        
    return(final)