import os
from datetime import datetime, timezone
import pandas as pd
import numpy as np

from functions.processKlystronData import processKlystronData
from functions.processEpoch import processEpoch

####SET DIRECTORY TO WHERE 'raw_cater_data' folder
read_path = ''
####

####SET DIRECTORY TO WHERE PROCESSED DATA SHOULD BE SAVED
write_path = ''
####

os.chdir(read_path)
folder_names = os.listdir() #find folders in the directory
folder_names.remove('.DS_Store') #don't know what this is.

for i in folder_names:
    file_names = [] #empty array for filenames
    data_list = [] #empty list for data storage
    
    print(i)

    #folder name processing
    folder_split = i.split("_")
    klystron = folder_split[0] + '_' + folder_split[1] #assumes the klystron format is corect

    #append all file names within folders into list
    for (dirpath, dirnames, filenames) in os.walk(read_path + '/' + i + '/'):
        file_names.extend(filenames)
        break
    
    #read + process in data
    for j in file_names:

        print(j)
        raw_data = pd.read_table(read_path + '/' + i + '/' + j)
        clean_data = processKlystronData(raw_data) #turn it into data frame
        clean_data = processEpoch(clean_data) #process timestamp
        
        #add klystron value
        clean_data = pd.concat([clean_data, 
                                pd.Series(np.repeat(klystron, len(clean_data)))], axis=1)
        clean_data.columns.values[6] = 'klystron'
        
        data_list.append(clean_data) #return data
        final_data = pd.concat(data_list, axis=0)
    
        final_data.to_csv(write_path + i, index=False)