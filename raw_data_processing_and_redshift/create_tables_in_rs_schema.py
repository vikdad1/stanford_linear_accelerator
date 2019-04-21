import psycopg2
import boto #AWS S3 interface
import os 

###SET PATH WHERE PROCESSED DATA EXISTS (DATA RUN THROUGH run_data_processing.py)
path = ''
###

###Connect to data warehouse --- Add Credentials here
data_warehouse = psycopg2.connect(database=, user=, password=, ,
		 host=, port=) #connect to data warehouse
cur = data_warehouse.cursor()
###

#queries for creating data tables
drop_table_query = 'drop table if exists test.%s'

schema_query = 'create table test.%s (secondsPastEpoch numeric, value float8, nanoseconds numeric, severity bigint, \
status bigint, timestamp timestamp, klystron varchar(256));' 

#working directory
os.chdir()
path = os.getcwd() #get working directory

#get file names
file_names = [] #empty array for filenames

#append all file names within folders into list
for (dirpath, dirnames, filenames) in os.walk(path):
	file_names.extend(filenames)
	break

#file_names.remove('.DS_Store')
#create the schemas on warehoue
schema_names = []
for i in file_names: #for each file name 

	print(i)

	name_list = i.split("_") #split
	del name_list[0:2] #find the 

	final = '_'.join(name_list)
	#drop the table if exists, then create the table
	
	file_drop_query = drop_table_query %final 
	file_query = schema_query %final
	cur.execute(file_drop_query)
	cur.execute(file_query)
	data_warehouse.commit()

###Close data warehouse connection
data_warehouse.close()
##