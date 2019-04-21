import psycopg2
import boto
import os

###SET PATH WHERE PROCESSED DATA EXISTS (DATA RUN THROUGH run_data_processing.py)
path = ''
###

###Set aws credentials:
aws_access_key_id = ''
aws_secret_access_key = ''
s3_bucket = '' #this is the S3 bucket to store data in
###

###Connect to data warehouse
data_warehouse = psycopg2.connect() #connect to data warehouse
cur = data_warehouse.cursor()
###

os.chdir(path)

#get file names
file_names = [] #empty array for filenames

#append all file names within folders into list
for (dirpath, dirnames, filenames) in os.walk(path):
	file_names.extend(filenames)
	break

file_names.remove('.DS_Store')

#s3 configuratio
s3_connect = boto.connect_s3() #connect to s3
bucket = s3_connect.get_bucket(s3_bucket) #access bucket
from boto.s3.key import Key 

#define the schema and file names
schema_names = []
for i in file_names:
	print(i)
	name_list = i.split("_")
	del name_list[0:2]
	#del name_list[(len(name_list)-1)]
	table_name = '_'.join(name_list)
	data_path = path + '/' + i #create data path fr fetching data
	k = Key(bucket) #create new key within bucket
	k.key = table_name #assign name to key
	k.set_contents_from_filename(data_path) #set the content

	##Replace brackets with values... the s3 directory, the aws access key, the aws secret within this thread
	s3_command = ("copy test." + table_name + " from 's3://" + s3_bucket  +"/" + table_name + "' " + 
	"credentials 'aws_access_key_id=" aws_access_key_id + ";aws_secret_access_key=" + aws_secret_access_key + 
	"' removequotes NULL 'nan' delimiter ',' ignoreheader as 1;")
	
	cur.execute(s3_command) #push the data to the relevant data in Redshift
	data_warehouse.commit() #commit the push
	print('done with' + i)
###

###Close data warehouse connection
data_warehouse.close()
###