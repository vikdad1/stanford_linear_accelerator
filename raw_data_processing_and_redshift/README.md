## instructions for processing raw data

The raw data for this project can be downloaded at this link: https://drive.google.com/file/d/1ZGobbbDGSPbogb4PNmzdUYiMSuB5hFXq/view?usp=sharing

Within the 'raw_slac_data' folder, there are folders that follow this structure: RF Station + Attribute,
where rf station example is 'li22_31' and attribute example is 'focus_i', resulting in a folder 'li22_31_focus_i'.

### raw_data_processing.py
To process this raw data, use the raw_data_processing.py script. Within the script, modify the read_path and write_path values,
where read_path is where the raw data is ('raw_slac_data') and write_path is where processed data should be stored.

If you want to extract new raw data from the EPICs network, store the data in the same folder structure/naming convention and
the script will work as expected.

### create_tables_in_rs_schema.py / load_data_into_rs.tables.py
For this project, processed data was stored on AWS Redshift. the 'create_tables_in_rs_schema.py' script reads the file names
from the processed data and creates tables in Redshift named after the processed data files. The 'load_data_into_rs_tables.py'
actually loads the data into the tables created.

To use Redshift, an AWS Redshift instance will need to be created. An easier path maybe to simply use the raw data files.
