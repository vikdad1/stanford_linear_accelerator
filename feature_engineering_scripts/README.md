##Feature Engineering Scripts

Within this folder, you'll find scripts used to create the response variables and features used for algorithm training.

A summary of scripts:

1. 'jitter_events_data_generation': This was the first script that was run. What it does is pull phase jitter data from 
the data warehouse on a monthly cadence and creates 'jitter event' and non-jitter event data (the response variable).

2. 'jitter_feature_generation_script': This script uses the outputs of the 'jitter_events_data_generation.R' to create
feature variables that will be used to predict the response.

3. 'jitter_event_reduction_script'L This script applies logic to the 'jitter_events_data_generation.R' outputs to correct
for class imbalance within the response viriable.

4. 'ml_data_sets_preprocessing.R': This script processes the outputs of the first three scripts, handles missing values,
and outputs a data set that is close to ready for algorithm development.