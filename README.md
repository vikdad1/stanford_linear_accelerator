# slac-capstone

In this folder, you'll find work related to my Master's Capstone project done in collaboration with
Stanford Linear Accelerator. The goal of the project was to predict hardware failures within the
linac (linear accelerator).

For an overview of the proect, find the Executive Summary. For a deep dive into the technical work I did,
find Chapter 1. For an analysis of the industry and the business/social of this project, find Chapter 2.

You can also find all the code related to my technical work. Don't expect this code to work out-of-box - it won't!
Much of data processing relies on an AWS Redshift instance I set up which can't be accessed by the public. Also,
many of scripts are still pointing to my local directories and weren't designed to be completely reusable. If you're
interested in getting a deep understanding of how the analysis was done, so you can reproduce it, then this code should
do the trick.

Where to find what:
1. raw_data_processing_and_redshift: How raw data was processed from SLAC and put into AWS Redshift.
2. feature_engineering_scripts: How response/feature variables were created.
3. feature_engineering_functions: The functions used by the scripts.
4. ml_training_scipts: Scripts for training classification algorithms.
5. jitter_data_set: Data used be scripts, from raw data to processed data.