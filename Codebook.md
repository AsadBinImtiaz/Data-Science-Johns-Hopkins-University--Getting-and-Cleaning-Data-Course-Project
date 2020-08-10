# Cleaning and Getting Data course project code book

## Files
The projexct uses following 6 files

* `x_train.txt`         : Training feature dataset
* `y_train.txt`         : Training label dataset
* `subject_train.txt`   : Training Subsjects (Subject IDs) 
* `x_test.txt`          : Test feature dataset
* `y_test.txt`          : Test label dataset
* `subject_test.txt`    : Test Subsjects (Subject IDs)

Additionally following files were used

* `activity_labels.txt` : A mapping of activity label and description
* `features.txt`        : A mapping of feature ID and name

## Process

The script `run_analysis.R` is the primary script for the entire data pipeline
Followng steps are perdormed

* 01 - The data directory is searched on the local system. If it does not exists, the dataset archive files is searched. If this is also missing dataset is downloaded from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
* 02 - If the dataset archive is found or downloaded, it is unzupped in data directory, overwriting anything already existing
* 03 - First the `features.txt` is read to get all feature names
* 04 - The important features which are mean and standard-deviation of all measures are are collected
* 05 - The feature names are cleansed. All brackets are removed and dots and commas are converted to underscores
* 06 - Next all training features `x_train.txt` are read and assigned names from step 05
* 07 - All training labels are read from `y_train.txt`
* 08 - All training subjects are read from `subject_train.txt`
* 09 - The above 3 data tables are combined into one data table for Training-Set
* 10 - Next all testing features `x_test.txt` are read and assigned names from step 05
* 11 - All test labels are read from `y_test.txt`
* 12 - All test subjects are read from `subject_test.txt`
* 13 - The above 3 data tables are combined into one data table for Test-set
* 14 - Traing and test sets from step 09 and 13 are combined into one big set
* 15 - Only important labels from step 04 are retained
* 16 - All activity labels are read from `activity_labels.txt`
* 17 - The labes in merged dataset are replaced with descriptions from step 16
* 18 - Tidy data table is persisted to disk
* 19 - A summary data table with mean values per subject and activity is calculated
* 20 - the summary data table is persisted to disk

## Measures in tidy data

* tbodyacc_mean_x
* tbodyacc_mean_y
* tbodyacc_mean_z
* tbodyacc_std_x
* tbodyacc_std_y
* tbodyacc_std_z
* tgravityacc_mean_x
* tgravityacc_mean_y
* tgravityacc_mean_z
* tgravityacc_std_x
* tgravityacc_std_y
* tgravityacc_std_z
* tbodyaccjerk_mean_x
* tbodyaccjerk_mean_y
* tbodyaccjerk_mean_z
* tbodyaccjerk_std_x
* tbodyaccjerk_std_y
* tbodyaccjerk_std_z
* tbodygyro_mean_x
* tbodygyro_mean_y
* tbodygyro_mean_z
* tbodygyro_std_x
* tbodygyro_std_y
* tbodygyro_std_z
* tbodygyrojerk_mean_x
* tbodygyrojerk_mean_y
* tbodygyrojerk_mean_z
* tbodygyrojerk_std_x
* tbodygyrojerk_std_y
* tbodygyrojerk_std_z
* tbodyaccmag_mean
* tbodyaccmag_std
* tgravityaccmag_mean
* tgravityaccmag_std
* tbodyaccjerkmag_mean
* tbodyaccjerkmag_std
* tbodygyromag_mean
* tbodygyromag_std
* tbodygyrojerkmag_mean
* tbodygyrojerkmag_std
* fbodyacc_mean_x
* fbodyacc_mean_y
* fbodyacc_mean_z
* fbodyacc_std_x
* fbodyacc_std_y
* fbodyacc_std_z
* fbodyaccjerk_mean_x
* fbodyaccjerk_mean_y
* fbodyaccjerk_mean_z
* fbodyaccjerk_std_x
* fbodyaccjerk_std_y
* fbodyaccjerk_std_z
* fbodygyro_mean_x
* fbodygyro_mean_y
* fbodygyro_mean_z
* fbodygyro_std_x
* fbodygyro_std_y
* fbodygyro_std_z
* fbodyaccmag_mean
* fbodyaccmag_std
* fbodybodyaccjerkmag_mean
* fbodybodyaccjerkmag_std
* fbodybodygyromag_mean
* fbodybodygyromag_std
* fbodybodygyrojerkmag_mean
* fbodybodygyrojerkmag_std



