# README.md

## Getting and Cleaning Data - Course Project

The R script called ```run_analysis.R``` does the following:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive activity names.
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.


## How to use the R script ```run_analysis.R```

1. Download the original data from http://bit.ly/1p1sdWC
2. Unzip it in your working directory (you'll then have a ```UCI HAR Dataset``` folder)
3. Put the script ```run_analysis.R``` in your working directory
4. Running ```source("run_analysis.R")``` will generate a new file in your working directory called ```final_dataset.txt```


## Dependencies

```run_analysis.R``` depends only on the ```plyr``` package will help you to install the dependencies automatically.