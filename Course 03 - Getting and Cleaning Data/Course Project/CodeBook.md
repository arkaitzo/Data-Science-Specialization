# CodeBook.md

This code book describes the variables, the data, and any transformations or work that I performed to clean up the data.


## The data source

* Data for the project: http://bit.ly/1p1sdWC
* Full description: http://bit.ly/1mEvWTG


## Data Set Information (from the source)

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.


## The data

The dataset was created using the following files:

- 'features.txt': List of all features.
- 'activity_labels.txt': Links the class labels with their activity name.
- 'X_test.txt': Test set.
- 'y_test.txt': Test labels.
- 'X_train.txt': Training set.
- 'y_train.txt': Training labels.


## ```run_analysis.R``` does the following:

* Loads all the data required (subject IDs, test and training data sets (including the outcome variable and feature and activity labels).
* From the test and training data set, extracts ONLY the measurements on the mean and standard deviation.
* Labels the data set with descriptive activity names.
* Merges the test and the training sets to create one data set.
* Applies descriptive activity names to name the activities in the data set.
* Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.
