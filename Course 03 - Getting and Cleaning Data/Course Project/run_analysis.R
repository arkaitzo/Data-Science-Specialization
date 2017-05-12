################################################################################
# This R script does the following:                                            #
#                                                                              #
# 1. Merges the training and the test sets to create one data set.             #
# 2. Extracts only the measurements on the mean and standard deviation for     #
#    each measurement.                                                         #
# 3. Uses descriptive activity names to name the activities in the data set    #
# 4. Appropriately labels the data set with descriptive variable names.        #
# 5. From the data set in step 4, creates a second, independent tidy data set  #
#    with the average of each variable for each activity and each subject.     #
################################################################################
#                                                                              #
# Assuming Samsung data is in your working directory.                          #
#                                                                              #
################################################################################

# Clear the working directory first
rm(list = ls())

# Download and unzip the data (uncomment if necessary)
# if(!file.exists("./data")) {dir.create("./data")}
# download.file(
#     "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
#     destfile = "./UCI-HAR-Dataset.zip", method = "curl")
# dateDownloaded <- date()
# unzip(zipfile = "./UCI-HAR-Dataset.zip", exdir = "./")

cat("Running the script. Please wait...")

# Identifiers of the subject who carried out the experiment
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subject <- rbind(subject_test, subject_train)
subject <- as.data.frame(as.factor(subject[, 1]))
rm(subject_test, subject_train)

# 561-feature vector with time and frequency domain variables
x_test <- read.table(file = "./UCI HAR Dataset/test/X_test.txt")
x_train <- read.table(file = "./UCI HAR Dataset/train/X_train.txt")
x <- rbind(x_test, x_train)
rm(x_test, x_train)

# Outcome variable (activities by id)
y_test <- read.table(file = "./UCI HAR Dataset/test/y_test.txt")
y_train <- read.table(file = "./UCI HAR Dataset/train/y_train.txt")
y <- rbind(y_test, y_train)
y <- as.data.frame(as.factor(y[, 1]))
rm(y_test, y_train)

# Descriptive activity names to name the activities in the data set
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt",
                              col.names = c("activityId", "activity"))
activity_labels$activityId <- as.factor(activity_labels$activityId)



# Descriptive variable names...
names(subject) <- "subject"
names(y) <- "activityId"
feature_names <- read.table("./UCI HAR Dataset/features.txt",
                            colClasses = "character", stringsAsFactors = TRUE)[, 2]
features <- grepl("mean|std", feature_names)
names(x) <- feature_names
x <- x[, features] # Only mean and standard deviation
rm(feature_names, features)



# Merge the test and the training sets to create one data set
tempData <- cbind(subject, x, y)
rm(subject, x, y)



# Apply descriptive activity names to name the activities in the data set
if (!require("plyr")) {install.packages("plyr")}; library(plyr)
data <- arrange(join(tempData, activity_labels), subject)
data$activityId <- NULL
rm(tempData, activity_labels)



# Data set with the average of each variable for each activity and each subject
finalData <- aggregate(. ~ subject + activity, data = data, FUN = "mean")
write.table(x = finalData, file = "./final_dataset.txt", row.name = FALSE)

cat("Finished!")
