#Peer-graded Assignment: Getting and Cleaning Data Course Project

#Tasks to accomplish in this R script
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive variable names. 
# 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.


# Lets get the working directory first
getwd()
setwd(Users/lw2134/Desktop/ZKWDataSci)

#Loading libraries
library(tidyr)
library(dplyr)
library(downloader)

#Create directory for downloaded data
if (!file.exists("data")) {
  dir.create("data")
}

#Download data and unzip file in the created directory
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/AssignmentData.zip", method = "curl")
unzip(zipfile="./data/AssignmentData.zip",
      exdir="./data") 

#Lets start accomplishing the tasks
#Task 1. Merge the training and the test sets to create one data set.

#1.1 Lets start reading some files

# Reading feature vector:
features <- read.table('./data/UCI HAR Dataset/features.txt')

# Reading activity labels:
activityLabels <- read.table('./data/UCI HAR Dataset/activity_labels.txt')

# Reading trainings tables:
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

#1.2 Assign column names
colnames(activityLabels)<-c("activityId","activityType")
colnames(subject_train) <- "subId"
colnames(x_train) <- features[,2]
colnames(y_train) <- "activityId"

#1.3 Reading testing tables
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

#1.4 Assign column names
colnames(subject_test) <- "subId"
colnames(x_test) <- features[,2]
colnames(y_test) <- "activityId"

#1.5 Merge the training and the test sets to create one data set.
merged_train_data <- cbind(y_train, subject_train, x_train)
merged_test_data <- cbind(y_test, subject_test, x_test)
fullDataCombined <- rbind(merged_train_data, merged_test_data)

#1.6 Create a vector for all the data and name it fullDataCombined
colNames <- colnames(fullDataCombined);


# Task 2. Extract only the measurements on the mean and standard deviation for each measurement. 
mean_and_standard_deviation <-fullDataCombined[,grepl("mean|std|subId|activityId",colnames(fullDataCombined))]


# Task 3Use descriptive activity names to name the activities in the data set

#Lets clean up the variable names
names(mean_and_standard_deviation) <- gsub("\\(|\\)", "", names(mean_and_standard_deviation), perl  = TRUE)

#Lets correct the syntax
names(mean_and_standard_deviation) <- make.names(names(mean_and_standard_deviation))

#change descriptions to be clearer and clean up unwanted symbols
old.desc <- c("Acc", "^t", "^f", "BodyBody", "mean", "std", "Freq", "Mag")
new.desc <- c("Acceleration", "Time", "Frequency", "Body", "Mean", "Std", "Frequency", "Magnitude")

#Loop to replace old names to descriptive names
for(item in 1:length(old.desc)) {
  names(mean_and_standard_deviation) <- gsub(old.desc[item], new.desc[item], names(mean_and_standard_deviation))  
}

## Task 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
tidyData <- mean_and_standard_deviation %>%
  group_by(activityId, subId) %>%
  summarise_all(mean)

#5.1 Replace activityId with activityLabels - This will make activity names more descriptive
tidyData <- activityLabels %>%
  left_join(tidyData, by = "activityId")

#Remove activityId 
tidyData$activityId <- NULL

#Save final version of tidied data :)
saveRDS(tidyData, "tidyData.rds")




