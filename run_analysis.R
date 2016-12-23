##create one R script called run_analysis.R that does the following:
## 1)Merges the training and the test sets to create one data set
## 2)Extracts only the measurements on the mean and standard deviation for each measurement
## 3)Uses descriptive activity names to name the activities in the data set
## 4)Appropriately labels the data set with descriptive variable names
## 5)From the data set in step 4, creates a second, independent tidy data set with the 
##    average of each variable for each activity and each subject



##Set working directory
setwd("C:/Users/Timothy.Knox/Desktop/Coursera/Getting & Cleaning Data/Course Project/UCI HAR Dataset")

##Read in files
features <- read.table('./features.txt', header = FALSE)
activity_label <- read.table('./activity_labels.txt', header = FALSE)
subjectTest <- read.table('./test/subject_test.txt', header = FALSE)
xTest <- read.table('./test/X_test.txt', header = FALSE)
yTest <- read.table('./test/y_test.txt', header = FALSE)
subjectTrain <- read.table('./train/subject_train.txt', header = FALSE)
xTrain <- read.table('./train/X_train.txt', header = FALSE)
yTrain <- read.table('./train/y_train.txt', header = FALSE)

##Apply column names to the above data files
colnames(activity_label) <- c('activityID', 'activityType')
colnames(subjectTrain) <- "subjectID"
colnames(xTrain) <- features[,2]
colnames(yTrain) <- "activityID"
colnames(subjectTest) <- "subjectID"
colnames(xTest) <- features[,2]
colnames(yTest) <- "activityID"

## Merge Training and Test data
training_data <- cbind(yTrain, subjectTrain, xTrain)
test_data <- cbind(yTest, subjectTest, xTest)
final_data <- rbind(training_data, test_data)

## Extract only columns with mean and standard deviation variables
colNames <- colnames(final_data)
mean_sd_columns <- (grepl("activityID", colNames) | 
                      grepl("subjectID", colNames) | 
                      grepl("mean..", colNames) | 
                      grepl("std..", colNames))
final_data <- final_data[, mean_sd_columns == TRUE]

## Use descriptive activity names to name the activities in the data set
final_data <- merge(activity_label, final_data, by = 'activityID', all.x = TRUE)

## Appropriately label data set with descriptive variable names
colNames <- colnames(final_data)
for (i in 1:length(colNames)) {
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

colnames(final_data) <- colNames

## Create second tidy data set with the average of each variable for each activity and each subject
library(plyr) ## Need plyr package
tidy_data <- aggregate(. ~ subjectID + activityID, final_data, mean)
tidy_data <- tidy_data[order(tidy_data$subjectID, tidy_data$activityType.x)]

##Export tidy data set into txt file
write.table(tidy_data, './tidyData.txt', row.names = FALSE, sep = '\t')












