
################################################### lOAD LIBRARIES ###############################

library(dplyr)

################################################## GETTING DATA ##################################

# download zip file containing data if it hasn't already been downloaded
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "zipped Dataset.zip"

if (!file.exists(zipFile)) {
        download.file(zipUrl, zipFile, mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
        unzip(zipFile)
}

################################################# READING DATA ###################################

trainSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

dim(trainActivity)
dim(trainSubjects)
dim(trainValues)


testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

dim(testSubjects)
dim(testActivity)
dim(testValues)

features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

dim(features)
head(features)

activities <- read.table(file.path(dataPath, "activity_labels.txt"))
dim(activities)
activities

colnames(activities) <- c("activityId", "activityLabel")
activities

################################################# MERGING THE DATA ##############################


DataActivity <- rbind(
        cbind(trainSubjects, trainValues, trainActivity),
        cbind(testSubjects, testValues, testActivity)
        )

# assign column names
colnames(DataActivity) <- c("subject", features[, 2], "activity")

###################### Extract only the measurements on the mean and standard deviation #########
#################################        for each measurement             #######################

measurments <- grepl("subject|activity|mean|std", colnames(DataActivity))

DataActivity <- DataActivity[, measurments]

##########################  name the activities in the data set #################################


# replace activity values with named factor levels
DataActivity$activity <- factor(DataActivity$activity,levels = activities[, 1], labels = activities[, 2])

########################## label the data set with descriptive variable names ###################

DataActivityCols <- colnames(DataActivity)

# remove special characters
DataActivityCols <- gsub("[\\(\\)-]", "", DataActivityCols)

# expand abbreviations and clean up names
DataActivityCols <- gsub("^f", "frequencyDomain", DataActivityCols)
DataActivityCols <- gsub("^t", "timeDomain", DataActivityCols)
DataActivityCols <- gsub("Acc", "Accelerometer", DataActivityCols)
DataActivityCols <- gsub("Gyro", "Gyroscope", DataActivityCols)
DataActivityCols <- gsub("Mag", "Magnitude", DataActivityCols)
DataActivityCols <- gsub("Freq", "Frequency", DataActivityCols)
DataActivityCols <- gsub("mean", "Mean", DataActivityCols)
DataActivityCols <- gsub("std", "StandardDeviation", DataActivityCols)
DataActivityCols <- gsub("BodyBody", "Body", DataActivityCols)

colnames(DataActivity) <- DataActivityCols


####### tidy set with the average of each variable for each activity and each subject ##########

DataActivityAverage <- DataActivity %>% 
        group_by(subject, activity) %>%
        summarise_all(funs(mean))

# output to file "tidy_data.txt"
write.table(DataActivityAverage, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)



