# 0. Preparatory steps

# Load required packages
library(dplyr)
library(data.table)

#Download file if it does not exist already
file_name <- "Coursera_DS3_Final.zip"
if (!file.exists(file_name)){
        file_URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(file_URL, file_name, method="curl")
}  

#Unzip file if needed
if (!file.exists("UCI HAR Dataset")) { 
        unzip(file_name) 
}

#Read features of data 
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
#Read activity labels
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

#Read the data and assign to variables
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")


# 1. Merge the training and the test sets to create one data set.

x_merge <- rbind(x_train, x_test)
y_merge <- rbind(y_train, y_test)
subj_merge <- rbind(subject_train, subject_test)
Data_merged <- cbind(subj_merge, y_merge, x_merge)

# 2. Extract only the measurements on the mean and standard deviation for each measurement.

Data_selected<- Data_merged %>% select(subject, code, contains("mean"), contains("std"))

# 3. Use descriptive activity names to name the activities in the data set.

#Add the labels from the activities file to the merged, selected data set. 

Data_selected$code <- activities[Data_selected$code, 2]

# 4. Appropriately label the data set with descriptive variable names.

#Remove typos, special characters and abbreviations from activity names
names(Data_selected)[2] = "activity"
names(Data_selected)<-gsub("Acc", "Accelerometer", names(Data_selected))
names(Data_selected)<-gsub("Gyro", "Gyroscope", names(Data_selected))
names(Data_selected)<-gsub("BodyBody", "Body", names(Data_selected))
names(Data_selected)<-gsub("Mag", "Magnitude", names(Data_selected))
names(Data_selected)<-gsub("^t", "Time", names(Data_selected))
names(Data_selected)<-gsub("^f", "Frequency", names(Data_selected))
names(Data_selected)<-gsub("tBody", "TimeBody", names(Data_selected))
names(Data_selected)<-gsub("-mean()", "Mean", names(Data_selected), ignore.case = TRUE)
names(Data_selected)<-gsub("-std()", "STD", names(Data_selected), ignore.case = TRUE)
names(Data_selected)<-gsub("-freq()", "Frequency", names(Data_selected), ignore.case = TRUE)
names(Data_selected)<-gsub("angle", "Angle", names(Data_selected))
names(Data_selected)<-gsub("gravity", "Gravity", names(Data_selected))

# 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

#Create a finalized data set with the mean for each activity and subject

Finalized<- Data_selected %>%
        group_by(subject, activity) %>%
        summarise_all(funs(mean))

#Save this finalized data as a new data table for submission.

write.table(Finalized, "tidy_data.txt", row.name=FALSE)