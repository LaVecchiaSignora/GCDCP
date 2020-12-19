library(data.table)
library(dplyr)
##download the data
dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(dataUrl,destfile="./dataset.zip",method="curl")

unzip("dataset.zip")

features <- read.table("features.txt", col.names = c("n","functions"))
activities <- read.table("activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("subject_test.txt", col.names = "subject")
x_test <- read.table("X_test.txt", col.names = features$functions)
y_test <- read.table("y_test.txt", col.names = "code")
subject_train <- read.table("subject_train.txt", col.names = "subject")
x_train <- read.table("X_train.txt", col.names = features$functions)
y_train <- read.table("y_train.txt", col.names = "code")

##Merges the training and the test sets to create one data set.
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged<- cbind(Subject, Y, X)

##Extracts only the measurements on the mean and 
## standard deviation for each measurement.
tidydata <- Merged %>% select(subject, code, contains("mean"), contains("std"))


##Uses descriptive activity names to name the activities in the data set
tidydata$code <- activities[tidydata$code, 2]


##Appropriately labels the data set with descriptive variable names. 
names(tidydata)[2] = "activity"
names(tidydata)<-gsub("^t", "Time", names(tidydata))
names(tidydata)<-gsub("^f", "Frequency", names(tidydata))
names(tidydata)<-gsub("Acc", "Accelerometer", names(tidydata))
names(tidydata)<-gsub("Gyro", "Gyroscope", names(tidydata))
names(tidydata)<-gsub("BodyBody", "Body", names(tidydata))
names(tidydata)<-gsub("Mag", "Magnitude", names(tidydata))


##From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.

FinalData <- tidydata %>%
  group_by("subject", "activity") %>%
  summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)

altdata<-aggregate(. ~subject + activity, tidydata, mean)
altdata<-altdata[order(altdata$subject,altdata$activity),]
write.table(altdata, file = "tidydata.txt",row.name=FALSE)


str(altdata)


library(codebook)

new_codebook_rmd()