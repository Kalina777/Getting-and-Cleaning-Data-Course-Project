# Getting and Cleaning Data Course Project
#Olga Shirokova
#11.11.2017

library(data.table)
library(dplyr)

#Set Working Directory

setwd("C:/Users/Olgas/Documents/Coursera/Download and clean data/UCI HAR Dataset/")

# Reading supporting data.
featureNames <- read.table("./features.txt")
activityLabels <- read.table("./activity_labels.txt", header = FALSE)

# Reading training data.
subjectTrain<-read.table("./train/subject_train.txt", header=FALSE)
xTrain<- read.table("./train/X_train.txt", header=FALSE)
yTrain<- read.table("./train/y_train.txt", header=FALSE)

# Reading test data.
subjectTest<-read.table("./test/subject_test.txt", header=FALSE)
xTest<- read.table("./test/X_test.txt", header=FALSE)
yTest<- read.table("./test/y_test.txt", header=FALSE)


###1. Merge the training and the test sets to create one data set

#Combining the data
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(yTrain, yTest)
features <- rbind(xTrain, xTest)

#Labeling columns
colnames(features) <- t(featureNames[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"

#Merging teh data
completeData <- cbind(features,activity,subject)


###2. Extract only the measurements on the mean and standard deviation for each measurement.

#Get columns with mean or std in them
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

requiredColumns <- c(columnsWithMeanSTD, 562, 563)
extractedData <- completeData[,requiredColumns]


###3. Use descriptive activity names to name the activities in the data set
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
     extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

extractedData$Activity <- as.factor(extractedData$Activity)



###4. Appropriately label the data set with descriptive variable names
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))


###5. Create a second, independent tidy data set with the average of each variable for each activity and each subject

#set Subject as a factot variable
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

#create tidyData
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
