library(data.table)
library(dplyr)

#Part 1 - Merges the training and the test sets to create one data set.
featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

#Training data
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

#Test data
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)


#Combining training and test data
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

#Column naming
colnames(features) <- t(featureNames[2])

#Merging data together
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
totalData <- cbind(features,activity,subject)


#Part 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
#Identify and extract columns with mean or std in them
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(totalData), ignore.case=TRUE)

#Create activity and subject columns
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(totalData)

#Create extracted data
finalData <- totalData[,requiredColumns]
dim(finalData)


#Part 3 - Uses descriptive activity names to name the activities in the data set
#Change numeric to character names or labels
finalData$Activity <- as.character(finalData$Activity)
for (i in 1:6){
  finalData$Activity[finalData$Activity == i] <- as.character(activityLabels[i,2])
}

#Factor "activity" variable
finalData$Activity <- as.factor(finalData$Activity)


#Part 4 - Appropriately labels the data set with descriptive variable names. 
#Changing labels to full names
names(finalData)<-gsub("Acc", "Accelerometer", names(finalData))
names(finalData)<-gsub("Gyro", "Gyroscope", names(finalData))
names(finalData)<-gsub("BodyBody", "Body", names(finalData))
names(finalData)<-gsub("Mag", "Magnitude", names(finalData))
names(finalData)<-gsub("^t", "Time", names(finalData))
names(finalData)<-gsub("^f", "Frequency", names(finalData))
names(finalData)<-gsub("tBody", "TimeBody", names(finalData))
names(finalData)<-gsub("-mean()", "Mean", names(finalData), ignore.case = TRUE)
names(finalData)<-gsub("-std()", "STD", names(finalData), ignore.case = TRUE)
names(finalData)<-gsub("-freq()", "Frequency", names(finalData), ignore.case = TRUE)
names(finalData)<-gsub("angle", "Angle", names(finalData))
names(finalData)<-gsub("gravity", "Gravity", names(finalData))


#Part 5 - From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
#Set "Subject" as variable
finalData$Subject <- as.factor(finalData$Subject)
finalData <- data.table(finalData)

#Create "tidyData" set
tidyDataset <- aggregate(. ~Subject + Activity, finalData, mean)
tidyDataset <- tidyDataset[order(tidyDataset$Subject,tidyDataset$Activity),]
write.table(tidyDataset, file = "Tidy Dataset.txt", row.names = FALSE)




