# Load the Library
library(data.table)
library(dplyr)

featureLabels <- read.table("./UCI HAR Dataset/features.txt")
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)

# x<- features data; y<- activities data

subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
yTrain <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
xTrain <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)

subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
yTest <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
xTest <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)

# 1.Merges the training and the test sets to create one data set.

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(yTrain, yTest)
features <- rbind(xTrain, xTest)

colnames(features) <- t(featureLabels[2])

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
CombinedData <- cbind(features,activity,subject)

# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 

MeanSTD <- grep(".*Mean.*|.*Std.*", names(CombinedData), ignore.case=TRUE)

DataMatrix <- c(MeanSTD, 562, 563)
dim(CombinedData)

YieldData <- CombinedData[,DataMatrix]
dim(YieldData)

# 3. Uses descriptive activity names to name the activities in the data set

YieldData$Activity <- as.character(YieldData$Activity)
for (i in 1:6){ YieldData$Activity[YieldData$Activity == i] <- as.character(activityLabels[i,2])
  }
YieldData$Activity <- as.factor(YieldData$Activity)

# 4. Appropriately labels the data set with descriptive variable names

names(YieldData)

names(YieldData)<-gsub("Acc", "Accelerometer", names(YieldData))
names(YieldData)<-gsub("Gyro", "Gyroscope", names(YieldData))
names(YieldData)<-gsub("BodyBody", "Body", names(YieldData))
names(YieldData)<-gsub("Mag", "Magnitude", names(YieldData))
names(YieldData)<-gsub("^t", "Time", names(YieldData))
names(YieldData)<-gsub("^f", "Frequency", names(YieldData))
names(YieldData)<-gsub("tBody", "TimeBody", names(YieldData))
names(YieldData)<-gsub("-mean()", "Mean", names(YieldData), ignore.case = TRUE)
names(YieldData)<-gsub("-std()", "STD", names(YieldData), ignore.case = TRUE)
names(YieldData)<-gsub("-freq()", "Frequency", names(YieldData), ignore.case = TRUE)
names(YieldData)<-gsub("angle", "Angle", names(YieldData))
names(YieldData)<-gsub("gravity", "Gravity", names(YieldData))

names(YieldData)

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

YieldData$Subject <- as.factor(YieldData$Subject)
YieldData<- data.table(YieldData)

tidyData <- aggregate(. ~Subject + Activity, YieldData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
