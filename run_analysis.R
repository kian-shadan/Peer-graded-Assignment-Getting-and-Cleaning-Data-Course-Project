# First we have to download the file
> if(!file.exists("./data")){dir.create("./data")}
> fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
> if(!file.exists("./data")){dir.create("./data")}
> fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
> download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

# Then the zipfile must unzip 
unzip(zipfile="./data/Dataset.zip",exdir="./data")
# now unziped files are in the foder " UCI HAR Dataset" Get the list of the files

> unzip(zipfile="./data/Dataset.zip",exdir="./data")
> path_rf <- file.path("./data" , "UCI HAR Dataset")
> files<-list.files(path_rf, recursive=TRUE)
> files

# read data from the targeted data
# the supporting metadata in this data are the name of the features and the name of the activities. They are loaded into variables featureNames and activityLabels.

featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

# now we have to format training and test data sets.
# first read training data set.

subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

# second read test data sets.

subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

# So, now we can combine the data in training and test data sets, that we stored in subjet, activity, and features.

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

# Naming the columns
# the colunms in the features data set can be named featureNames.

colnames(features) <- t(featureNames[2])

# Merge the data
# As we stored the data in "features, activity, and subjet". we merge in "completeDate"

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

# Extracts only the measurements on the mean and standard deviation for each measurement.
# extract the column indices that have either mean or std in them.

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

Add activity and subject columns to the list and look at the dimension of completeData

requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)
## [1] 10299   563
We create extractedData with the selected columns in requiredColumns. And again, we look at the dimension of requiredColumns.

extractedData <- completeData[,requiredColumns]
dim(extractedData)
## [1] 10299    88

Uses descriptive activity names to name the activities in the data set()

The activity field in extractedData is originally of numeric type. We need to change its type to character so that it can accept activity names. The activity names are taken from metadata activityLabels.

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
We need to factor the activity variable, once the activity names are updated.

extractedData$Activity <- as.factor(extractedData$Activity)

# Appropriately labels the data set with descriptive variable names
# these are the variables in "extractedData"

names(extractedData)

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

# these are the variable names in extractedData after they are edited

names(extractedData)


# finally, From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

# We create tidyData as a data set with average for each activity and subject. Then, we order the enties in tidyData and write it into data file Tidy.txt that contains the processed data.

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)


