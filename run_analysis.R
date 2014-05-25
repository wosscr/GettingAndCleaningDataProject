# load required libraries
library("plyr")
library("reshape2")


# load training set
trainingSetData <- read.table("./data/UCI HAR Dataset/train/X_train.txt")

# load test set
testSetData <- read.table("./data/UCI HAR Dataset/test/X_test.txt")

# merge data sets
mergedData <- rbind(trainingSetData, testSetData)

# load label ids for the training set
trainingActivityIds <- read.table("./data/UCI HAR Dataset/train/Y_train.txt")
names(trainingActivityIds) <- c("activity")

# label label ids for the test set
testActivityIds <- read.table("./data/UCI HAR Dataset/test/Y_test.txt")
names(testActivityIds) <- c("activity")

# append sets of activities
activities <- rbind(trainingActivityIds, testActivityIds)

# load the subjects for the training set
trainingSubjects <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
names(trainingSubjects) <- c("subject")

# load the subjects for the test set
testSubjects <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
names(testSubjects) <- c("subject")

# append sets of subjects
subjects <- rbind(trainingSubjects, testSubjects)

# append subject and activities data to the merged data
mergedData <- cbind(subject = subjects$subject, mergedData)
mergedData <- cbind(activity = activities$activity, mergedData)

# load the feature names
featureLabels <- read.table("./data/UCI HAR Dataset/features.txt")
names(featureLabels) <- c("id", "feature")
featureLabels <- rbind(data.frame(id= 0, feature = "subject"), featureLabels)
featureLabels <- rbind(data.frame(id= 0, feature = "activity"), featureLabels)

# update the names of the data set with the featuer names
names(mergedData) <- featureLabels$feature

# load the activity names
activityLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
activityLabels[ , "V2"] <- tolower(activityLabels$V2)
activityLabels[ , "V2"] <- sub("_", " ", activityLabels$V2)
names(activityLabels) <- c("activity", "activitynames")

activityNames <- join(activities, activityLabels, by = "activity")

# append activity names to the data set
mergedData <- cbind(activityNames, mergedData)

# create the list of variables that need to be extracted
meanFeatures <- featureLabels[grepl("mean()",featureLabels$feature), ]
meanFeatures <- meanFeatures[!grepl("meanFreq()",meanFeatures$feature), ]
stdFeatures <- featureLabels[grepl("std()",featureLabels$feature), ]
selectedFeaturesNames <- merge(meanFeatures, stdFeatures, all = TRUE)
selectedFeaturesNames <- rbind(data.frame(id = 0, feature = "subject"), selectedFeaturesNames)
selectedFeaturesNames <- rbind(data.frame(id = 0, feature = "activitynames"), selectedFeaturesNames)

# createa a data frame with the extrated variables
selectedFeatures <- mergedData[, which(names(mergedData) %in% selectedFeaturesNames$feature)]

# reshape the data based on activity names and subject
shapedData <- melt(selectedFeatures, id.vars = .(activitynames, subject))

# get the medians per subject and activity
shapedDataMedians <- dcast(shapedData, activitynames + subject ~ variable, mean)

# write the result in a file as result
write.table(shapedDataMedians, file = "./data/tidyData.txt")