## run_analysis.R

## This script assumes that the zip file was uncompressed in the data directory and that the folder name hasn't changed

# necessary libraries
library("plyr")
library("reshape2")

######## load the data into R ######## 

#label text, it works for both training and testing sets
activityLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
activityLabels[ , "V2"] <- tolower(activityLabels$V2)
activityLabels[ , "V2"] <- sub("_", " ", activityLabels$V2)

featureLabels <- read.table("./data/UCI HAR Dataset/features.txt")
names(featureLabels) <- c("id", "feature")
featureLabels <- rbind(data.frame(id= 0, feature = "subject"), featureLabels)
featureLabels <- rbind(data.frame(id= 0, feature = "activity"), featureLabels)

##### Training Set ##### 

### labels ###
# label id for training set
trainingActivityIds <- read.table("./data/UCI HAR Dataset/train/Y_train.txt")
# merge label names with the ids
trainingActivity <- merge(activityLabels, trainingActivityIds, by = "V1", all = TRUE)
# rename columns
names(trainingActivity) <- c("id", "activity")

### subjects ###
# training subjects
trainingSubjects <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
# rename coolumns
names(trainingSubjects) <- c("subject")

### data ###
# training set
trainingSetData <- read.table("./data/UCI HAR Dataset/train/X_train.txt")

# add activities and subjects to the data
trainingSet <- cbind(subject = trainingSubjects$subject, trainingSetData)
trainingSet <- cbind(activity = trainingActivity$activity, trainingSet)

# remove unnecesary items from memory
rm("trainingActivityIds","trainingActivity","trainingSetData","trainingSubjects")




##### Testing Set #####

### labels ###
# label id for test set
testActivityIds <- read.table("./data/UCI HAR Dataset/test/Y_test.txt")
# merge label names with the ids
testActivity <- merge(activityLabels, testActivityIds, by = "V1", all = TRUE)
# rename columns
names(testActivity) <- c("id", "activity")

### subjects ###
# training subjects
testSubjects <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
# rename coolumns
names(testSubjects) <- c("subject")

### data ###
# test set
testSetData <- read.table("./data/UCI HAR Dataset/test/X_test.txt")

# add activities and subjects to the data
testSet <- cbind(subject = testSubjects$subject, testSetData)
testSet <- cbind(activity = testActivity$activity, testSet)

# remove unnecesary items from memory
rm("testActivityIds","testActivity","testSetData","testSubjects","activityLabels")

##### Merge and Analyzing Data #####
## 1. Merges the training and the test sets to create one data set
mergedData <- merge(trainingSet, testSet, all = TRUE)

names(mergedData) <- featureLabels$feature

rm("testSet", "trainingSet")

## 2. Extracts only the measurements on the mean and standard deviation for each measurement
meanFeatures <- featureLabels[grepl("mean()",featureLabels$feature), ]
meanFeatures <- meanFeatures[!grepl("meanFreq()",meanFeatures$feature), ]
stdFeatures <- featureLabels[grepl("std()",featureLabels$feature), ]
selectedFeaturesNames <- merge(meanFeatures, stdFeatures, all = TRUE)
selectedFeaturesNames <- rbind(data.frame(id = 0, feature = "subject"), selectedFeaturesNames)
selectedFeaturesNames <- rbind(data.frame(id = 0, feature = "activity"), selectedFeaturesNames)

selectedFeatures <- mergedData[, which(names(mergedData) %in% selectedFeaturesNames$feature)]

rm("meanFeatures", "selectedFeaturesNames", "stdFeatures", "featureLabels")

## 3. Uses descriptive activity names to name the activities in the data set
# this is already done by adding the column names to the set

## 4. Appropriately labels the data set with descriptive activity names.
# this is already done by converting activities to small and replacing underscore with 
# blank spaces since the beginning before the activities were bound to the data set

## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
# I couldn't find a way to return the average per column grouped, I could only return the column average in general
# even though is grouped, all fields have the same value which is the aveage for the whole column
newSet <- ddply(selectedFeatures, .(activity, subject), function(x) colwise(mean)(selectedFeatures[ , c(-1, -2)]))

# write the data to the data set
write.table(newSet, file = "./data/tidyData.txt")