# Load libraries
library("dplyr")
library("reshape2")

# Load the input data
# Assumes data is in a subdirectory called UCI HAR Dataset under the working directory.
testFeatures <- read.table("UCI HAR Dataset/test/X_test.txt")
testActivities <- read.csv("UCI HAR Dataset/test/y_test.txt", header=FALSE)
testSubjects <- read.csv("UCI HAR Dataset/test/subject_test.txt", header=FALSE)
trainFeatures <- read.table("UCI HAR Dataset/train/X_train.txt")
trainActivities <- read.csv("UCI HAR Dataset/train/y_train.txt", header=FALSE)
trainSubjects <- read.csv("UCI HAR Dataset/train/subject_train.txt", header=FALSE)
featureLabels <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors=FALSE)[,2]
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors=FALSE)[,2]

# Find the features we want to use (mean and std related; see Code Book)
featureNumbers <- grep("mean|std", featureLabels) # find mean- and std-related labels

# Rename features to conform to variable naming guidelines in Week 4 videos and to make them more understandable.
meanAndstdLabels <- featureLabels[featureNumbers] # Select mean- and std-related labels
meanAndstdLabels <- gsub("\\(\\)","",meanAndstdLabels) # Eliminate parentheses from variable names
meanAndstdLabels <- gsub("BodyBody","Body", meanAndstdLabels) # Eliminate redundant "Body"
meanAndstdLabels <- gsub("^t","time", meanAndstdLabels) # Spell out initial "t" into "time"
meanAndstdLabels <- gsub("^f","frequency", meanAndstdLabels) # Spell out initial "f" into "frequency"
meanAndstdLabels <- gsub("Acc","Acceleration", meanAndstdLabels) # Spell out "Acc" into "Acceleration"
meanAndstdLabels <- gsub("Freq","Frequency", meanAndstdLabels) # Spell out "Freq" into "Frequency"
meanAndstdLabels <- gsub("Mag","Mag", meanAndstdLabels) # What does Mag stand for?
meanAndstdLabels <- tolower(meanAndstdLabels) # Turn it all lower case.
meanAndstdLabels <- make.names(meanAndstdLabels, unique=TRUE) # to make sure we have syntactically correct variable names and that they are not repeated after our manipulations

# Susbset the feature data we want to use (mean and std related; see Code Book) and label them appropriately
testMeanStdFeatures <- testFeatures[, featureNumbers]
trainMeanStdFeatures <- trainFeatures[, featureNumbers]
names(trainMeanStdFeatures) <- meanAndstdLabels
names(testMeanStdFeatures) <- meanAndstdLabels

# Label subject lists
names(testSubjects) <- "Subject"
names(trainSubjects) <- "Subject"

# Label activity lists and translate activity numbers to explicit activity names
names(testActivities) <- "Activity"
names(trainActivities) <- "Activity"
testActivities$Activity <- activityLabels[testActivities$Activity]
trainActivities$Activity <- activityLabels[trainActivities$Activity]

# Add trainingOrtest variable
trainData <- character(length(trainActivities$Activity))
testData <- character(length(testActivities$Activity))
trainData[1:length(testActivities$Activity)] <- "train"
testData[1:length(testActivities$Activity)] <- "test"
trainDataColumn <- data.frame(trainortest=trainData, stringsAsFactors=FALSE)
testDataColumn <- data.frame(trainortest=testData, stringsAsFactors=FALSE)

# Add subject, activity, and train/test columns
trainMeanStdData <- cbind(trainSubjects, trainActivities, trainDataColumn, trainMeanStdFeatures)
testMeanStdData <- cbind(testSubjects, testActivities, testDataColumn, testMeanStdFeatures)

# Combine the two sets of data - train and test. This is the first tidy set asked for in the course project statement.
MeanStdData <- rbind(trainMeanStdData, testMeanStdData)

# Second tidy data set
# Three ways to get at it:
meansOfMeanStdDatabySubjectandActivity <- aggregate(MeanStdData[,4:82], by=list(MeanStdData$Subject, MeanStdData$Activity), FUN=mean)
names(meansOfMeanStdDatabySubjectandActivity)[1:2] <- c("Subject", "Activity")
# Second way, using summarise_each.
#meansOfMeanStdDatabySubjectandActivity <- summarise_each(group_by(MeanStdData, Subject, Activity), funs(mean))
# Third way, melt the data into a skinny tidy set arranged by Subject and Activity
#MeanStdDataMelt <- melt(MeanStdData, id=c("Subject", "Activity"), measure_vars=meanAndstdLabels)
# ...then apply the mean function to all the selected features by Subject and Activity
# meansOfMeanStdDatabySubjectandActivity <- dcast(MeanStdDataMelt, Subject + Activity ~ meanAndstdLabels, mean)

write.table(meansOfMeanStdDatabySubjectandActivity, "tidyset2.txt", row.names=FALSE)
