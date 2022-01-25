## Coursera - Getting and Cleaning Data - Project

## 1. Download Data
library(data.table)
library(utils)

fURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

if (!file.exists("J:\\9. Pobrane\\getdata_projectfiles_UCI HAR Dataset.zip")){
  download.file(fURL, destfile = "J:\\9. Pobrane\\getdata_projectfiles_UCI HAR Dataset.zip", mode = wb)
  unzip("J:\\9. Pobrane\\UCI HAR Dataset.zip", exdir = "./data")
}

## 2. Read Data 

## List of all features
features <- read.csv("./data/UCI HAR Dataset/features.txt", header = FALSE, sep = " ")
features <- as.character(features[,2])

## Training ./train
TrainingSet <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
TrainingLabels <- read.csv("./data/UCI HAR Dataset/train/y_train.txt", header = FALSE, sep = " ")
SubjectTrain <- read.csv("./data/UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = " ")
# 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30

colnames(TrainingSet) = features
colnames(TrainingLabels) = "Labels"
colnames(SubjectTrain) = "Subject"
train <- data.frame(SubjectTrain, TrainingLabels, TrainingSet)

## Test ./test
SubjectTest <- read.csv("./data/UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = " ")
TestLabels <- read.csv("./data/UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = " ")
TestSet <- read.table("./data/UCI HAR Dataset/test/X_test.txt")

colnames(SubjectTest) = "Subject"
colnames(TestLabels) = "Labels"
colnames(TestSet) = features
test <- data.frame(SubjectTest, TestLabels, TestSet)

## Merges the training and test Sets
allT <- rbind(train, test)

## 3. Cleaning
## a. Extracts only the measurements on the mean and standard deviation for each measurement.
## look for a regular expression by using grep
Mean_STD <- grep("[Mm]ean|[Ss][Tt][Dd]", features)

Extract <- allT[,c(1,2,Mean_STD +2)]

## b. Uses descriptive activity names to name the activities in the data set
ActivityLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt", header = FALSE)
ActivityLabels <- as.character(ActivityLabels[,2])

Extract$Labels <- ActivityLabels[Extract$Labels]

## c. appropriately labels the data set with descriptive variable names
names(Extract) <- gsub("Acc", "Accelerometer", names(Extract))
names(Extract) <- gsub("Gyro", "Gyroscope", names(Extract))
names(Extract) <- gsub("BodyBody", "Body", names(Extract))
names(Extract) <- gsub("Mag", "Magnitude", names(Extract))
names(Extract) <- gsub("^t", "Time", names(Extract))
names(Extract) <- gsub("^f", "Frequency", names(Extract))
names(Extract) <- gsub("-mean()", "Mean", names(Extract), ignore.case = TRUE)
names(Extract) <- gsub("-std()", "STD", names(Extract), ignore.case = TRUE)
names(Extract) <- gsub("-freq()", "Frequency", names(Extract), ignore.case = TRUE)
names(Extract) <- gsub("angle", "Angle", names(Extract))
names(Extract) <- gsub("gravity", "Gravity", names(Extract))

## d. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
Extract <- data.table(Extract)
dim(Extract)

TidyData <- aggregate(Extract[,3:88], by = list(Subject = Extract$Subject, Activity = Extract$Labels), FUN = mean)
write.table(x = TidyData, file = "TidyData.exe", row.names = FALSE)


