# This is the code for the Assignment of Getting and Cleaning data. Author: JacobaDS.
# Step 0: download data & unzip
# Step 1: Merges the training and the test sets to create one data set.
# Step 1A: read all data into R
# Step 1B: explore what is in this data, to find out which merges makes sense
# Step 1C: give names to the 561 columns of the test & train datasets. Names according to the features.
# Step 1D: merge
# Step 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
# Step 3: Uses descriptive activity names to name the activities in the data set
# Step 4: Appropriately labels the data set with descriptive variable names.
# Step 5.From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject

setwd("D:/My Data/PersonalFiles_for_Backup/CursusDataScientist/DS_03/wk4/Assignment")

#Step 0. Download zip file to working directory & unzip data:
dataset_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" 
download.file(dataset_url,"Dataset.zip")
unzip("Dataset.zip")

#as a result, a subdirectory "UCI HAR Dataset" was created. So I change the working directory to that sub directory:
setwd("D:/My Data/PersonalFiles_for_Backup/CursusDataScientist/DS_03/wk4/Assignment/UCI HAR Dataset")
#let's see what files are in there:
list.files()

#Step 1.Merges the training and the test sets to create one data set.
#Step 1A. Read all training and test data into R. Moreover, read the features:

train <- read.table("train/X_train.txt")
train_labels <- read.table("train/y_train.txt")
train_subject <- read.table("train/subject_train.txt")

test <- read.table("test/X_test.txt")
test_labels <- read.table("test/y_test.txt")
test_subject <- read.table("test/subject_test.txt")

features_char <- read.table("features.txt",stringsAsFactors = FALSE) 

#Step 1B: xplore what is in this data, to find out which merges makes sense
str(train)         #=> data frame ; 7352 rows ; 561 columns ; numeric values, no descriptive column names yet
sum(is.na(train)) #0 => No NAs detected

str(train_labels) #=> data frame ; 7352 rows ; 1 column ; integer values
sum(is.na(train_labels)) #0 => No NAs detected
summary(train_labels) #=> 1,2,...,6 => these are the 6 activities

str(train_subject) #=> data frame ; 7352  rows ; 1 column ; integer values
sum(is.na(train_subject)) #0 => No NAs detected
summary(train_subject) #=> 1,..., 30 => representing the 30 volunteers

str(test)         #=> data frame ; 2947 rows ; 561 columns ; numeric values, no descriptive column names yet
sum(is.na(test)) #0 => No NAs detected

str(test_labels) #=> data frame ; 2947 rows ; 1 column ; integer values
sum(is.na(test_labels)) #0 => No NAs detected
summary(test_labels) #=> 1,2,...,6 => these are the 6 activities

str(test_subject) #=> data frame ; 2947 rows ; 1 column ; integer values
sum(is.na(test_subject)) #0 => No NAs detected
summary(test_subject) #=> 2,..., 24 => => representing parts of the 30 volunteers

str(features_char) #data frame with 561 features (characters), in second column (V2)

# Step 1C: give names to the 561 columns of the test & train datasets. Names according to the 561 features.
names(train) <- features_char$V2
names(test) <- features_char$V2

# Step 1D: merge
# What kind of merge makes sence? In my opinion, a row bind would make sence = all observations in 1 dataset
# note: test = 30% of the volunteers & train = 70% of the volunteers. Create data = 100% of the participants
data <- rbind(train,test)
str(data)

# Moreover, a merge (=row bind) of train_labels With test_labels makes sense
# => 10.299 observations and 1 column (representing the integer label of the activity)
labels <- rbind(train_labels,test_labels)

# Finally,, merge train_subject & test_subject 
# => 10.299 observations and 1 column (representing the volunteer)
subjects <- rbind(train_subject,test_subject)
names(subjects) <- "subject"

# step 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
# I am going to use select from dplyr package, i.c.w.: contains mean / std
# Step 2A. some pre processing turns out to be necessary:
library(dplyr)

#using select gives following error:
#found duplicated column name: fBodyAcc-bandsEnergy()-1,8, fBodyAcc-bandsEnergy()-9,16,...

#tip from stack overflow:
#The root of the problem is invalid characters in the original column names. 
#Try forcing unique column names with valid characters, with make.names() . 

valid_column_names <- make.names(names=names(data), unique=TRUE, allow_ = TRUE)
names(data) <- tolower(valid_column_names)

#Step 2B. Extracts only the measurements on the mean and standard deviation for each measurement. 
data_mean_std <- select(data,contains("mean"),contains("std")) #10.299 rows & 86 columns

#3: Uses descriptive activity names to name the activities in the data set
#see: activity_labels.txt
activities<- read.table("activity_labels.txt") 
#change WALKING_UPsTAIRS to walkingupstairs etcetera (more tidy), using gsub as follows:
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
#change the integer labels to these descriptions, by using subsetting:
activities[labels[,1], 2] #=> standing, standing, .... 
labels[,1] = activities[labels[,1], 2] #=> overwrite labels with this
#name the column of label: activity
names(labels) <- "activity"

# Step 4: Appropriately labels the data set with descriptive variable names.
# After all the pre processing above, all that remains is a simple column bind of subjects, labels & data_mean_std
data_all <- cbind(subjects,labels,data_mean_std)
write.table(data_all,"merged_test_train.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject. 

uniqueSubjects = unique(subjects)[,1] 
uniqueSubjects
numSubjects = length(unique(subjects)[,1]) #30
numActivities = length(activities[,1]) #6
numCols = dim(data_all)[2]  
result = data_all[1:(numSubjects*numActivities), ] 
str(result) 

row = 1 
for (s in 1:numSubjects) { 
  for (a in 1:numActivities) { 
    result[row, 1] = uniqueSubjects[s] 
    result[row, 2] = activities[a, 2] 
    tmp <- data_all[data_all$subject==s & data_all$activity==activities[a, 2], ] 
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols]) 
    row = row+1 
  } 
} 

str(result) 
write.table(result, "tidy_data_step_5.txt" , row.name = FALSE) 
