# If you haven't installed reshape2, and plyr, install it for the use of this data package
# Author is Zamora, Juan Carlo 7 February 2018
install.packages("reshape2")

# Set your Working Directory First, remmeber that R reads /
# In this data set, I have my working directory inside my personal computer Zamora
setwd("C:/Users/zamojua/Documents/Graduate Studies/Data Science Course/Cleaning Data/Week 4/Week 4 PRoject/UCI HAR Dataset")

# Initital Step -- Preparing Data
# Download Human Activity Recognition(HAR) Database from UCI Machine Learning Repository
# This checks the Working Directory's contents and downloads the necessary data
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

# Command that unzips the dataSet to the created /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")

# Action 1-- This code merges the training and the test sets to create one data set
# This code reads the data from the Train folder use read.tale command
subjectTrain<-read.table("./data/UCI HAR Dataset/Train/subject_train.txt") 
xTrain <- read.table("./data/UCI HAR Dataset/Train/X_train.txt")
yTrain <- read.table("./data/UCI HAR Dataset/Train/y_train.txt")

# This code combines all three train dataframes by columns in CombinedDataTrain dataframe-- use cbind
CombinedDataTrain <- cbind(subjectTrain,xTrain,yTrain)

# Read data in Test folder
subjectTest <- read.table("./data/UCI HAR Dataset/Test/subject_test.txt")
xTest <- read.table("./data/UCI HAR Dataset/Test/X_test.txt")
yTest <- read.table("./data/UCI HAR Dataset/Test/y_test.txt")

# Combine all three test dataframes by columns in CombinedDataSet dataframe
CombinedDataSet<-cbind(subjectTest,xTest,yTest)

# Append CombinedDataTrain and CombinedDataSet - combine them by rows in big dataframe named as CombinedDataSet
CombinedDataSet<-rbind(CombinedDataTrain,CombinedDataSet)

# Read the features.txt  file with labels for the measures from UCI HAR Dataset folder
FeatureName<-read.table("./data/UCI HAR Dataset/features.txt")

# Rename the columns in Data dataframe. Use dim(labels) to view which names to be rename
names(CombinedDataSet)[1] <- "SubjectID"
names(CombinedDataSet)[563] <- "Activity"
names(CombinedDataSet)[2:562] <- as.character(FeatureName[ ,2])
names(CombinedDataSet)

# Action 2 -- Extracts only the measurements on the mean and standard deviation for each measurement
# Extract the measurements on the mean and standard deviation for each measurement together with the SubjectID and activity column
CombinedDataSetTidyTidy <- CombinedDataSet[,grepl("mean\\(\\)|std\\(\\)|SubjectID|Activity",names(CombinedDataSet))]

# Action 3 --Use descriptive activity names to name the activities in the data set
# Read activity_labels.txt file from UCI HAR Dataset Folder
activityLabels<-read.table("./Data/UCI HAR Dataset/activity_labels.txt")

# Generating new variable activityName which will store the activity labels
CombinedDataSetTidy$activityName <- rep(NA,nrow(CombinedDataSetTidy))

# Assigning the activity labels to activityName variable
for (i in 1:length(activityLabels[,2])) 
{                       
  CombinedDataSetTidy$activityName[CombinedDataSetTidy$Activity==i] <- as.character(activityLabels[i,2])
}

# Transforming the variable into factor 
CombinedDataSetTidy$activityName <- as.character(CombinedDataSetTidy$activityName)        

# Action 4 -- Appropriately label the data set with descriptive column names
# Clean the names of the variables in dataTidy dataframe
anyDuplicated(names(CombinedDataSetTidy))#There are no duplicates names in the columns in the dataframe

# Eliminate the special characters in labels like (,),- e.t.c
names(CombinedDataSetTidy) <- gsub("\\(", "", names(CombinedDataSetTidy))
names(CombinedDataSetTidy) <- gsub("\\)", "", names(CombinedDataSetTidy))
names(CombinedDataSetTidy) <- gsub("-","",names(CombinedDataSetTidy))

# All lower case in the labels
names(CombinedDataSetTidy) <- tolower(names(CombinedDataSetTidy))

# Save the dataframe-tidy data as a text file 
write.table(CombinedDataSetTidy,"FirstTidy.txt", row.names = FALSE)   


# Action 5 -- Create a second, independent tidy data set with the average of each variable for each activity and each subject
library(reshape2)

# Define the vector of ID variables, since we have done string manipulation, "activityName" will be set to all lowercases as a "activityname"        
idVars <- c("activity","activityname","subjectid")

# Define the vector of measured variables        
measures <- names(CombinedDataSetTidy)[2:67]

# Melt CombinedDataSetTidy dataframe into form suitable for casting
melted <-melt(CombinedDataSetTidy,idVars,measure.vars=measures) 

# Recast melted data computing avarage of measures
SecondTidy <- dcast(melted, activityname + subjectid ~ variable, mean)

# Save the dataframe of averaged measures by activity and subject into txt file        
write.table(SecondTidy,"SecondTidy.txt" ,row.names = FALSE)

