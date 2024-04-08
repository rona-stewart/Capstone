## Obtain the datasets
download.file ("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip","Coursera-SwiftKey.zip")

## Unzip the dataset 
zipfilepath <- "./Coursera-Swiftkey.zip"
extracted_dir <- "./Data"
if (!dir.exists(extracted_dir)) {
        dir.create(extracted_dir)
}
unzip(zipfilepath, exdir = extracted_dir)

# Load the relevant libraries
library(tidytext); library(tm); library(quanteda)

# Create a directory path to the files
firstfolder <- list.files("./Data")
filepath <- file.path("./Data",firstfolder)

secondfolder <- list.files(file.path(".","Data",firstfolder))
allfilepaths <- NULL

for (i in 1:length(secondfolder)){
        thirdfolder <- list.files(file.path(".","Data",firstfolder,secondfolder[i]))
        allfilepaths <- c(allfilepaths, file.path(".","Data",firstfolder, secondfolder[i],thirdfolder))
}

# Explore the EN data files
ENfiles <- allfilepaths[4:6]
len1 <- length(readLines(ENfiles[1])) # Note that this returns 899288 entries
len2 <- length(readLines(ENfiles[2])) # Note that this returns 77259 entries
len3 <- length(readLines(ENfiles[3])) # Note that this returns 2360148 entries

# Given the volume of data, read only a random subset (30%) of each file in to the training data for manipulation
### Set seed to ensure reproducability of results
set.seed(1240)
### Create a vector identifying those elements for the training set
inTrain1 <- caret::createDataPartition(y = (1:len1), p=0.5, list = TRUE)
inTrain2 <- caret::createDataPartition(y = (1:len2), p=0.5, list = TRUE)
inTrain3 <- caret::createDataPartition(y = (1:len3), p=0.5, list = TRUE)

# Read in the data and create the subset for analysis
data1 <- readLines(ENfiles[1], skipNul = TRUE)
blogssub <- data1[inTrain1$Resample1]

data2 <- readLines(ENfiles[2], skipNul = TRUE)
newssub <- data2[inTrain2$Resample1]

data3 <- readLines(ENfiles[3], skipNul = TRUE)
twitsub <- data3[inTrain3$Resample1]

# Clean memory space
rm(firstfolder, filepath, secondfolder, thirdfolder, allfilepaths, i, ENfiles, 
   len1, len2, len3, inTrain1, inTrain2, inTrain3, data1, data2, data3)