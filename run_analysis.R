library(plyr)
library(dplyr)

#variables
workFolder <- "GCDataProject"
dataDir <- paste(getwd(),workFolder, sep="/")

#constants
zipDir <-paste(workFolder,"UCI HAR Dataset",sep="/")
testDir <-"test"
trainDir <-"train"

main <- function()
{
  prepareFiles() #uncomment to rerun data
  
  #(1) Merges the training and the test sets to create one data set.
  #(3) Uses descriptive activity names to name the activities in the data set
	dataSet <- loadDataSet()
	
	#(2) Extracts only the measurements on the mean and standard deviation for each measurement.
	newdataSet <- getStats(dataSet)
	
	#(4) Appropriately labels the data set with descriptive variable names.
	assignActivities(dataSet)
	
	#(5)From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
	groupByIDActivity(newdataSet)
}

#download and unzip file into a temp folder
prepareFiles <- function()
{
  #prepare temp folders
  unlink(dataDir, recursive=TRUE) #delete inconsiderably!
  dataURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  dir.create(dataDir, showWarnings = TRUE, recursive = FALSE)
  
  #download and unzip
  dataFile <- paste(workFolder,basename(dataURL),sep="/")
  download.file(dataURL,dataFile)
  unzip(dataFile, list = FALSE, overwrite = TRUE,exdir=workFolder)
}

#(1) Merges the training and the test sets to create one data set.
#read unzipped folder for key files and build porject dataset
loadDataSet <- function()
{
  #cycle through folders looking for needed files
  files <- list.files(zipDir, recursive=TRUE)
  dataXFiles <- files[grepl("(^test|^train)/[X]_(test|train).txt", files)]
  dataYFiles <- files[grepl("(^test|^train)/[y]_(test|train).txt", files)]
  subjectsFiles <- files[grepl("(^test|^train)/subject_(test|train).txt", files)]
  featureFile <- files[grepl("^features.txt$", files)]
  
  #append relative objects
  SData <- mergeFiles(subjectsFiles) #  1 col
  Ydata <- mergeFiles(dataYFiles)    #  1 col
  Xdata <- mergeFiles(dataXFiles)    #561 col
  features <- mergeFiles(featureFile)
  
  #side-by-side join observations
  #TODO consider merge
  dataSet <- cbind(SData,Ydata,Xdata)
  
  #(3) Uses descriptive activity names to name the activities in the data set
  #rename columns per documentation
  cnames <- c(c("id"),c("activity"),as.character(features$V2))
  dataSet <- setNames(dataSet, cnames)
  dataSet
}

#(2) Extracts only the measurements on the mean and standard deviation for each measurement.
#get only columns with means and sds into new dataset
#param:dataSet - data table
getStats <- function(dataSet)
{
  means <- grep("std",names(dataSet))
  sds   <- grep("mean",names(dataSet))
  newColumns <- c(1,2,means,sds)
  newdataSet <- dataSet[,newColumns]
  savetodisk(newdataSet,"newStats")
  newdataSet
}

#(4) Appropriately labels the data set with descriptive variable names.
#read activity labels and replace activity col with factored names
#param:dataSet - data table
assignActivities <- function(dataSet)
{
  files <- list.files(zipDir, recursive=TRUE)
  activityFile <- files[grepl("^activity_labels.txt$", files)]
  filePath<-paste(zipDir,activityFile,sep="/")
  activities <- read.table(filePath)
  dataSet$activity <- factor(dataSet$activity,levels=activities$V1,labels=activities$V2)
  dataSet
}

#(5)
#creates a second, independent tidy data set
#of average of each variable for each activity and each subject
groupByIDActivity <- function(newdataSet)
{
  res <- ddply(newdataSet, .(id,activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
  savetodisk(res,"tidydataset")
}

#helper functions
#Append multiple files into one
#param: files - homogenious files containing same type of datasets
mergeFiles <- function (files)
{	
  for(i in seq(length(files)))
  {
    filePath<-paste(zipDir,files[i],sep="/")
    x <- read.table(filePath)
    if(i>1)
    {
      dumpfiles <- rbind(dumpfiles,x)
    }
    else
    {
      dumpfiles <-x
    }
  }
  dumpfiles
}

#save object into a file
#param: object - data table 
#param: filename - only the name of the file 
savetodisk <- function (object,filename)
{
  filePath <- paste(dataDir, "/", filename,".csv" ,sep="")
  write.table(object,filePath,row.name=FALSE, sep=",")
}


			




