#run_analysis.R
#download the datset and save it to folder 'wearable'
#the data can be found here: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#For purposes of the analysis, we can discard the Inertial Signals data

#The assignment:
#You should create one R script called run_analysis.R (this file) that does the following.

#1.Merges the training and the test sets to create one data set.
#2.Extracts only the measurements on the mean and standard deviation for each measurement.
#3.Uses descriptive activity names to name the activities in the data set
#4.Appropriately labels the data set with descriptive variable names.
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(plyr)
library(dplyr)

runAnalysis<-function(){
#returns a tidy dataset
#usage: 
#1. set your wd to the location of your downloaded data
#  ie. setwd("./rwd/wearabledata")
#2. load this source
#   ie. source("run_analysis.R")
#3. run the analysis
#    ie. tidy<-runAnalysis()
#4. export the tidy data
#    ie. write.csv(tidy, file="tidydata.csv", row.names=FALSE)
    
#read the activities...
activities<-read.table(file="activity_labels.txt")
#make tidy...6 distinct activities are expected
activities<-plyr::rename(activities, c("V1"="activity_id","V2"="activity"))

#read the features...
features<-read.table(file="features.txt")
#make tidy...561 distinct features are expected
features<-plyr::rename(features, c("V1"="feature_id","V2"="feature"))
#flag the features that are means or std deviations...
features$mean_or_std<-FALSE
mean_or_std<-grep("-mean\\(|-std\\(", features$feature)
features[mean_or_std,"mean_or_std"]<-TRUE

#read subject data
#read subject training and label var as 'subject_id'
subject_train<-read.table(file="./train/subject_train.txt")
subject_train<-plyr::rename(subject_train, c("V1"="subject_id"))

#read subject testing and label var as 'subject_id'
subject_test<-read.table(file="./test/subject_test.txt")
subject_test<-plyr::rename(subject_test, c("V1"="subject_id"))

#read training data & assign var names
x_train<-read.table("./train/X_train.txt", header=FALSE)
colnames(x_train)<-features$feature
y_train<-read.table("./train/Y_train.txt", header=FALSE)
y_train<-plyr::rename(y_train, c("V1"="activity_id"))

#read testing data & assign var names
x_test<-read.table("./test/X_test.txt", header=FALSE)
colnames(x_test)<-features$feature
y_test<-read.table("./test/y_test.txt", header=FALSE)
y_test<-plyr::rename(y_test, c("V1"="activity_id"))

#combine training data into 1 dataset
training_data<-cbind(subject_train, y_train, x_train)

#combine testing data into 1 dataset
testing_data<-cbind(subject_test, y_test, x_test)

#recombine training and testing datasets into 1 dataset
combined_data<-rbind(training_data, testing_data)
#join combined data with activities on activity_id
joined_data<-plyr::join(combined_data, activities, by="activity_id")

#subset joined data to only include measures that are mean or std deviations
measures<-filter(features, mean_or_std==TRUE)
master_data<-joined_data[,c('subject_id','activity',as.character(measures$feature))]

#make a tidy dataset with descriptive var names that are intuitive
tidy<-aggregate(. ~subject_id + activity, master_data, mean)
sorted_tidy<-arrange(tidy, subject_id, activity)
#give features more descriptive, intuitive names
measures$descriptive<-measures$feature
measures$descriptive<-gsub("^t", "time", measures$descriptive)
measures$descriptive<-gsub("^f", "freq", measures$descriptive)
measures$descriptive<-gsub("Acc", "Accelerometer", measures$descriptive)
measures$descriptive<-gsub("Gyro", "Gyroscope", measures$descriptive)
measures$descriptive<-gsub("Mag", "Magnitude", measures$descriptive)
measures$descriptive<-gsub("BodyBody", "Body", measures$descriptive)
measures$descriptive<-gsub("-mean\\(\\)", "Mean", measures$descriptive)
measures$descriptive<-gsub("-std\\(\\)", "SDev", measures$descriptive)
descriptives<-c('subject','activity', as.character(measures$descriptive))
colnames(sorted_tidy)<-descriptives
return(sorted_tidy)
}



