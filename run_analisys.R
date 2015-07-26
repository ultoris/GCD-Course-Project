require("dplyr")

# Download and unzip data
get_data <- function (url="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"){
        download.file(url,destfile="dataset.zip",method="curl")
        unzip("dataset.zip")
}

# Get the data
get_data()

# Enter to the new folder
setwd("UCI HAR Dataset/")
# Read features labels
features <- read.table("features.txt")
features <- features[,2]
# Read activities labels
activities <- read.table("activity_labels.txt")

# Get test data and label it
X.test <- read.table("test/X_test.txt")
names(X.test)<-features
y.test <- unlist(read.table("test/y_test.txt"))
X.test$activity = factor(y.test,levels=activities[,1],labels=as.character(activities[,2]))
X.test$subject <- unlist(read.table("test/subject_test.txt"))

# Get train data and label it
X.train <- read.table("train/X_train.txt")
names(X.train)<-features
y.train <- unlist(read.table("train/y_train.txt"))
X.train$activity = factor(y.train,levels=activities[,1],labels=as.character(activities[,2]))
X.train$subject <- unlist(read.table("train/subject_train.txt"))

# Merge both datasets
X.merged <- rbind(X.test,X.train)

# Select only mean and std measures
is.mean <- grepl("mean",features)
is.std <- grepl("std",features)
selector = is.mean | is.std
X.mean_std <- X.merged[,selector]

# Generate labels for subject/activity
combinations <- expand.grid(1:30,activities[,2])

# Compute the mean for each subject/activity
compute_means <- function(cmb){
        f=filter(X.mean_std,activity==cmb[[2]] & subject==as.numeric(cmb[[1]]))
        f=select(f,-subject,-activity)
        sapply(f,mean)
} 
X.means<-t(apply(combinations,1,compute_means))

# Assign labels to the means dataset
combine_names <- function(cmb){
        paste(cmb,collapse=".")
} 
X.means<-data.frame(X.means)
row.names(X.means)<-apply(combinations,1,combine_names)

# Return to parent folder
setwd("..")

# Save means dataset
write.table(X.means,file="tidy_dataset.csv")
write.table(X.means,file="tidy_dataset_norowname.csv",row.name=FALSE )
