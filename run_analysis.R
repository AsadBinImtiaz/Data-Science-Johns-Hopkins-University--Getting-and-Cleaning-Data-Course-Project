#install.packages("dplyr")
library(dplyr)
packageVersion("dplyr")

datadir <- "UCI HAR Dataset"
datafile <- paste(datadir,"zip",sep=".")

# download data
if(!dir.exists(datadir)) {
  if(!file.exists(datafile)) 
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", datafile)
  if(file.exists(datafile)) 
    unzip(datafile)
}

# Read Files
# Data Labels
features      <- read.table(paste(datadir,"features.txt"             , sep="/"), header=FALSE, col.names=c("id","name"))

# Only Mean and Std are important
features_imp  <- filter(features, grepl("mean\\(|std\\(", name)) %>% mutate( name = gsub("\\(","",name) , name = gsub("\\)","", name) , name = gsub("-","_", name), name = tolower(name))
features      <- mutate( features, name = gsub("\\(","",name) , name = gsub("\\)","", name) , name = gsub("-","_", name), name = tolower(name))
feature_names <- as.vector(features[, "name"])

# Activity labels
acty_lbls     <- read.table(paste(datadir,"activity_labels.txt"      , sep="/"), header=FALSE , col.names=c("label","activity"))

# Training Data
feature_train <- read.table(paste(datadir,"train","X_train.txt"      , sep="/"), header=FALSE , col.names=feature_names)
label_train   <- read.table(paste(datadir,"train","y_train.txt"      , sep="/"), header=FALSE , col.names="label")
Ids_train     <- read.table(paste(datadir,"train","subject_train.txt", sep="/"), header=FALSE , col.names="subject")

# Test Data
feature_test  <- read.table(paste(datadir,"test" ,"X_test.txt"       , sep="/"), header=FALSE , col.names=feature_names)
label_test    <- read.table(paste(datadir,"test" ,"y_test.txt"       , sep="/"), header=FALSE , col.names="label")
Ids_test      <- read.table(paste(datadir,"test" ,"subject_test.txt" , sep="/"), header=FALSE , col.names="subject")

# Merges the training and the test sets to create one data set.
train_data    <- data.frame(Ids_train,feature_train,label_train)
test_data     <- data.frame(Ids_test ,feature_test ,label_test)
data          <- rbind(train_data, test_data)

# Clear memory
remove(features)
remove(feature_train,label_train,Ids_train)
remove(feature_test ,label_test ,Ids_test )
remove(test_data,train_data)

# Extract only the measurements on the mean and standard deviation for each measurement.
# Replace Labels with Activity-labels
data <- data[ , c("subject","label",features_imp$name)] %>% merge(acty_lbls) %>% select( -label )

# Write data to file
write.table(data, "mean_std_per_activity_subject.txt")

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
avgdata <- group_by(data,activity,subject) %>%summarise(across(where(is.numeric), mean, na.rm = TRUE))
## Write the data out
write.table(avgdata, "avg_per_activity_subject.txt")
