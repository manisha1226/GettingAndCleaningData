# 
#  This is my script to download, merge, extract, label, average and create a final tidy data set for 
#  Getting and Cleaning Data course assignment.
#

library(dplyr)
library(data.table)

#
# First I will read all the files I need for this assignment. Please note the Inertial Signals Data
# has already been combined into the Xtest, and Xtrain datasets, and we do not need that for this assignment
#

activity <- read.table("activity_labels.txt",sep="")
features <- read.table("features.txt",sep="")

subjects_test <- read.table("subject_test.txt",sep="")
Xtest <- read.table("X_test.txt",sep="")
Ytest <- read.table("y_test.txt",sep="")

subjects_train <- read.table("subject_train.txt",sep="")
Xtrain <- read.table("X_train.txt",sep="")
Ytrain <- read.table("y_train.txt",sep="")

#
#          *********************                     STEP 1 : Merge        ********************* 
# Next, I will merge all the X, Y, and Subjects train and test datasets to create a single set of X, Y, and Subjects data set
#

X <- rbind(Xtest,Xtrain)
Y <- rbind(Ytest,Ytrain)
subjects <- rbind(subjects_test,subjects_train)

#
#          *********************     STEP 2 & 4 : Extract and Appropriately Label      *********************
#
# Now, we need to only focus on the variables related to the mean and standard deviation for each measurement.
# I will use the information in the "features" dataset t help me extract the mean & std variables from the "X" dataset
#

stdmeanVars <- filter(features,V2 %like% "mean\\()" | V2 %like% "std\\()" )
observations <- X[,stdmeanVars$V1]
names(observations) <- stdmeanVars$V2

#
# Now, I will work on combining the "subjects", "Y" and "observations" datasets into a single dataset "fullobservations".
#

subjactivity <- cbind(subjects,Y)
names(subjactivity) <- c("subject","activitynum")
fullobservations <- cbind(subjactivity,observations)

#
#          *********************     STEP 3 Use descriptive activity names      *********************
#
# Now, I will translate the activity number into the activity name using the "activity" dataset and the merge function
#

fullobservations <- fullobservations %>%
        merge(activity, by.x ="activitynum", by.y="V1") %>%
        rename(activity = V2) %>%
        select(-activitynum) %>%
        select(activity, subject,everything())

#    
#          *********************     STEP 5 create tidy data set with means      *********************
#
# From the data set in step 4, creates a second, independent tidy data set
# with the average of each variable for each activity and each subject.
#

tidydataset <- fullobservations %>%
        group_by(activity,subject) %>%
        summarise_each(mean)

#
# Final Step, write out the data set to a txt file for upload
#

write.table(tidydataset,file="TidyData.txt",row.name=FALSE)

#
# Dear reviewer, you can use the below command to read and view the Tidy Data back
#

data <- read.table("TidyData.txt", header=TRUE)
View(data)

