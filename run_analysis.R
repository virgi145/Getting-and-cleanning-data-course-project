run_analysis <- function(){

# Set the working directory with the data set
setwd("C:/Users/Usuario/Documents/R/coursera/UCI HAR Dataset")
  
# 1) MERGES THE TRAINING AND THE TEST SETS TO CREATE ONE DATA SET
  
  # Read the general files
  features <- read.table("features.txt", col.names = c("fid", "flabel")) # Features names
    
  # Read the train files
  subjecttrain <- read.table("train/subject_train.txt") # Subject identifier
  ytrain <- read.table("train/y_train.txt") # Activity labels
  xtrain <- read.table("train/X_train.txt") # Features values

  # Create the train data frame
  subjectytrain <- data.frame(subjecttrain, ytrain) # Subject and Activity train data frame
  names(subjectytrain) <- c("subject", "activity") # Assign names to subjectytrain
  names(xtrain) <- as.character(features$flabel) # Assign names to xtrain
  traindata <- cbind.data.frame(subjectytrain, xtrain) # Train data frame
    
  # Read the test files
  subjecttest <- read.table("test/subject_test.txt") # Subject identifier
  ytest <- read.table("test/y_test.txt") # Activity labels
  xtest <- read.table("test/X_test.txt") # Features values

  # Create the test data frame
  subjectytest <- data.frame(subjecttest, ytest) # Subject and Activity test data frame
  names(subjectytest) <- c("subject", "activity") # Assign names to subjectytest
  names(xtest) <- as.character(features$flabel) # Assign names to xtest
  testdata <- cbind.data.frame(subjectytest, xtest) # Test data frame    
    
  # Combine train and test data frames
  mydata <- rbind.data.frame(traindata, testdata) # Train and test data frame
  
# 2) EXTRACTS ONLY THE MEASUREMENTS ON THE MEAN AND STANDARD DEVIATION FOR EACH MEASUREMENT
    
  require(dplyr)
  mytable <- tbl_df(mydata) # Covert data frame to data table for dplyr
  tablenames <- make.names(names = names(mytable), unique = TRUE) # Correct duplicated names
  names(mytable) <- tablenames # Assign names to mytable
  meanstdtable <- select(mytable, subject, activity, contains("mean"), contains("std")) # Select columns
  
# 3) USES DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES IN THE DATA SET
    
  # Read the general files
  activitylabels <- read.table("activity_labels.txt", col.names = c("activity", "activitylabel")) # Labels
    
  # Merge the data frames with activity id and activity labels
  activitytable <- merge(meanstdtable, activitylabels, by.x = "activity", by.y = "activity")

# 4) APPROPRIATELY LABELS THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES
  
  # Remove dots from column names
  names(activitytable) <- gsub("\\.", "", names(activitytable))
    
# 5) CREATE DATA SET WITH THE AVERAGE OF EACH VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT
  
  # Group and summarise
  grouptable <- group_by(tbl_df(activitytable), subject, activitylabel)
  dataset <- summarise_each(grouptable, funs(mean))
  
  # Create txt file
  write.table(dataset, file = "dataset.txt", row.names = FALSE)
  
}
  