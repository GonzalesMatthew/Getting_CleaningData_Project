##Merge Training and Test Sets to Create One Set
## setwd("mydirectoryhere")

##Read Features and activityType
features <- read.table('./features.txt', header = FALSE)
activityType <- read.table('./activity_labels.txt',header = FALSE)

##Read Training Data Sets
subjectTrain <- read.table('./train/subject_train.txt', header = FALSE)
xTrain <- read.table('./train/X_train.txt', header = FALSE)
yTrain <- read.table('./train/y_train.txt', header = FALSE)

##Assign column names to data
colnames(activityType) <- c('activityID','activityType')
colnames(subjectTrain) <- "subjectId"
colnames(xTrain) <- features[,2]
colnames(yTrain) <- "activityId"

##Create Training Data: CBind the separate tables
trainingData <- cbind(subjectTrain,xTrain,yTrain)

##Read Testing Data Sets
subjectTest <- read.table('./test/subject_test.txt', header = FALSE)
xTest <- read.table('./test/X_test.txt', header = FALSE)
yTest <- read.table('./test/y_test.txt', header = FALSE)

##Assign column names to data
colnames(subjectTest) <- "subjectId"
colnames(xTest) <- features[,2]
colnames(yTest) <- "activityId"

#Create Testing Data: CBind the separate tables
testingData <- cbind(subjectTest,xTest,yTest)

#Combine Training and Testing Data
Data <- rbind(trainingData,testingData)

##Create Vector of Column Names of Data table 
colNames <- colnames(Data)

##2. Extracts only the measurements on the mean and standard 
##deviation for each measurement.

# Create vector that keeps desired keywords

Vector <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))
Data <- Data[Vector==TRUE]

# 3. Use descriptive activity names to name the activities in the data set

#merge Data with actiityType table to get descriptions
Data <- merge(Data,activityType,by='activityId',all.x=TRUE)

#Update the colNames after the merge
colNames<-colnames(Data) 

## 4.Appropriately label the data set with descriptive activity names. 

##Clean up variable names
for (i in 1:length(colNames)) 
{
  colNames[i] <- gsub("\\()","",colNames[i])
  colNames[i] <- gsub("-std$","StdDev",colNames[i])
  colNames[i] <- gsub("-mean","Mean",colNames[i])
  colNames[i] <- gsub("^(t)","time",colNames[i])
  colNames[i] <- gsub("^(f)","freq",colNames[i])
  colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
}

##Update COlumn names after the manipulation
colnames(Data)<-colNames

##5.Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a table without the activityType column
DataNoActivityType  <- Data[,names(Data) != 'activityType']

# Summarizing the table to include only the mean of each variable for each activity/subject
Tidy  <- aggregate(DataNoActivityType[,names(DataNoActivityType) != c('activityId','subjectId')],by=list(activityId=DataNoActivityType$activityId,subjectId = DataNoActivityType$subjectId),mean)

##Merge Tidy with activityType to include descriptive acitvity names
Tidy    <- merge(Tidy,activityType,by='activityId',all.x=TRUE)

# ExportTidy 
write.table(Tidy, './Tidy.txt',row.names=TRUE,sep='\t')
