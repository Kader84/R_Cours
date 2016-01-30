#################################################################################
##
## Coursera Getting and Cleaning Data
## 2015-10-01
## Abdelkader LOUNIS
#################################################################################


# set working directory 

setwd("/Users/abdelkader-lounis/Documents/CoursR/UCI HAR Dataset");

# Read files 

features= read.table("./features.txt",header=FALSE);
activityType = read.table("./activity_labels.txt",header=FALSE);
subjectTrain = read.table("./train/subject_train.txt",header=FALSE);
xTrain= read.table("./train/x_train.txt",header=FALSE);
yTrain= read.table("./train/y_train.txt",header=FALSE);

# Assign column names 

colnames(activityType) = c("activityId","activityType");
colnames(subjectTrain) = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

# Merging the training data

trainingData = cbind(yTrain,subjectTrain,xTrain);

# Read files test data 

subjectTest = read.table("./test/subject_test.txt",header=FALSE); 
xTest       = read.table("./test/x_test.txt",header=FALSE); 
yTest       = read.table("./test/y_test.txt",header=FALSE); 

#Assign colum names 

colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";


#Merging Test data 

testData = cbind(yTest,subjectTest,xTest);

#Final Data

finalData = rbind(trainingData,testData);
colNames = colnames(finalData);

#Mean and std for each measurement

  ## TRUE for ID, Mean and STD colums , FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

  ## final data with mean and std
finalData = finalData[logicalVector==TRUE];

# name activities 

finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);
colNames  = colnames(finalData); 

# Appropriately label the data set with descriptive activity names

#cleaning variable names

for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

#new description

colnames(finalData) = colNames;

#independent tidy data set with the average of each variable for each activity and each subject

#finalDataNoActivityType without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];

#summarizing
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# final data 

tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# export data
write.table(tidyData, "./tidyData.txt",row.names=TRUE,sep="\t");

###############################################################################







