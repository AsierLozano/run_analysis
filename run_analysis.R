#Libraries
library(data.table)
library(dplyr)

#Load data
features<-read.table("features.txt")
activity_labels<-read.table("activity_labels.txt")

# Load the training data set and the test data set
subject_train<-read.table("train/subject_train.txt")
y_train<-read.table("train/y_train.txt")
x_train<-read.table("train/X_train.txt")

subject_test<-read.table("test/subject_test.txt")
y_test<-read.table("test/y_test.txt")
x_test<-read.table("test/X_test.txt")

# 1.Merge the training and the test sets to create one data set 
subject<-rbind(subject_train,subject_test)
Y<-rbind(y_train,y_test)
X<-rbind(x_train,x_test)

colnames(X)<-features[,2]
colnames(Y) <- "Activity"
colnames(subject) <- "Subject"
data<-cbind(X,Y,subject)

#2.Extract only the measurements on the mean and standard deviation 
#for each measurement.
subset<-grep("-mean|-std",colnames(data))
columns<-c(subset,562,563)
mean_std<-data[,columns]
dim(mean_std)
    
#3.Use descriptive activity names to name the activities in the data set
mean_std$Activity<-as.character(mean_std$Activity)
for(i in 1:6) {mean_std$Activity[mean_std$Activity==i]<-as.character(activity_labels[,2])}
mean_std$Activity<-as.factor(mean_std$Activity)

#4.Appropriately labels the data set with descriptive variable names.
names(mean_std)<-gsub("Acc", "Accelerometer", names(mean_std))
names(mean_std)<-gsub("Gyro", "Gyroscope", names(mean_std))
names(mean_std)<-gsub("BodyBody", "Body", names(mean_std))
names(mean_std)<-gsub("Mag", "Magnitude", names(mean_std))
names(mean_std)<-gsub("^t", "Time", names(mean_std))
names(mean_std)<-gsub("^f", "Frequency", names(mean_std))
names(mean_std)<-gsub("tBody", "TimeBody", names(mean_std))
names(mean_std)<-gsub("-mean()", "Mean", names(mean_std), ignore.case = TRUE)
names(mean_std)<-gsub("-std()", "STD", names(mean_std), ignore.case = TRUE)
names(mean_std)<-gsub("-freq()", "Frequency", names(mean_std), ignore.case = TRUE)
names(mean_std)<-gsub("angle", "Angle", names(mean_std))
names(mean_std)<-gsub("gravity", "Gravity", names(mean_std))

#5.From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.
mean_std$Subject<-as.factor(mean_std$Subject)
mean_std<-data.table(mean_std)

mean<-aggregate(. ~Subject+Activity,mean_std,mean) 
mean<-mean[order(mean$Subject,mean$Activity),]
write.table(x=mean,file="average.txt",row.names=FALSE)
