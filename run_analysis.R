##task 0 :reading data set
features <- read.table("features.txt", col.names = c("n","functions"))
activity<-read.table("activity_labels.txt",col.names = c("n","labels"))
x_test <- read.table("X_test.txt", col.names = features$functions)
y_test <- read.table("y_test.txt", col.names = "code")
subject_train <- read.table("subject_train.txt", col.names = "subject")
subject_test <- read.table("subject_test.txt", col.names = "subject")
x_train <- read.table("X_train.txt", col.names = features$functions)
y_train <- read.table("y_train.txt", col.names = "code")
##task 0:reading dataset completed



##task 1:Merges the training and the test sets to create one data set.
X<-rbind(x_train,x_test)
Y<-rbind(y_train,y_test)
Subject<-rbind(subject_train,subject_test)
Merged_Data <- cbind(Subject, Y, X)
##task 1:Merges the training and the test sets to create one data set. COMPLETED



##Task 2:Extracts only the measurements on the mean and standard deviation for each measurement.
##COLUMN CODE REFERS TO THE ACTIVITIES PERFORMED
a<-Merged_Data[,grep("(*mean*)|(*std*)",tolower(colnames(Merged_Data)))]
a<-cbind(Merged_Data$code,a)
colnames(a)[1]<-"code"  ##added column code and named it as column code
a<-cbind(Merged_Data$subject,a)
colnames(a)[1]<-"subject" ##added column subject and named it as column subject
##Task 2:Extracts only the measurements on the mean and standard deviation for each measurement.COMPLETED




##task 3:Uses descriptive activity names to name the activities in the data set
a$code<-as.factor(a$code)##converting column code from integer to factor
levels(a$code)<-activity$labels
##task 3:Uses descriptive activity names to name the activities in the data set. COMPLETED

##task 4:Appropriately labels the data set with descriptive variable names.
names(a)[2] = "activity"
names(a)<-gsub("Acc", "Accelerometer", names(a))
names(a)<-gsub("^t", "Time", names(a))
names(a)<-gsub("^f", "Frequency", names(a))

names(a)<-gsub("BodyBody", "Body", names(a))


##task 5:From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


tidy_data_mean<-tbl_df(a)%>%group_by(activity,subject)%>%summarize_all(mean)

##task 5:From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. COMPLETED


##saving files in csv form
write.table(a, "Tidydata.txt", row.name=FALSE)
write.table(tidy_data_mean, "tidy_data_mean.txt", row.name=FALSE)
##write.csv(a,"tidydata.csv",row.names = FALSE)
##write.csv(tidy_data_mean,"tidy_data_mean.csv",row.names = FALSE)