library(tidyr)

#1.Merges the training and the test sets to create one data set.
train<-read.table("./UCI HAR Dataset/train/X_train.txt")
test<-read.table("./UCI HAR Dataset/test/X_test.txt")

train_st<-read.table("./UCI HAR Dataset/train/subject_train.txt")
train_y<-read.table("./UCI HAR Dataset/train/y_train.txt")

test_st<-read.table("./UCI HAR Dataset/test/subject_test.txt")
test_y<-read.table("./UCI HAR Dataset/test/y_test.txt")

train<-cbind(train,train_st)
train<-cbind(train,train_y)

test<-cbind(test,test_st)
test<-cbind(test,test_y)

dataset<-rbind(train,test)

column_names<-read.table("./UCI HAR Dataset/features.txt",stringsAsFactors = FALSE)
column_names<-column_names$V2
names(dataset)[562]="subject"
names(dataset)[563]="activity"

#2.Extracts only the measurements on the mean and standard deviation for each measurement.

#only std and mean values for resultant values are included, values for individual XYZ axes are omitted
std_dev_cols<-grep("std\\()$",column_names)
mean_cols<-grep("mean\\()$",column_names)
subject<-dataset$subject
activity_type<-dataset$activity
#bind everything together
extracted_dataset<-cbind(dataset[,std_dev_cols],dataset[,mean_cols])
extracted_dataset<-cbind(extracted_dataset, subject)
extracted_dataset<-cbind(extracted_dataset, activity_type)
#pass names of selected columns to the new dataset
names(extracted_dataset)[1:(length(std_dev_cols)+length(mean_cols))]=c(column_names[std_dev_cols],column_names[mean_cols])
names(extracted_dataset)[length(names(extracted_dataset))]

#3.Uses descriptive activity names to name the activities in the data set
#labels imported from file 
activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt",stringsAsFactors = FALSE)
activity_labels<-activity_labels$V2
#appropriate labels replace the numbers
extracted_dataset$activity_type<-activity_labels[extracted_dataset$activity_type]

#4.Appropriately labels the data set with descriptive variable names.
#already done, operated on names earlier for convenience, the dataset is extracted_dataset

#5.From the data set in step 4, creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject

#aggredating data, trimming redundant two columns. Some warnings caused by the two last column, but they are 
# harmless since the data is trimmed of these two columns anyway
data<-aggregate(extracted_dataset, by=list(extracted_dataset$activity_type,extracted_dataset$subject), FUN=mean)
names(data)[1]="activity_type"
names(data)[2]="subject"
data<-data[,1:20]

# decoding and separation of the measurment names components. The code would be so much prittier with %>% operator, 
# but it was prevented by need to manipulate the columns content in between the main operations  
data2<-gather(data, domain_detector_operation,mean, 3:20)
data3<-separate(data2,domain_detector_operation, c("domain_detector","operation"))
# "_" introduced for easy separation of the string parts
data3$domain_detector<-sub("^t","t_",data3$domain_detector)
data3$domain_detector<-sub("^f","f_",data3$domain_detector)
data4<-separate(data3,domain_detector, c("domain","detector"))
data4$domain<-sub("t","time",data4$domain)
data4$domain<-sub("f","frequency",data4$domain)

#writing the tidy file
write.table(data4,"tidy_data.txt",row.name = FALSE)