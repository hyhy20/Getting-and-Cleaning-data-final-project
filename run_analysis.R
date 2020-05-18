library(dplyr)
#Q1:Merges the training and the test sets to create one data set.
#Extract general files
file1 <- ("UCI HAR Dataset/features.txt")
features <- read.table(file1)
file2 <- ("UCI HAR Dataset/activity_labels.txt")
activity_label <- read.table(file2)
#Extract test files
test_file1 <- ("UCI HAR Dataset/test/y_test.txt")
test_y <- read.table(test_file1)
test_file2 <- ("UCI HAR Dataset/test/x_test.txt")
test_x <- read.table(test_file2)
test_file3 <- ("UCI HAR Dataset/test/subject_test.txt")
test_subject <- read.table(test_file3)
#Extract train files
tr_file1 <- ("UCI HAR Dataset/train/subject_train.txt")
tr_subject <- read.table(tr_file1)
tr_file2 <- ("UCI HAR Dataset/train/X_train.txt")
tr_x <- read.table(tr_file2)
tr_file3 <- ("UCI HAR Dataset/train/Y_train.txt")
tr_y <- read.table(tr_file3)


#Make test data dataframe 
test_data <- data.frame(test_subject,test_y,test_x)
#Make train data dataframe
train_data <- data.frame(tr_subject,tr_y,tr_x)
#Combine two together
data <- rbind(test_data,train_data)
colnames(data) <- c("subject","activity",features[,2])

#Q2:Extracts only the measurements on the mean and standard deviation.
tolower(names(data))
mean <- data[,grep("mean", names(data))]
std <- data[,grep("std",names(data))]
final_data <- cbind(data[,1:2], mean, std)

#Q3:Uses descriptive activity names
final_data$activity <- factor(
    final_data$activity,levels=1:6,labels = activity_label$V2)
final_data <- final_data[order(final_data$subject,final_data$activity),]

#Q4:Appropriately labels the data set with descriptive variable names.
names(final_data) <- gsub("\\()","",names(final_data))
names(final_data) <- gsub("^t","Time:",names(final_data))
names(final_data) <- gsub("^f","Frequence:",names(final_data))
names(final_data) <- gsub("-mean","'s mean",names(final_data))
names(final_data) <- gsub("-std","'s standard deviation",names(final_data))

#Q5:Creates a second, independent tidy data set with the average of 
#   each variable for each activity and each subject.
output<-final_data %>% 
    group_by(subject, activity) %>% summarise_each(funs(mean))
write.table(output,"output.txt", row.names = FALSE)