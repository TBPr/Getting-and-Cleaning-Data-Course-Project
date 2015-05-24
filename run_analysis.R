library(dplyr)
# read input data frames
setwd("UCI HAR Dataset")
data_train_x<-read.table("./train/x_train.txt",header=FALSE)
data_train_y<-read.table("./train/y_train.txt",header=FALSE)
data_train_subject<-read.table("./train/subject_train.txt",header=FALSE)
data_test_x<-read.table("./test/x_test.txt",header=FALSE)
data_test_y<-read.table("./test/y_test.txt",header=FALSE)
data_test_subject<-read.table("./test/subject_test.txt",header=FALSE)

# names of columns
features<-read.table("./features.txt",header=FALSE,colClasses = "character")
names_data<-features[,1]
names(data_train_x)<-names_data
names(data_test_x)<-names_data

names(data_test_y)<-c("activity")
names(data_train_y)<-c("activity")

names(data_test_subject)<-c("subject")
names(data_train_subject)<-c("subject")

# merge with col's subject ant activity

data_test<-cbind(data_test_subject,data_test_y,data_test_x)
data_train<-cbind(data_train_subject,data_train_y,data_train_x)

# add cols with subset train/test

s_train<-data.frame(subset=rep("train",7352))
data_train<-cbind(s_train,data_train)
s_test<-data.frame(subset=rep("test",2947))
data_test<-cbind(s_test,data_test)

# Merges the training and thr test sets to create one data set

data_full<-rbind(data_train,data_test)

# define columns for extracts 

vars1<-c(1:9,44:49,84:89,124:129,164:169,204:205,217:218,230:231,243:244,256:257,269:274,348:353,427:432,506:507,519:520,532:533,545:546)
# Extracts only the measurements on the mean and standard deviation for each measurement
data_extract<-select(data_full,vars1)

#Uses descriptive activity names to name the activities in the data set
activity<-read.table("./activity_labels.txt",header=FALSE,colClasses = "character")
names(activity)<-c("activity","Activity")
Data_extract=merge(data_extract,activity,by.x="activity",by.y="activity",all=TRUE)

Data_extract<-select(Data_extract,-activity)

#Appropriately labels the data set with descriptive variable names. 
vars2<-c(4:9,44:49,84:89,124:129,164:169,204:205,217:218,230:231,243:244,256:257,269:274,348:353,427:432,506:507,519:520,532:533,545:546)-3
v_names<-c("Subset","Subject",features[vars2,2],"Activity")
names(Data_extract)<-v_names

# From the data set in step 4, creates a second, 
# independent tidy data set with the average of each variable for each activity and each subject.

Data2<-filter(Data_extract,Subject==1,Activity==activity[1,2])

num<-0
for (i in 1:30)
    {for (j in 1:6)
        {        
        table_temp<-filter(Data_extract,Subject==i,Activity==activity[j,2])
        num<-num+1
        Data2[num,1]<-table_temp[1,1]
        Data2[num,2]<-table_temp[1,2]
        Data2[num,69]<-table_temp[1,69]
        for (k in 3:68)
        {
           Data2[num,k]<-mean(table_temp[,k])
         
        }
    
    }
}
# Result file upload
write.table(Data2,file="Data_result.txt",row.name=FALSE)
