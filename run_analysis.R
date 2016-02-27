
#Merges the training and the test sets to create one data set.
#REading Test Dataset

#FOR TEST DATA
#Reading data

#Reading activity_lables
  activity_labels <- read.table("~/Documents/Coursera/Getting and Cleaning Data/Week3/UCI HAR Dataset/activity_labels.txt", quote="\"", comment.char="", stringsAsFactors=FALSE)

#REading colnames
  features <- read.table("~/Documents/Coursera/Getting and Cleaning Data/Week3/UCI HAR Dataset/features.txt", quote="\"", comment.char="", stringsAsFactors=FALSE)
  X_test <- read.table("~/Documents/Coursera/Getting and Cleaning Data/Week3/UCI HAR Dataset/test/X_test.txt", quote="\"", comment.char="", stringsAsFactors=FALSE)
  y_test <- read.table("~/Documents/Coursera/Getting and Cleaning Data/Week3/UCI HAR Dataset/test/y_test.txt", quote="\"", comment.char="", stringsAsFactors=FALSE)
  subject_test <- read.table("~/Documents/Coursera/Getting and Cleaning Data/Week3/UCI HAR Dataset/test/subject_test.txt", quote="\"", comment.char="", stringsAsFactors=FALSE)

#Rename Columns in X_test:
  old_names<-names(X_test)
  new_names<-features[,"V2"]

#Change the names
  library(data.table)
  old_names<-names(X_test)
  new_names<-features[,"V2"]
  setnames(X_test, old=old_names, new=new_names)
  
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#Subsetting mean and std columns 
test <- grep(".*mean|.*std",new_names)
X_test_mean_std<-X_test[,new_names[test]]


#Uses descriptive activity names to name the activities in the data set
y_test_lable<-merge(y_test,activity_labels,by.x="V1",by.y="V1",all.x=T)

X_test_merge<-cbind(X_test_mean_std,subject=subject_test,y=y_test_lable[,"V2"])

##################################X_train##############3
X_train <- read.table("~/Documents/Coursera/Getting and Cleaning Data/Week3/UCI HAR Dataset/train/X_train.txt", quote="\"", comment.char="", stringsAsFactors=FALSE)
y_train <- read.table("~/Documents/Coursera/Getting and Cleaning Data/Week3/UCI HAR Dataset/train/y_train.txt", quote="\"", comment.char="", stringsAsFactors=FALSE)
subject_train <- read.table("~/Documents/Coursera/Getting and Cleaning Data/Week3/UCI HAR Dataset/train/subject_train.txt", quote="\"", comment.char="", stringsAsFactors=FALSE)

#Rename Columns in X_train:
old_names<-names(X_train)
new_names<-features[,"V2"]

#Change the names
library(data.table)
old_names<-names(X_train)
new_names<-features[,"V2"]
setnames(X_train, old=old_names, new=new_names)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#Subsetting mean and std columns 
train <- grep(".*mean|.*std",new_names)
X_train_mean_std<-X_train[,new_names[train]]


#Uses descriptive activity names to name the activities in the data set
y_train_lable<-merge(y_train,activity_labels,by.x="V1",by.y="V1",all.x=T)

X_train_merge<-cbind(X_train_mean_std,subject=subject_train,y=y_train_lable[,"V2"])

################################MERGE

#Merging Datasets:Train And Test
X_all<-merge(X_train_merge,X_test_merge,all=T)
library(plyr)
X_all <- rename(X_all, replace=c("V1" = "subject"))
X_all <- rename(X_all, replace=c("y" = "activities"))


#Change the names of columns
old_names1<-names(X_all)
new_names1<-sub("^f", "frequency", old_names1)
new_names2<-sub("^t", "time", new_names1)
new_names3<-sub("-", "", new_names2)
new_names4<-sub("-", "", new_names3)
new_names5<-sub("[()]", "", new_names4)
new_names6<-sub("[)]", "", new_names5)
new_names6

setnames(X_all, old=old_names1, new=new_names6)
names(X_all)
library(dplyr)

#Average For Every Var
group_data<-group_by(X_all,subject,activities)
data_r<-summarize(group_data
                  ,AV_timeimeBodyAccmeanX=mean(timeimeBodyAccmeanX,na.rm=T)
                  ,AV_timeimeBodyAccmeanY=mean(timeimeBodyAccmeanY,na.rm=T)
                  ,AV_timeimeBodyAccmeanZ=mean(timeimeBodyAccmeanZ,na.rm=T)
                  ,AV_timeimeBodyAccstdX=mean(timeimeBodyAccstdX,na.rm=T)
                  ,AV_timeimeBodyAccstdY=mean(timeimeBodyAccstdY,na.rm=T)
                  ,AV_timeimeBodyAccstdZ=mean(timeimeBodyAccstdZ,na.rm=T)
                  ,AV_timeimeGravityAccmeanX=mean(timeimeGravityAccmeanX,na.rm=T)
                  ,AV_timeimeGravityAccmeanY=mean(timeimeGravityAccmeanY,na.rm=T)
                  ,AV_timeimeGravityAccmeanZ=mean(timeimeGravityAccmeanZ,na.rm=T)
                  ,AV_timeimeGravityAccstdX=mean(timeimeGravityAccstdX,na.rm=T)
                  ,AV_timeimeGravityAccstdY=mean(timeimeGravityAccstdY,na.rm=T)
                  ,AV_timeimeGravityAccstdZ=mean(timeimeGravityAccstdZ,na.rm=T)
                  ,AV_timeimeBodyAccJerkmeanX=mean(timeimeBodyAccJerkmeanX,na.rm=T)
                  ,AV_timeimeBodyAccJerkmeanY=mean(timeimeBodyAccJerkmeanY,na.rm=T)
                  ,AV_timeimeBodyAccJerkmeanZ=mean(timeimeBodyAccJerkmeanZ,na.rm=T)
                  ,AV_timeimeBodyAccJerkstdX=mean(timeimeBodyAccJerkstdX,na.rm=T)
                  ,AV_timeimeBodyAccJerkstdY=mean(timeimeBodyAccJerkstdY,na.rm=T)
                  ,AV_timeimeBodyAccJerkstdZ=mean(timeimeBodyAccJerkstdZ,na.rm=T)
                  ,AV_timeimeBodyGyromeanX=mean(timeimeBodyGyromeanX,na.rm=T)
                  ,AV_timeimeBodyGyromeanY=mean(timeimeBodyGyromeanY,na.rm=T)
                  ,AV_timeimeBodyGyromeanZ=mean(timeimeBodyGyromeanZ,na.rm=T)
                  ,AV_timeimeBodyGyrostdX=mean(timeimeBodyGyrostdX,na.rm=T)
                  ,AV_timeimeBodyGyrostdY=mean(timeimeBodyGyrostdY,na.rm=T)
                  ,AV_timeimeBodyGyrostdZ=mean(timeimeBodyGyrostdZ,na.rm=T)
                  ,AV_timeimeBodyGyroJerkmeanX=mean(timeimeBodyGyroJerkmeanX,na.rm=T)
                  ,AV_timeimeBodyGyroJerkmeanY=mean(timeimeBodyGyroJerkmeanY,na.rm=T)
                  ,AV_timeimeBodyGyroJerkmeanZ=mean(timeimeBodyGyroJerkmeanZ,na.rm=T)
                  ,AV_timeimeBodyGyroJerkstdX=mean(timeimeBodyGyroJerkstdX,na.rm=T)
                  ,AV_timeimeBodyGyroJerkstdY=mean(timeimeBodyGyroJerkstdY,na.rm=T)
                  ,AV_timeimeBodyGyroJerkstdZ=mean(timeimeBodyGyroJerkstdZ,na.rm=T)
                  ,AV_timeimeBodyAccMagmean=mean(timeimeBodyAccMagmean,na.rm=T)
                  ,AV_timeimeBodyAccMagstd=mean(timeimeBodyAccMagstd,na.rm=T)
                  ,AV_timeimeGravityAccMagmean=mean(timeimeGravityAccMagmean,na.rm=T)
                  ,AV_timeimeGravityAccMagstd=mean(timeimeGravityAccMagstd,na.rm=T)
                  ,AV_timeimeBodyAccJerkMagmean=mean(timeimeBodyAccJerkMagmean,na.rm=T)
                  ,AV_timeimeBodyAccJerkMagstd=mean(timeimeBodyAccJerkMagstd,na.rm=T)
                  ,AV_timeimeBodyGyroMagmean=mean(timeimeBodyGyroMagmean,na.rm=T)
                  ,AV_timeimeBodyGyroMagstd=mean(timeimeBodyGyroMagstd,na.rm=T)
                  ,AV_timeimeBodyGyroJerkMagmean=mean(timeimeBodyGyroJerkMagmean,na.rm=T)
                  ,AV_timeimeBodyGyroJerkMagstd=mean(timeimeBodyGyroJerkMagstd,na.rm=T)
                  ,AV_frequencyrequencyBodyAccmeanX=mean(frequencyrequencyBodyAccmeanX,na.rm=T)
                  ,AV_frequencyrequencyBodyAccmeanY=mean(frequencyrequencyBodyAccmeanY,na.rm=T)
                  ,AV_frequencyrequencyBodyAccmeanZ=mean(frequencyrequencyBodyAccmeanZ,na.rm=T)
                  ,AV_frequencyrequencyBodyAccstdX=mean(frequencyrequencyBodyAccstdX,na.rm=T)
                  ,AV_frequencyrequencyBodyAccstdY=mean(frequencyrequencyBodyAccstdY,na.rm=T)
                  ,AV_frequencyrequencyBodyAccstdZ=mean(frequencyrequencyBodyAccstdZ,na.rm=T)
                  ,AV_frequencyrequencyBodyAccmeanFreqX=mean(frequencyrequencyBodyAccmeanFreqX,na.rm=T)
                  ,AV_frequencyrequencyBodyAccmeanFreqY=mean(frequencyrequencyBodyAccmeanFreqY,na.rm=T)
                  ,AV_frequencyrequencyBodyAccmeanFreqZ=mean(frequencyrequencyBodyAccmeanFreqZ,na.rm=T)
                  ,AV_frequencyrequencyBodyAccJerkmeanX=mean(frequencyrequencyBodyAccJerkmeanX,na.rm=T)
                  ,AV_frequencyrequencyBodyAccJerkmeanY=mean(frequencyrequencyBodyAccJerkmeanY,na.rm=T)
                  ,AV_frequencyrequencyBodyAccJerkmeanZ=mean(frequencyrequencyBodyAccJerkmeanZ,na.rm=T)
                  ,AV_frequencyrequencyBodyAccJerkstdX=mean(frequencyrequencyBodyAccJerkstdX,na.rm=T)
                  ,AV_frequencyrequencyBodyAccJerkstdY=mean(frequencyrequencyBodyAccJerkstdY,na.rm=T)
                  ,AV_frequencyrequencyBodyAccJerkstdZ=mean(frequencyrequencyBodyAccJerkstdZ,na.rm=T)
                  ,AV_frequencyrequencyBodyAccJerkmeanFreqX=mean(frequencyrequencyBodyAccJerkmeanFreqX,na.rm=T)
                  ,AV_frequencyrequencyBodyAccJerkmeanFreqY=mean(frequencyrequencyBodyAccJerkmeanFreqY,na.rm=T)
                  ,AV_frequencyrequencyBodyAccJerkmeanFreqZ=mean(frequencyrequencyBodyAccJerkmeanFreqZ,na.rm=T)
                  ,AV_frequencyrequencyBodyGyromeanX=mean(frequencyrequencyBodyGyromeanX,na.rm=T)
                  ,AV_frequencyrequencyBodyGyromeanY=mean(frequencyrequencyBodyGyromeanY,na.rm=T)
                  ,AV_frequencyrequencyBodyGyromeanZ=mean(frequencyrequencyBodyGyromeanZ,na.rm=T)
                  ,AV_frequencyrequencyBodyGyrostdX=mean(frequencyrequencyBodyGyrostdX,na.rm=T)
                  ,AV_frequencyrequencyBodyGyrostdY=mean(frequencyrequencyBodyGyrostdY,na.rm=T)
                  ,AV_frequencyrequencyBodyGyrostdZ=mean(frequencyrequencyBodyGyrostdZ,na.rm=T)
                  ,AV_frequencyrequencyBodyGyromeanFreqX=mean(frequencyrequencyBodyGyromeanFreqX,na.rm=T)
                  ,AV_frequencyrequencyBodyGyromeanFreqY=mean(frequencyrequencyBodyGyromeanFreqY,na.rm=T)
                  ,AV_frequencyrequencyBodyGyromeanFreqZ=mean(frequencyrequencyBodyGyromeanFreqZ,na.rm=T)
                  ,AV_frequencyrequencyBodyAccMagmean=mean(frequencyrequencyBodyAccMagmean,na.rm=T)
                  ,AV_frequencyrequencyBodyAccMagstd=mean(frequencyrequencyBodyAccMagstd,na.rm=T)
                  ,AV_frequencyrequencyBodyAccMagmeanFreq=mean(frequencyrequencyBodyAccMagmeanFreq,na.rm=T)
                  ,AV_frequencyrequencyBodyBodyAccJerkMagmean=mean(frequencyrequencyBodyBodyAccJerkMagmean,na.rm=T)
                  ,AV_frequencyrequencyBodyBodyAccJerkMagstd=mean(frequencyrequencyBodyBodyAccJerkMagstd,na.rm=T)
                  ,AV_frequencyrequencyBodyBodyAccJerkMagmeanFreq=mean(frequencyrequencyBodyBodyAccJerkMagmeanFreq,na.rm=T)
                  ,AV_frequencyrequencyBodyBodyGyroMagmean=mean(frequencyrequencyBodyBodyGyroMagmean,na.rm=T)
                  ,AV_frequencyrequencyBodyBodyGyroMagstd=mean(frequencyrequencyBodyBodyGyroMagstd,na.rm=T)
                  ,AV_frequencyrequencyBodyBodyGyroMagmeanFreq=mean(frequencyrequencyBodyBodyGyroMagmeanFreq,na.rm=T)
                  ,AV_frequencyrequencyBodyBodyGyroJerkMagmean=mean(frequencyrequencyBodyBodyGyroJerkMagmean,na.rm=T)
                  ,AV_frequencyrequencyBodyBodyGyroJerkMagstd=mean(frequencyrequencyBodyBodyGyroJerkMagstd,na.rm=T)
                  ,AV_frequencyrequencyBodyBodyGyroJerkMagmeanFreq=mean(frequencyrequencyBodyBodyGyroJerkMagmeanFreq,na.rm=T)
                  
    
    )
data_r



