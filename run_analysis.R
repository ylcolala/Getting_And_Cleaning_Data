

merge_table = function(test,train) {
  test <- read.table(test)
  train <- read.table(train)
  data <- rbind(test,train)
}



merge_tables = function() {
  
  test.x <- "./data/UCI HAR Dataset/test/X_test.txt"
  test.y <- "./data/UCI HAR Dataset/test/y_test.txt"
  test.subject <- "./data/UCI HAR Dataset/test/subject_test.txt"
  train.x <- "./data/UCI HAR Dataset/train/X_train.txt"
  train.y <- "./data/UCI HAR Dataset/train/y_train.txt"
  train.subject <- "./data/UCI HAR Dataset/train/subject_train.txt"
  
  x <- merge_table(test.x,train.x)
  y <- merge_table(test.y,train.y)
  subject <- merge_table(test.subject,train.subject)
  list(x=x,y=y,subject=subject)
}

extracted.mean_std = function(dataset) {
  features.path <- "./data/UCI HAR Dataset/features.txt"
  features <- read.table(features.path)
  means <- sapply(features[,2], function(x) grepl("mean", x, fixed=T))
  stds <- sapply(features[,2], function(x) grepl("std", x, fixed=T))
  data.cols <- means|stds
  dataset<-dataset[,data.cols]
  colnames(dataset)<-features[data.cols,2]
  dataset
}

name.activities=function(dataset){
  activities.path <- "./data/UCI HAR Dataset/activity_labels.txt"
  activities<- read.table(activities.path)
  dataset<-merge(dataset, activities, by= intersect(names(dataset), names(activities)))
  colnames(dataset)<-c("activity_id","activity_name")
  dataset
}
  
clean_data<-function(){


  tables<- merge_tables();
  
  extracted<-extracted.mean_std(tables$x);
  
  activities<-name.activities(tables$y);
  
  colnames(tables$subject)<-"subject_id"
  
  tidy_data<-cbind(tables$subject,activities,extracted )
  
  tidy_data.average<- ddply(tidy_data,.(activity_id,subject_id), fun.aggregate=mean)

  write.table(tidy_data.average, file="./data/tidy_data.txt", row.names = FALSE)
}
