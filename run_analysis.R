#1
X <- rbind(read.table("./Dataset/train/X_train.txt"), read.table("./Dataset/test/X_test.txt"))
y <- rbind(read.table("./Dataset/train/y_train.txt"), read.table("./Dataset/test/y_test.txt"))
subject <- rbind(read.table("./Dataset/train/subject_train.txt"), read.table("./Dataset/test/subject_test.txt"))
#2
features <- read.table("./Dataset/features.txt")
X <- X[,grepl("mean\\(\\)",features[[2]]) | grepl("std\\(\\)",features[[2]])]
names(X) <- features[grepl("mean\\(\\)",features[[2]]) | grepl("std\\(\\)",features[[2]]),][[2]]
names(X) <- gsub("-","",names(X))
names(X) <- gsub("mean","Mean",names(X))
names(X) <- gsub("std","Std",names(X))
names(X) <- gsub("\\(\\)","",names(X))
#3
activity <- read.table("./Dataset/activity_labels.txt")
activity[[2]] <- tolower(gsub("_", "", activity[[2]]))
y[[1]] <- activity[y[[1]],2]
names(y) <- "activity"
#4
names(subject) <- "subject"
data <- cbind(X,subject,y)
write.table(data,file="tidydata.txt",sep=",")
#5
subjectsize <- length(table(subject))
activitysize <- length(activity[[2]])
columnsize <- length(X[1,])
allMean <- matrix(NA, nrow=subjectsize*activitysize, ncol=columnsize+1,dimnames=)
row <- 1
for(i in 1:subjectsize) {
    for(j in 1:activitysize) {
        rownames(allMean(row)) <- paste(i,activity[j,2],sep=".")
        allMean[row,1:columnsize] <- colMeans(data[data$subject==i & data$activity==activity[j,2],1:columnsize])
        row <- row+1
    }
}
names(allMean) <- features[grepl("mean\\(\\)",features[[2]]) | grepl("std\\(\\)",features[[2]]),2]
write.table(allMean,file="tidydata2.txt",sep=",")