###Read in data
featuresRaw<-read.table("./features.txt", stringsAsFactors=FALSE, quote="", header=FALSE)
trainSubject<-read.table("./train/subject_train.txt", stringsAsFactors=FALSE, quote="", header=FALSE)
trainActivity<-read.table("./train/y_train.txt", stringsAsFactors=FALSE, quote="", header=FALSE)
xTrain<-read.table("./train/X_train.txt", stringsAsFactors=FALSE, quote="", header=FALSE)
testSubject<-read.table("./test/subject_test.txt", stringsAsFactors=FALSE, quote="", header=FALSE)
testActivity<-read.table("./test/y_test.txt", stringsAsFactors=FALSE, quote="", header=FALSE)
xTest<-read.table("./test/X_test.txt", stringsAsFactors=FALSE, quote="", header=FALSE)

##Part 1: Combine data into one data frame
trainDat<-cbind(trainSubject, trainActivity, xTrain)
testDat<-cbind(testSubject, testActivity, xTest)
mergedDat<-rbind(trainDat,testDat)
names(mergedDat)<-c("subjectID", "activity", featuresRaw[,2] )

###Part 2: Filter on columns which contain 'mean' or 'std'
colMatches <- grepl("mean|std", names(mergedDat))
filteredDat <- cbind(mergedDat[,1:2],mergedDat[,colMatches])
#we want to remove meanFreq as well
filteredDat<-filteredDat[!(names(filteredDat) %in% grep("meanFreq",names(filteredDat),value=TRUE))]

###Part 3: Replace activity IDs with activity descriptions from activity_labels.txt
filteredDat[,2]<-sub("1", "WALKING", filteredDat[,2])
filteredDat[,2]<-sub("2", "WALKING_UPSTAIRS", filteredDat[,2])
filteredDat[,2]<-sub("3", "WALKING_DOWNSTAIRS", filteredDat[,2])
filteredDat[,2]<-sub("4", "SITTING", filteredDat[,2])
filteredDat[,2]<-sub("5", "STANDING", filteredDat[,2])
filteredDat[,2]<-sub("6", "LAYING", filteredDat[,2])

###Part 4: Label the data set with descriptive names. This was already done in part 1.
###Rename column names with tidy-like easy to read labels.
names(filteredDat)<-sub("\\(\\)","",names(filteredDat))
names(filteredDat)<-gsub("-","",names(filteredDat))
names(filteredDat)<-sub("mean","Mean",names(filteredDat))
names(filteredDat)<-sub("std","Std",names(filteredDat))
names(filteredDat)<-sub("tB","timeB",names(filteredDat))
names(filteredDat)<-sub("tG","timeG",names(filteredDat))
names(filteredDat)<-sub("fB","freqB",names(filteredDat))
names(filteredDat)<-sub("BodyBody","Body",names(filteredDat))
names(filteredDat)<-gsub("Acc","Accel",names(filteredDat))
names(filteredDat)<-gsub("Mag","Magnitude",names(filteredDat))

###Part 5: Create a second tidy data set from the output from part 4. Include the mean of the columns for the observations
###for each subject and activity performed by that subject. Print out tidy data set using write.table() using row.name=FALSE.
library(reshape2)
filteredDat<-filteredDat[ order(filteredDat[,1], filteredDat[,2]), ]
vars<-names(filteredDat)
vars<-vars[-(1:2)]
datMelt<-melt(filteredDat,id=c("subjectID", "activity"), measure.vars=vars)
datCast<-dcast(datMelt, subjectID + activity ~ variable,mean)
write.table(datCast, file="smartphone_tidydataset.txt", row.names=FALSE, sep="\t")