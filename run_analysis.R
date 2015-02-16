##################################################################
##################################################################
# This function does analysis on the data gathered from various 
# experiments that monitor sensor signals based on people's activities.
##################################################################
##################################################################

analysis <- function() {
	
	###########
	# Read files
	###########

	# read measurement files 	
	measure <- data.frame()
	x <- read.table("./UCI HAR Dataset/train/X_train.txt")
	measure <- rbind(measure,x)
	x <- read.table("./UCI HAR Dataset/test/X_test.txt")
	measure <- rbind(measure,x)
	numrowmeas<-nrow(measure)

	# read activity files 	
	activity <- data.frame()
	x <- read.table("./UCI HAR Dataset/train/Y_train.txt")
	activity <- rbind(activity,x)
	x <- read.table("./UCI HAR Dataset/test/Y_test.txt")
	activity <- rbind(activity,x)
	numrowactivity <- nrow(activity)
	
	# read subject files 	
	subject <- data.frame()
	x <- read.table("./UCI HAR Dataset/train/subject_train.txt")
	subject <- rbind(subject,x)
	x <- read.table("./UCI HAR Dataset/test/subject_test.txt")
	subject <- rbind(subject,x)
	numrowsubject <- nrow(subject)

	#read feature table
	feature <- read.table("./UCI HAR Dataset/features.txt")
	numrowfeat <- nrow(feature)
	feature$V2 <- as.character(feature$V2)
	
	#read activity labels table
	labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
	labels$V2 <- as.character(labels$V2)
	labels$V2 <- tolower(labels$V2)
	
	###########
	# Create data file
	###########

	#create data file column subject
	newdat<-subject
	names(newdat)[1] <- "Subject"

	#add data file column activity
	newact<-data.frame()
	
	for (i in 1:numrowactivity){
		actnum <- activity[i,]
		label1<-labels[which(labels[,"V1"] == actnum),]
		text <- label1[1,2]
		newact<-rbind(newact,text)
		newact[,1]<-as.character(newact[,1])
	}
	newdat <- cbind(newdat,newact)
	names(newdat)[2] <- "Activity"

	# add data file columns for measurements
	# only pick the mean and standard deviation columns

	for (i in 1:numrowfeat){
	
		texttosearch <- feature[i,2]		
		ismean <- grep("mean\\(\\)", texttosearch)
		isstd<-grep("std\\(\\)", texttosearch)
		
		# if mean or std found, then add to new tables
		if ((length(ismean) == 1) | (length(isstd) == 1 )){
						
    			# new data table
			oned <- measure[,i]
			newdat <- cbind(newdat,oned)
			numcol <- ncol(newdat)
			hdr<-gsub("\\(\\)","",texttosearch)
			hdr<-gsub("-",".",hdr)
			names(newdat)[numcol] <- hdr

		}
	}
	
	###########
	# Create tidy data 
	# uses package reshape2
	###########

	x <- newdat[order(newdat$Subject,newdat$Activity),]
	xmelt <- melt(x,id=c("Subject","Activity"),measure.vars=c(3:numcol))
	tidydata <- dcast(xmelt,Subject+Activity~variable,mean)
			
	###########
	# Return data
	###########

	return (tidydata)
}

			