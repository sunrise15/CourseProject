
analysis <- function() {
	
	# merge the training and test data files together 	
	dat <- data.frame()
	x <- read.table("./UCI HAR Dataset/train/X_train.txt")
	dat <- rbind(dat,x)
	x <- read.table("./UCI HAR Dataset/test/X_test.txt")
	dat <- rbind(dat,x)
	numrowdat<-nrow(dat)

	# read activity files 	
	activity <- data.frame()
	x <- read.table("./UCI HAR Dataset/train/Y_train.txt")
	activity <- rbind(activity,x)
	x <- read.table("./UCI HAR Dataset/test/Y_test.txt")
	activity <- rbind(activity,x)
	
	#read feature table
	feature <- read.table("./UCI HAR Dataset/features.txt")
	numrowfeat <- nrow(feature)
	feature$V2 <- as.character(feature$V2)
	
	#create new feature and data files 
	newfeat <- data.frame(c1=numeric(0),c2=character(0))
	newdat<-data.frame(matrix(nrow=numrowdat,ncol=0))
	newact<-data.frame()

	for (i in 1:numrowfeat){
		
		# is mean() or std() in the feature table?
		
		texttosearch <- feature[i,2]		
		isthere <- grep("mean()", texttosearch)
		if (length(isthere) == 0){
			isthere <- grep("std()", texttosearch)
			}
		
		# if mean() or std() found, then add to new tables

		if (length(isthere) == 1) {
			
			#new feature table
			onef<- cbind(i,texttosearch)
			names(onef)<-names(newfeat)
			newfeat <- rbind(newfeat,onef)
			
    			# new data table
			oned <- dat[,i]
			newdat <- cbind(newdat,oned)
			numcol <- ncol(newdat)
			names(newdat)[numcol] <- texttosearch

			# new activity table
			onea<-activity[i,]
			newact <- rbind(newact,onea)
			}
	}
	
	#create descriptive activity names for the data set
	oned2<-data.frame(0)
	oned2<-data.frame(lapply(oned2,as.character),stringsAsFactors=FALSE)
	tidydf<-data.frame()
			
	for (j in 1:numcol){
		colhdr<- names(newdat)[j]
		
		c<-substr(colhdr,0,1)	
		if (c=="t"){
		c1<-"Time"
		}else if (c=="f") {
		c1<-"Frequency"
		}else {c1<-""}
				
		isgyro<-grep("Gyro",colhdr)
		isacc<-grep("Acc",colhdr)
		if (length(isgyro)==1){
		c2<-"Gyroscope"
		}else if (length(isacc)==1) {
		c2<-"Accelerometer"
		}else {c2<-""}

		ismean <- grep("mean()", colhdr)
		isstd<-grep("std()", colhdr)
		if (length(ismean) == 1){
		c3 <- "Mean"
		}else if (length(isstd)==1){
		c3<-"Standard Deviation"
		}else {c3<-"NA"}

		isx <- grep("-X", colhdr)
		isy<-grep("-Y", colhdr)
		isz<-grep("-Z", colhdr)
		if (length(isx) == 1){
		c4 <- "X"
		}else if (length(isy)==1){
		c4<-"Y"
		}else if (length(isz)==1){
		c4<-"Z"
		}else {c4<-"ALL"}

		c5<-colMeans(newdat[j])
						
		actnum<-newact[j,]
		if(actnum == 1){
		c6<-"WALKING"
		}else if(actnum==2){
		c6<-"WALKING UPSTAIRS"
		}else if (actnum==3){
		c6<- "WALKING DOWNSTAIRS"
		}else if (actnum==4){
		c6<- "SITTING"
		}else if (actnum==5){
		c6<- "STANDING"
		}else if (actnum==6){
		c6<- "LAYING"
		}else {c6<-""}
		
		#header row for newdat
		oned2<-cbind(oned2,c6)
		
		#add tidy data
		keytidy<-paste(c6,c3,c4,sep="")
		onetidy<-cbind(keytidy,c6,c3,c4,c5)
		tidydf<-rbind(tidydf,onetidy)
	}

	
	#create newdat2 = newdat data frame with one row of descriptive header on top
	numcold2<-ncol(oned2)
	newdat2<-oned2[c(2:numcold2)]	
	newdat2<-data.frame(lapply(newdat2,as.character),stringsAsFactors=FALSE)
	names(newdat2)<-names(newdat)	
	newdat2<-rbind(newdat2,newdat) 

	#add column header to tidy data
	row.names(tidydf)<-NULL
	colnames(tidydf) <- c("Key","Activity","Variable","Subject","Average")	

	#get average for each variable
	x<-tidydf
	x$Key<-as.vector(x$Key)
	x$Average<-as.numeric(as.character(x$Average))
	x1<-ddply(x,.(Key),summarize,Activity=Activity,Variable=Variable,Subject=Subject,Average=ave(Average,FUN=mean))
	x2<-unique(x1[,1:5])
	tidydf2<-x2[,c(2:5)]

	#return data
	return (tidydf2)
}

			