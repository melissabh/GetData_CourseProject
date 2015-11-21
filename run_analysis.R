
# NOTE: function assumes that the './UCI HAR Dataset' data directory is 
# available relative to the current working directory and will also write
# a file, run_analysist.txt out to your current working directory
run_analysis <- function() {
    
    datadir = "./UCI HAR Dataset"

    # collect the test and training datasets into a list of data frames 
    # so we can combine them later into a single data frame
    dfs = list()
    dataTypes = c("test","train")
    
    for (i in 1:2) {
        
        dataType = dataTypes[i]
        
        # files needed are named like X_{dataType}.txt and live in a
        # subdirectory called {dataType} off the main data directory
        
        # this file has a 561-column row per each subject-activity observation
        valuesFile   = paste("X_",dataType,".txt",sep="")
        
        # this file has a single column which represents the numeric activity code
        # for the observation corresponding to the same numbered row in the values file
        labelsFile   = paste("y_",dataType,".txt",sep="") 
        
        # this file has a single column which represents the subject
        # for the observation corresponding to the same numbered row in the values files
        subjectsFile = paste("subject_",dataType,".txt",sep="")
    
        # get the data values, the activity labels column, the subjects column
        df          <- read.csv(file=paste(datadir,dataType,valuesFile,sep="/"),   header=FALSE, sep="")
        df.labels   <- read.csv(file=paste(datadir,dataType,labelsFile,sep="/"),   header=FALSE, sep="")
        df.subjects <- read.csv(file=paste(datadir,dataType,subjectsFile,sep="/"), header=FALSE, sep="")
        
        # concatenate the data frames column-wise to get a resulting dataframe
        # with columns: subject, activity, and data values columns
        df <- cbind(df.subjects, df.labels, df)

        # collect our dataframes in list
        dfs[[i]] <- df
    }
    
    # merge the training and test datasets
    df <- rbind(dfs[[1]],dfs[[2]])
    
    # put column headers on our merged sets dataframe. get the variable names of the 
    # features that were measured in the dataset. the features file will give us a (header-less)
    # dataframe with columns: FeatureNumber, FeatureName
    df.features <- read.csv(file=paste(datadir,"features.txt",sep="/"), header=FALSE, sep="")    
    names(df) <- c("subject","activity",as.character(df.features[,2]))
    
    # filter columns to include only those features referring to mean() and std()
    # measurements. (but still want to keep the subject and activity columns, hence
    # the leading 1 and 2 indexes we're pre-prending to the list)
    colFilter = c(1,2,grep('-mean\\(\\)|-std\\(\\)',names(df)))
    df <- df[,colFilter]
    
    # replace the activity codes with names. get the activity code => activity name 
    # mapping from the labels file. 
    df.label.map <- read.csv(file=paste(datadir,"activity_labels.txt",sep="/"), header=FALSE, sep="")
    names(df.label.map) <- c("ActivityCode","ActivityName")    
    df$activity <- factor(df$activity, levels=df.label.map$ActivityCode, labels=df.label.map$ActivityName)

    # clean up the variable names a bit to make more readable (kinda subjective what
    # you think makes nice and readable column names, but I think this change makes
    # things a little easier on the eyes)
    cols <- names(df)
    cols2 <- gsub(pattern='mean\\(\\)',replacement="Mean",x=cols)
    cols2 <- gsub(pattern='std\\(\\)',replacement="StdDev",x=cols2)
    names(df) <- cols2
    
    # group by subject, activity and calculate the mean over the groups for the measurements
    n <- ncol(df)
    df.summary <- aggregate( df[,3:n], by=list(df$subject,df$activity), FUN=mean, na.rm=TRUE)
    names(df.summary)[1:2] <- c("subject","activity") # put the nice column names back on because the aggregate leaves the grouping columns named as Group.1, Group.2, etc...
    
    # write the summarized df out to file - writes to the current working directory!
    write.table(df.summary, file=paste('./',"run_analysis_output.txt",sep=""), sep=" ", row.names = FALSE)
}